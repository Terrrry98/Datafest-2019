library(sqldf)
library(tidyverse)
library(robustHD)
library(leaps)

gps_dat = read.csv("D:/UW/DataFest 2019/Main Files/Datadest 2019/DataFestFiles/gps.csv", header = T)
gps_dat = gps_dat %>% filter(PlayerID > 21 | PlayerID < 18)

game_dat = read.csv("D:/UW/DataFest 2019/Main Files/Datadest 2019/DataFestFiles/games.csv", header = T)
rpe_dat = read.csv("D:/UW/DataFest 2019/Main Files/Datadest 2019/DataFestFiles/rpe.csv", header = T)
wellness_dat = read.csv("D:/UW/DataFest 2019/Main Files/Datadest 2019/DataFestFiles/wellness.csv", header = T)

#   Standardize numerical values in rpe data and wellness data for each PlayerID
#   RPE in rpe
#   Fatigue, Soreness, Desire, Irritability, SleepHours, SleepQuality, MonitoringScore in wellness
rpe_std = (rpe_dat[, c("Date", "PlayerID", "RPE", "Duration")] %>% 
               drop_na() %>% 
               group_by("PlayerID") %>%
               mutate_each(funs = standardize, RPE))[, 1:4]

wellness_std_num = (wellness_dat[, c(1, 2, 3, 4, 5, 6, 10, 11)] %>%
                        group_by("PlayerID") %>% 
                        mutate_each(funs = standardize, c(Fatigue, Soreness, Desire, Irritability, SleepQuality, MonitoringScore)))[, -9]

#   Standardize factor-numeric values in wellness data for each PlayerID
#   Pain, Illness, Menstruation, Nutrition, NutritionAdjustment, TrainingReadiness

training_value_map = structure(seq(0, 1, 0.05), names=levels(wellness_dat$TrainingReadiness))
menstrution_value_map = structure(c(0, 1), names=levels(wellness_dat$Menstruation))
nutrition_value_map = structure(3:1, names=levels(wellness_dat$Nutrition))
nutrition_adjustment_value_map = structure(c(0, -1, 1), names=levels(wellness_dat$NutritionAdjustment))

#   Extract standardized fatigue value for each player one day after training/non-training date.
fatigue_date = inner_join(wellness_std_num[, c("Date", "PlayerID", "Fatigue", "MonitoringScore")], 
                          rpe_dat[, c("Date", "PlayerID", "Training")], by = c("Date", "PlayerID")) %>%
    distinct()

#   Training
player_training_date = fatigue_date[, c(1, 2, 5)] %>% filter(Training == "Yes")
player_training_date$Date = as.character(as.Date(player_training_date$Date) + 1)

game_fatigue = inner_join(fatigue_date, player_training_date, by = c("Date", "PlayerID"))

#   None-training
player_not_training_date = fatigue_date[, c(1, 2, 5)] %>% filter(Training == "No")
player_not_training_date$Date = as.character(as.Date(player_not_training_date$Date) + 1)

none_game_fatigue = inner_join(fatigue_date, player_not_training_date, by = c("Date", "PlayerID"))


#   joint_table = inner_join(rpe_std, wellness_std_num, by = c("Date", "PlayerID"))
#   daily_workload = sqldf("SELECT RPE*Duration AS Workload, PlayerID, Date FROM joint_table as t")
#   daily_workload_val = sqldf("SELECT sum(Workload) AS Dailyworkload, PlayerID, Date
#                              FROM daily_workload as t GROUP BY PlayerID, Date")

#   daily_std_workload_fatigue = sqldf("SELECT d.Dailyworkload, d.PlayerID, d.Date, n.Fatigue
#                                      FROM daily_workload_std AS d, wellness_norm_num AS n
#                                      WHERE d.Date == n.Date AND d.PlayerID == n.PlayerID")

rpe_dat$Date = as.Date(rpe_dat$Date)
player_game_date = inner_join(rpe_dat[, c(1, 2, 4)], data.frame("Date" = as.Date(game_dat$Date)), by = "Date") %>% 
    distinct() %>%
    filter(SessionType == "Game")

player_game_date$Date = as.Date(player_game_date$Date)
rpe_std$Date = as.Date(rpe_std$Date)
game_dat$Date = as.Date(game_dat$Date)
wellness_std_num$Date = as.Date(wellness_std_num$Date)

player_pregame_date = player_game_date[rep(row.names(player_game_date), each = 5), ]
rownames(player_pregame_date) = seq(1, length(player_pregame_date$Date))
player_pregame_date$Date = player_pregame_date$Date - seq(1, 5)

player_output = inner_join(player_game_date, game_dat[, c("Date", "TeamPoints")], by = "Date")
player_output = (player_output[, -3] %>%
                     group_by(Date, PlayerID) %>%
                     summarise_each(TeamPoints, funs = mean))[, 1:3]

player_pregame_stats = left_join(player_pregame_date, wellness_std_num, by = c("Date", "PlayerID"))

write.csv(player_pregame_stats, "pregame_stats.csv")

player_pregame_stats_flatten = player_pregame_stats[, c(1, 2, 5, 6, 7, 8)]
player_pregame_stats_flatten$Date = player_pregame_stats_flatten$Date + seq(1, 5)

flatten_pregame_stats$Date = player_game_date$Date
game_stats_output = inner_join(player_output, flatten_pregame_stats, by = c("Date", "PlayerID"))

regfwd = regsubsets(TeamPoints ~ ., data = game_stats_output[, c(-1, -2)], method = "forward", na.action = na.exclude)
regbwd = regsubsets(TeamPoints ~ ., data = game_stats_output[, c(-1, -2)], method = "backward")

avg.distance = gps_dat[, c("GameID", "PlayerID", "Speed")] %>%
    group_by(GameID, PlayerID) %>%
    summarise_each(Speed, funs = sum)
avg.distance$Distance = avg.distance$Speed * 0.1

distance_player_date = (inner_join(avg.distance, game_dat[, c(1, 2)], by = "GameID"))[, c("PlayerID", "Date", "Distance")] %>%
    group_by(PlayerID, Date) %>%
    summarise_each(Distance, funs = sum) %>%
    distinct()

game_distance_prestats = inner_join(distance_player_date, game_stats_output, by = c("Date", "PlayerID"))
regfwd = regsubsets(Distance ~ ., data = game_distance_prestats[, c(-1, -2, -4)], method = "forward", na.action = na.omit)
regbwd = regsubsets(Distance ~ ., data = game_distance_prestats[, c(-1, -2, -4)], method = "backward", na.action = na.omit)

player_pregame_stats_avg = player_pregame_stats %>%
    group_by(Date, PlayerID) %>%
    summarise_each(c(Soreness, Desire, Irritability, SleepQuality), funs = mean)


new_well = wellness_std_num
new_well$Date = new_well$Date + 1

game_distance_avg_prestats = inner_join(distance_player_date, player_pregame_stats_avg, by = c("Date", "PlayerID"))
lm.dst.stats = lm(Distance ~ ., data = game_distance_avg_prestats[, c(-1, -2)], na.action = na.omit)

dwl.wellness = inner_join(daily_work_load, new_well, by = c("Date", "PlayerID"))

lm.dwl.ms = lm(MonitoringScore ~ Dailyworkload, data = dwl.wellness)


fatigue.game = data.frame(rbind(game_fatigue[, -5], none_game_fatigue[, -5]))
colnames(fatigue.game)[5] = "Training"
ggplot(data = fatigue.game, mapping = aes(x = as.factor(PlayerID), y = Fatigue, fill = Training)) +
    geom_boxplot() + 
    ylab("Standardized fatigue for each player") + 
    xlab("Player ID")

