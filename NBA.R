# ================================
# NBA Project - R Script Version
# Author: Brady Biehn
# ================================

# Load packages
library(tidyverse)
library(mosaic)
library(ggfortify)

# -------------------------------
# Import data
# -------------------------------
teamDefenseStats <- read_csv("team_stats_defense_rs.csv")
teamAdvancedStats <- read_csv("team_stats_advanced_rs.csv")
teamTraditionalStats <- read_csv("team_stats_traditional_rs.csv")

# -------------------------------
# Join datasets
# -------------------------------
fullJoinedData <- teamDefenseStats %>%
  select(c(1:15, 17:19, 21, 24, 26, 27, 55)) %>%
  left_join(
    teamAdvancedStats %>% select(-c(2:7, 12, 16:18, 21:45)),
    by = c("TEAM_ID", "SEASON")
  ) %>%
  select(1, SEASON, everything()) %>%
  arrange(TEAM_NAME)

fullJoinedData <- fullJoinedData %>%
  left_join(
    teamTraditionalStats %>% select(-c(2:7, 28:54)),
    by = c("TEAM_ID", "SEASON")
  )

filteredJoinedData <- fullJoinedData %>%
  mutate(FG2A = (FGA - FG3A)) %>%
  mutate(OPP_FG2A = (OPP_FGA - OPP_FG3A)) %>%
  mutate(W_PCT = W_PCT * 100) %>%
  select(-c(1, 4:6, 8:9, 11:12, 14:15, 17:22, 24:26, 28:37, 39:40, 42:52)) %>%
  select(c(1:2, 10:12, 8:9, 7, 6, 12, 4:5, 13)) %>%
  filter(SEASON %in% c("2003-04", "2017-18", "2022-23"))

# Inspect structure
glimpse(filteredJoinedData)

# -------------------------------
# Summary statistics & plots
# -------------------------------
favstats(FG3A ~ SEASON, data = filteredJoinedData)

filteredJoinedData %>%
  ggplot(aes(x = SEASON, y = FG3A)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "NBA Season", y = "Three Pointers Attempted")

filteredJoinedData %>% filter(FG3A > 40)

favstats(DEF_RATING ~ SEASON, data = filteredJoinedData)

filteredJoinedData %>%
  ggplot(aes(x = SEASON, y = DEF_RATING)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "NBA Season", y = "Defensive Rating")

filteredJoinedData %>%
  filter(SEASON == "2003-04") %>%
  filter(DEF_RATING < 95 | DEF_RATING > 108)

filteredJoinedData %>%
  filter(SEASON == "2022-23") %>%
  filter(DEF_RATING <= 108.8)

# -------------------------------
# Offensive Model
# -------------------------------
pointsScoredModel <- lm(PTS ~ FG3A + FG2A + FTA, data = filteredJoinedData)
summary(pointsScoredModel)

# Partial F-tests
pointsScoredReducedFree <- lm(PTS ~ FG3A + FG2A, data = filteredJoinedData)
anova(pointsScoredReducedFree, pointsScoredModel)

pointsScoredReducedThree <- lm(PTS ~ FG2A + FTA, data = filteredJoinedData)
anova(pointsScoredReducedThree, pointsScoredModel)

pointsScoredReducedTwo <- lm(PTS ~ FG3A + FTA, data = filteredJoinedData)
anova(pointsScoredReducedTwo, pointsScoredModel)

# Confidence intervals
confint(pointsScoredModel)

# -------------------------------
# Defensive Model
# -------------------------------
defenseModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG3A + SEASON * OPP_FG2A + SEASON * OPP_FTA,
  data = filteredJoinedData
)
vif(defenseModel, type = "predictor")
summary(defenseModel)

defenseModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG2A + OPP_FTA * SEASON,
  data = filteredJoinedData
)
vif(defenseModel)

defenseModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG2A + OPP_FTA * SEASON + OPP_FG3A * SEASON,
  data = filteredJoinedData
)

defenseRecudedModelThree <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG2A + OPP_FTA * SEASON,
  data = filteredJoinedData
)

defenseReducedTwoModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + OPP_FTA * SEASON + OPP_FG3A * SEASON,
  data = filteredJoinedData
)

defenseReducedFreeThrowModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG2A + OPP_FG3A * SEASON,
  data = filteredJoinedData
)

anova(defenseRecudedModelThree, defenseModel)
anova(defenseReducedTwoModel, defenseModel)
anova(defenseReducedFreeThrowModel, defenseModel)

# Final model
finalDefensiveModel <- lm(
  DEF_RATING ~ OPP_FG3A + OPP_FG2A + OPP_FTA +
    SEASON + SEASON * OPP_FG3A + SEASON * OPP_FG2A,
  data = filteredJoinedData
)
summary(finalDefensiveModel)
confint(finalDefensiveModel)

# -------------------------------
# Diagnostics
# -------------------------------
autoplot(pointsScoredModel) + theme_bw()
vif(pointsScoredModel)

autoplot(finalDefensiveModel) + theme_bw()
vif(finalDefensiveModel, type = "predictor")
