library(tidyverse)
library(rpart)
library(rpart.plot)

tsl_veri <- tr_super_league_matches %>% select(-matchid, -Match_Date, -Match_Result)
tsl_veri <- tsl_veri %>% mutate(difference = Home_Score - Away_Score) %>%
  mutate(ou25 = Home_Score + Away_Score > 2.5) %>%
  mutate(ou35 = Home_Score + Away_Score > 3.5)

#Over/Under 2.5 model

tsl_train <- tsl_veri %>% filter(season<2017) %>% select(-season, -ou35)
tsl_model <- rpart(ou25 ~., data = tsl_train)
rpart.plot(tsl_model)

tsl_test <- tsl_veri %>% filter(season==2017) %>% select(-season, -ou35) %>% 
  filter(!grepl("goztepe|yeni malatyaspor",Home)) %>%
  filter(!grepl("goztepe|yeni malatyaspor",Away))
tsl_predict <- predict(tsl_model, newdata = tsl_test)
head(tsl_predict)

tsl_test_prediction <-
  cbind(
    tsl_predict %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_predict >= 2.5,1,0)),
    tsl_test %>% tbl_df %>%
      transmute(tsl_actual = ifelse(ou25 ==1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_test_prediction

tsl_in_sample <- predict(tsl_model)
head(tsl_in_sample)

tsl_train_prediction <-
  cbind(
    tsl_in_sample %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_in_sample >= 2.5,1,0)),
    tsl_train %>% tbl_df %>%
      transmute(tsl_actual = ifelse(ou25 == 1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_train_prediction

#Over/Under 3.5 model

tsl_train <- tsl_veri %>% filter(season<2017) %>% select(-season, -ou25)
tsl_model <- rpart(ou35 ~., data = tsl_train)
rpart.plot(tsl_model)

tsl_test <- tsl_veri %>% filter(season==2017) %>% select(-season, -ou25) %>% 
  filter(!grepl("goztepe|yeni malatyaspor",Home)) %>%
  filter(!grepl("goztepe|yeni malatyaspor",Away))
tsl_predict <- predict(tsl_model, newdata = tsl_test)
head(tsl_predict)

tsl_test_prediction <-
  cbind(
    tsl_predict %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_predict >= 3.5,1,0)),
    tsl_test %>% tbl_df %>%
      transmute(tsl_actual = ifelse(ou35 ==1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_test_prediction

tsl_in_sample <- predict(tsl_model)
head(tsl_in_sample)

tsl_train_prediction <-
  cbind(
    tsl_in_sample %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_in_sample >= 3.5,1,0)),
    tsl_train %>% tbl_df %>%
      transmute(tsl_actual = ifelse(ou35 == 1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_train_prediction

#Goal difference model

tsl_train <- tsl_veri %>% filter(season<2017) %>% select(-season, -ou25, -ou35)
tsl_model <- rpart(difference ~., data = tsl_train)
rpart.plot(tsl_model)

tsl_test <- tsl_veri %>% filter(season==2017) %>% select(-season, -ou25, -ou35) %>% 
  filter(!grepl("goztepe|yeni malatyaspor",Home)) %>%
  filter(!grepl("goztepe|yeni malatyaspor",Away))
tsl_predict <- predict(tsl_model, newdata = tsl_test)
head(tsl_predict)

tsl_test_prediction <-
  cbind(
    tsl_predict %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_predict >= 0,1,0)),
    tsl_test %>% tbl_df %>%
      transmute(tsl_actual = ifelse(difference ==1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_test_prediction

tsl_train_prediction <-
  cbind(
    tsl_in_sample %>% tbl_df %>%
      transmute(tsl_predict = ifelse(tsl_in_sample >= 0,1,0)),
    tsl_train %>% tbl_df %>%
      transmute(tsl_actual = ifelse(difference == 1,1,0))
  ) %>%
  mutate(correct_class = (tsl_predict == tsl_actual)) %>%
  group_by(correct_class) %>%
  summarise(count=n(),percentage=n()/nrow(.))
tsl_train_prediction
