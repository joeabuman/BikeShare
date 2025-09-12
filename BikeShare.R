# STAT348 – Bike Sharing (tidymodels; slide-style; NA-safe)

library(tidymodels)
library(tidyverse)
library(vroom)
library(lubridate)

# ---- read ----
train <- vroom("train.csv", show_col_types = FALSE)
test  <- vroom("test.csv",  show_col_types = FALSE)

# ---- feature engineering (works on both; no NAs from levels) ----
make_features <- function(df){
  df %>%
    mutate(
      dt    = ymd_hms(datetime),
      hour  = factor(hour(dt), levels = 0:23),
      wday  = wday(dt, label = TRUE, abbr = TRUE),
      month = month(dt, label = TRUE, abbr = TRUE),
      year  = factor(year(dt)),
      season  = factor(season, 1:4, c("Spring","Summer","Fall","Winter")),
      weather = factor(weather, 1:4,
                       c("Clear/Partly","Mist/Clouds","Light snow/rain","Heavy snow/rain")),
      holiday    = factor(holiday),
      workingday = factor(workingday)
    )
}

# align factor levels by engineering on the combined frame
both_raw <- bind_rows(train %>% mutate(.set = "train"),
                      test  %>% mutate(.set = "test"))
both_fe  <- make_features(both_raw)
trainData <- both_fe %>% filter(.set == "train") %>% select(-.set)
testData  <- both_fe %>% filter(.set == "test")  %>% select(-.set)

# ---- model (slides style) ----
my_linear_model <-
  linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>%
  fit(formula = log1p(count) ~
        temp + atemp + humidity + windspeed +
        season + weather + hour + wday + month + year +
        holiday + workingday,
      data = trainData)

# ---- predictions; back-transform; ensure no NA ----
bike_predictions <- predict(my_linear_model, new_data = testData) %>%
  mutate(.pred = pmax(0, expm1(.pred))) %>%      # back-transform to original scale
  tidyr::replace_na(list(.pred = 0))             # guard against any residual NA

# ========================
# KEEPING YOUR BOTTOM BLOCK EXACTLY
# ========================
## Format the Predictions for Submission to Kaggle
kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")

