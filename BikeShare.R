library(tidyverse)
library(tidymodels)
library(vroom)
library(lubridate)
library(patchwork)

test  <- vroom("test.csv")
train <- vroom("train.csv")

train <- train %>%
  mutate(dt = ymd_hms(datetime),
         hour = hour(dt),
         season = factor(season),
         weather = factor(weather))

plot1 <- ggplot(data = train, aes(x = weather)) + geom_bar()
plot2 <- ggplot(data = train, aes(x = hour, y = count)) + stat_summary(fun = mean, geom = "line")
plot3 <- ggplot(data = train, aes(x = season, y = count)) + geom_boxplot()
plot4 <- ggplot(data = train, aes(x = temp, y = count)) + geom_point(alpha = .2) + geom_smooth(se = FALSE)

(plot1 + plot2) / (plot3 + plot4)


