# Clean environment
rm(list=ls())

# Load packages
library(plyr)
library(dplyr)
library(ggplot2)

# Import file with predictions and actuals
aggdf <- read.csv("your directory")

# Remove columns and create hours column
aggdf <- aggdf[-c(1)]
aggdf$Prediction <- aggdf$Prediction * aggdf$Actual.weight
aggdf <- aggdf[-c(3:4)]
aggdf <- aggdf[-c(4:17)]


# Create deviation columns
aggdf$Deviation <- (aggdf$Actual.hours - aggdf$Prediction)
aggdf$ABSDeviation <- abs(aggdf$Actual.hours - aggdf$Prediction)
aggdf$DeviationPercentage <- (aggdf$Actual.hours - aggdf$Prediction) / aggdf$Actual.hours
aggdf$ABSDeviationPercentage <- abs(aggdf$Actual.hours - aggdf$Prediction) / aggdf$Actual.hours

# Aggregate tables, sum deviations
aggdf$ProjectID <- as.factor(aggdf$ProjectID)

aggregated_df2 <- aggregate(aggdf$Deviation, by=list(ProjectID=aggdf$ProjectID), FUN=sum)

aggregated_df3 <- aggdf %>% count(ProjectID)

aggregated_df4 <- aggregate(aggdf$Actual.hours, by=list(ProjectID=aggdf$ProjectID), FUN=sum)

aggregated_df5 <- aggregate(aggdf$ABSDeviation, by=list(ProjectID=aggdf$ProjectID), FUN=sum)

aggregated_df6 <- aggregate(aggdf$DeviationPercentage, by=list(ProjectID=aggdf$ProjectID), FUN=sum)

aggregated_df7 <- aggregate(aggdf$ABSDeviationPercentage, by=list(ProjectID=aggdf$ProjectID), FUN=sum)


aggregated_df <-inner_join(aggregated_df3, aggregated_df2)
aggregated_df <-inner_join(aggregated_df, aggregated_df4, by = join_by(ProjectID))
aggregated_df <-inner_join(aggregated_df, aggregated_df5, by = join_by(ProjectID))
aggregated_df <-inner_join(aggregated_df, aggregated_df6, by = join_by(ProjectID))
aggregated_df <-inner_join(aggregated_df, aggregated_df7, by = join_by(ProjectID))

# Divide the sum of deviations by the amount of sections in a project to get the Mean errors
aggregated_df$ME <- aggregated_df$x.x / aggregated_df$n
aggregated_df$MAE <- aggregated_df$x.x.x / aggregated_df$n
aggregated_df$MPE <- aggregated_df$x.y.y / aggregated_df$n
aggregated_df$MAPE <- aggregated_df$x / aggregated_df$n

summary(aggregated_df$MAE)

# Plot all MPE's
aggregated_df |>
  ggplot(aes(x = MPE)) +
  geom_histogram(bins = 50) +
  ylab("Frequency") +
  xlab("MPE") +
  theme_bw(base_size = 22)

# Plot a single project
df_id <- filter(aggdf, ProjectID == "id")

df_id |>
  ggplot(aes(x = Actual.hours, y = Prediction)) +
  geom_point() +
  xlab("Actuals") +
  ylab("Predictions") +
  geom_abline() +
  theme_bw(base_size = 22)


