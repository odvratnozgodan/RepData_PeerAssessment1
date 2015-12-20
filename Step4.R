#the number of missing values
dim(df %>% filter(is.na(steps)))[1]

# calculate the average steps per day while replacing the ones whith missing values
avg_steps_per_day <- (df %>% mutate(steps = ifelse(is.na(steps), 0, steps)) 
                      %>% group_by(date) 
                      %>% summarise( steps=round(mean(steps)) )
)

df_filled_steps <- df


for(i in 1:nrow(df_filled_steps)) {
  date <- df_filled_steps[i, "date"]
  steps <- df_filled_steps[i, "steps"]
  if(is.na(steps)){
    df_filled_steps[i, "steps"]<-avg_steps_per_day[avg_steps_per_day$date == date, "steps"]
  }
}

steps_per_day_filled_na = df_filled_steps %>% group_by(date) %>% summarise(steps=sum(steps))

qplot(data=steps_per_day_filled_na, steps, xlab="Steps per day", binwidth = bwidth) + labs(title="Histogram of steps/day with filled NA values");

mean_filled_na = steps_per_day_filled_na %>% summarise(mean(steps));
median_filled_na = steps_per_day_filled_na %>% summarise(median(steps))

mean_filled_na
median_filled_na