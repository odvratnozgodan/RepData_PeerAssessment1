week_days=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

df_filled_steps <- (
        df_filled_steps %>% 
          mutate(day=as.factor(
              ifelse(weekdays(date) %in% week_days, "weekday", "weekend")
            )
          )
                    
)

grouped_interval_filled_steps <- df_filled_steps %>% group_by(interval, day)
weekdays <- grouped_interval_filled_steps  %>% filter(day=="weekday") %>% summarise(steps=mean(steps))
weekends <- grouped_interval_filled_steps  %>% filter(day=="weekend") %>% summarise(steps=mean(steps))
grouped_interval_filled_steps <- bind_rows(weekdays, weekends)

head(grouped_interval_filled_steps)


ggplot(data=grouped_interval_filled_steps, aes(x=interval, y=steps)) + geom_line() +facet_wrap(~day, nrow=2)