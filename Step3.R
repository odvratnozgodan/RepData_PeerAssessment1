grouped_interval <- df %>% filter(complete.cases(steps)) %>% group_by(interval) %>% summarise(steps=sum(steps))

head(grouped_interval)

ggplot(data=grouped_interval, aes(x=interval, y=steps, group=1)) + geom_line()

interval_with_max_steps <- grouped_interval %>% arrange(desc(steps)) %>% top_n(1)

interval_with_max_steps
