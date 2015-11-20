suppressPackageStartupMessages(library(ggplot2))

steps_per_day = df %>% filter(complete.cases(steps)) %>% group_by(date) %>% summarise(steps=sum(steps))

breaks <- pretty(range(steps_per_day$steps), n = nclass.FD(steps_per_day$steps), min.n = 1)
bwidth <- breaks[2]-breaks[1]
qplot(data=steps_per_day, steps, xlab="Steps per day", binwidth = bwidth) + labs(title="Histogram of steps/day");

mean = steps_per_day %>% summarise(mean(steps));
median = steps_per_day %>% summarise(median(steps))