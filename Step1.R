suppressPackageStartupMessages(library(dplyr))
if(file.exists("activity.zip")){
    unzip("activity.zip");
    df = data.frame(read.csv("activity.csv"));
}else{
    temp <- tempfile("activity.zip");
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = "activity.zip", method = "curl");
    unzip("activity.zip");
    unlink(temp);
    df = data.frame(read.csv("activity.csv"));
}

df = mutate(df, date = as.Date(date, format = '%Y-%m-%d'));
head(df);