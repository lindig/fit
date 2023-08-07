#! /usr/bin/env -S Rscript

library(dplyr)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
file = args[1L]
base = tools::file_path_sans_ext(file)

df <- read.csv(file)
df <- df[df$speed > 3.0,]
p <- ggplot(df, aes(cadence, speed)) 
p <- p + geom_smooth(method="glm", formula=y~log(x)) 
p <- p + geom_jitter() 
p <- p + scale_y_continuous(sec.axis = sec_axis(~ (500/.), 
    name = "speed s/500m", 
    breaks=c(87,90,95,100,105,110,115,120,125,130,140,150)),
    limits=c(2,5.8),name="speed m/s")
p <- p + scale_x_continuous(limits=c(15, 45),name="stroke/min")
ggsave(paste0(base,".png"),p)
