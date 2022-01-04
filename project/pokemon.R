.libPaths("C:/Program Files (x86)/R/win-library/4.0")
library(dplyr)
library(ggplot2)
library(rgrs)

data = read.csv("./project/Pokemon.csv", header = TRUE, sep = ",")

head(data)
summary(data)

data[["Type.2"]][data[["Type.2"]] == ""] <- "None"
data[["Legendary"]] <- as.logical(data[["Legendary"]])
data$Number <- data$X
data <- data[,-1] %>% select(Number, Name:Legendary)
data$Generation <- as.factor(data$Generation)
head(data)

gen1 <- data %>% filter(!grepl("Mega", Name, fixed = TRUE) & Generation == 1 & Legendary == FALSE)

gen1 %>% 
  select(Type.1, HP:Speed) %>% 
    group_by(Type.1) %>% 
      summarize_all(funs(if(is.numeric(.)) mean(.) else table(.)))

head(gen1)
boxplot(gen1[, "HP"])
ggplot(gen1, aes(x= Attack, y= Defense)) + geom_point(aes(color = Type.1, size = HP )) 
