
library(tidyverse)


stops1 <- read_csv("data/stops1.csv")
stops2 <- read_csv("data/stops2.csv")


stops <- full_join(stops1, stops2, by=names(stops1))

stops$duration_cut <- cut(stops$stop_duration_minutes, 
                          breaks = c(1,5,20,60, 120, 360), 
                          include.lowest = T)

youth <- subset(stops, age=="Juvenile")

stops$age <- as.numeric(stops$age)

ggplot(stops, aes(stop_district, fill=race_ethnicity)) + geom_bar(position="fill")

ggplot(stops, aes(duration_cut, fill=race_ethnicity)) + geom_bar(position="dodge")

stops %>% 
  group_by(race_ethnicity) %>% 
  summarize(median=median(stop_duration_minutes, na.rm = T), 
            mean=mean(stop_duration_minutes, na.rm=T))


table(youth$race_ethnicity)

ggplot(stops, aes(age, fill=race_ethnicity)) + geom_density(alpha=0.6)


