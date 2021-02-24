
# c. mcclintock
# defund mpd - now

# ................................................................

# load libraries
library(tidyverse)
library(RColorBrewer)
library(knitr)
library(scales)

# read in the data 
stops1 <- read_csv("data/stops1.csv", na = "NULL")
stops2 <- read_csv("data/stops2.csv", na = "NULL")

# ................................................................

# join time periods together
stops <- full_join(stops1, stops2, by=names(stops1))

# categorical duration values
stops$duration_cut <- cut(stops$stop_duration_minutes, 
                          breaks = c(1,5,20,60, 120, 360), 
                          include.lowest = T)

# create subset for youth
youth <- subset(stops, age=="Juvenile")

# age to numeric
stops$age <- as.numeric(stops$age)

stops$race_ethnicity <- fct_explicit_na(stops$race_ethnicity)

# race by stop district
ggplot(subset(stops, !is.na(stop_district)), aes(stop_district, fill=race_ethnicity)) + 
  geom_bar(position="fill") + 
  scale_fill_manual(values=brewer.pal(8, "Set2")) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="Race of People Stopped by Police District", 
       fill="Race/Ethnicity", 
       x="District", y="Percent") 

# stop duration by race
ggplot(stops, aes(duration_cut, fill=race_ethnicity)) + geom_bar(position="dodge")

# and median and mean
stops %>% 
  group_by(race_ethnicity) %>% 
  summarize(median=median(stop_duration_minutes, na.rm = T), 
            mean=mean(stop_duration_minutes, na.rm=T))

# youth stop race demographics
table(youth$race_ethnicity) %>% prop.table()

# overall stop race demographics
table(stops$race_ethnicity) %>% prop.table()


# age by race
ggplot(stops, aes(age, fill=race_ethnicity)) + geom_density(alpha=0.6)

# stop type by race
kable(100*prop.table(table(stops$stop_type, stops$race_ethnicity), margin=2), digits = 2)
# black citizens who were stopped were more likely to have a non-ticket stop 
# evidence of racial profiling



