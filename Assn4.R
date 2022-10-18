#Emily L. Bierbaum
#Assignment 4
#October 18, 2022


#Download the required R packages for this assignment

require(tidyverse)

require(dplyr)


#Import the csv file of interest
#Make sure you are in the correct working directory

MBT_ebird<-read_csv("Data/MBT_ebird.csv")

getwd()
#If your file does not load, check your directory 


glimpse(MBT_ebird)
#Examine the csv file



#Question 1: In which year did he observe the most individual birds, and how many?

bird_total<-MBT_ebird %>%
  group_by(year) %>%
  summarize(Bird_Num=sum(count))


arrange (bird_total, by = desc(Bird_Num))

##Answer: in 2014, he observed 9303 individuals



#Question 2: In 2014, how many different species of birds?

Species_of_Birds<-MBT_ebird %>%
  filter(year == 2014) %>%
  count(common_name)

count(Species_of_Birds, "common_name")

##Answer: 210 different species of birds in 2014



#Question 3: In which state did he observe the most Red-winged Blackbirds?

RWBB<-MBT_ebird %>%
  filter(common_name == "Red-winged Blackbird") %>%
  group_by(location) %>%
  summarize(State_num=sum(count_tot))

arrange (RWBB, by = desc(State_num))

##Answer: Missouri, he observed the most Red-winged Blackbirds


#Question 4: Filter observations btwn 5-200 mins. Calc mean rate per checklist. Calc the num of species in each checklist divided by duration. Then take the mean for the year.

Bird_Duration <-MBT_ebird %>%
  filter(duration >= 5 & duration <= 200) %>%
  group_by(list_ID)

# create the new variable rate
Bird_Duration <- mutate(Bird_Duration, rate=(count/duration))

ungroup(Bird_Duration)

Bird_Duration<-group_by(Bird_Duration, year)

summarize(Bird_Duration, mean_value=mean(rate))

##Answer: Observe the output for the answer


#Question 5: Create a tibble that includes the top 10 most frequently observed species.

Top_10_Birds<-MBT_ebird %>%
  group_by(common_name) %>%
  mutate(frequency = sum(count)) %>%
  slice(which.max(frequency)) 

Top_10<- head(arrange(Top_10_Birds, by = desc(frequency)),10) %>%
  group_by(scientific_name, frequency) %>%
  filter(frequency == max(frequency, na.rm=TRUE))

# this package may or may not be required to export the csv file
require(readr)

write_csv(Top_10, "Results/Top_10_Birds.csv")

##Answer: view csv file
