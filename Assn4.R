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
#You create a new file called "bird_total from the original csv, that defines a group by year
#This groups all of the same rows (year) together
#Then you pipe it to summarize the sum of the counts and create a new column called "Bird_sum"
#This also deletes all of the columns besides year and bird_sum

arrange (bird_total, by = desc(Bird_Num))
#Lastly you arrange bird_num by descending, so the greatest value is at the top

##Answer: in 2014, he observed 9303 individuals




#Question 2: In 2014, how many different species of birds did he observe?

Species_of_Birds<-MBT_ebird %>%
  filter(year == 2014) %>%
  count(common_name)
#This creates a new file that filters out the values from rows that match the exact year of 2014
#Then it is piped to count up all of the common names
#You could also use scientific name instead of common name

count(Species_of_Birds, "common_name")
#The total number of different common names are counted
#You could also use scientific name instead of common name, if desired

##Answer: 210 different species of birds in 2014




#Question 3: In which state did he observe the most Red-winged Blackbirds?

RWBB<-MBT_ebird %>%
  filter(common_name == "Red-winged Blackbird") %>%
  group_by(location) %>%
  summarize(State_num=sum(count_tot))
#This creates a new file that filters out only Red-winged Blackbirds
#Then that species is grouped by location
#Afterwards the summarize function creates a new variable called "State_num" and deletes all other columns besides location
#The State_num is the sum of the count_tot

arrange (RWBB, by = desc(State_num))
#The results are arranged with the greatest value at the top, Missouri

##Answer: Missouri, he observed the most Red-winged Blackbirds




#Question 4: Filter observations btwn 5-200 mins. Calculate the mean rate per checklist. Calculate the number of species in each checklist divided by duration. Then take the mean for the year.
Bird_Duration <-MBT_ebird %>%
  filter(duration >= 5 & duration <= 200) %>%
  group_by(list_ID)
#The file Bird_Duration contains data that only has the duration value of equal to or greater than 5min to lesser or equal to 200mins
#Then the rows are grouped together by list_ID

Bird_Duration <- mutate(Bird_Duration, rate=(count/duration))
#A new variable called rate is created which is count divided by duration
#You use the mutate function because you do not want to delete all of the other columns

ungroup(Bird_Duration)
#The data set is then ungrouped, because it needs to be grouped by year

Bird_Duration<-group_by(Bird_Duration, year)
#Data set is grouped by year

summarize(Bird_Duration, mean_value=mean(rate))
#Lastly, the summarize function is used to create a new column which is the the mean of all the rates called "mean_value"
#Then the year and mean of rates per year are listed in the output 

##Answer: Observe the output for the answer




#Question 5: Create a tibble that includes the top 10 most frequently observed species.

Top_10_Birds<-MBT_ebird %>%
  group_by(common_name) %>%
  mutate(frequency = sum(count)) %>%
  slice(which.max(frequency)) 
#A new file is called Top_10_Birds which groups the rows by common_name
#Mutate is used so the other rows are not deleted, and a new variable called frequency is created
#Then it slices the rows with the max frequency values


Top_10<- head(arrange(Top_10_Birds, by = desc(frequency)),10) %>%
  group_by(scientific_name, frequency) %>%
  filter(frequency == max(frequency, na.rm=TRUE))
#Then another file is created which arranges the Top_10_Birds and slices out the top ten frequency values
#The scientific name and frequency are grouped
#Afterwards the filter function collects the max frequency values per scientific name
#The last part is used in case NA's are included within the data

require(readr)
#This package may or may not be required to export the csv file

write_csv(Top_10, "Results/Top_10_Birds.csv")
#Export the csv file
#It includes all of the observation columns for the top 10 observed species


##Answer: view csv file
