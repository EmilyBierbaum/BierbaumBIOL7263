#Emily L. Bierbaum
#October 20, 2022
#Assignment 5

#Required packages for this assignment
require(tidyverse)
require(readr)
require(dplyr)

#Import the part 1 and 2 files from GitHub 

Part1_Assn5<-read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/assignment6part1.csv?raw=true",show_col_types = FALSE) 

Part2_Assn5<-read_csv("https://github.com/mbtoomey/Biol_7263/blob/main/Data/assignment6part2.csv?raw=true", show_col_types = FALSE)
#I had issues importing the csv files, a warning popped up about using the specs() function
#Adding in "?csv=true" and "show_col_types = FALSE" helped in uploading the files successfully
  ##The last function helps if the column types are not clearly specified or untidy

Part1_tibble <- Part1_Assn5 %>% pivot_longer(cols = starts_with("Sample"),
                                             names_to = c("Sample", "Sex", "Group"),
                                             names_prefix = "Sample", names_transform = list(Sample = as.integer),
                                             names_sep = "_") %>%
  pivot_wider(names_from = ID, values_from = value)
#A new file is created "Part1_tibble" which takes the columns information and pivots it into rows with the function 'pivot_longer'
  ## the columns decrease in number and the rows increase, making the data tidier and easier to analyze
#I then specified which columns to be pivoted (start with "Sample")
#Then with 'names_to' I indicated what the column names will be called
#The 'names_prefix' removes the matching text ("Sample") from the start of each column name
#When I tried to combine at the end of the assignment, there was a problem with one tibble being an integer and the other being a string, so I changed them both to integers
#Lastly 'names_sep' splits the column names at the "_"

#'Pivot_wider' helps when there are observations spread across multiple rows, this increases columns and decreases rows
#Two columns are added for age and body length





Part2_tibble <- Part2_Assn5 %>% pivot_longer(cols = starts_with("Sample"), 
                                             names_to = c("SampleTreatment"), 
                                             names_prefix = "Sample",
                                             names_transform = list(Sample = as.integer), 
                                             values_to = "count") %>% separate(SampleTreatment,
                                             into = c("Sample", "Treatment"), 
                                             convert = TRUE) %>% 
  pivot_wider(names_from = ID, values_from = count) %>% select(-Treatment)

#Similar to above, the 'pivot_longer' expression was used for columns that start with "Sample"
#Then those are renamed to "SampleTreatment"
#The values are transformed from a string into an integer
#Then the values are reorganized in each column into a variable called count
#Afterwards the "SampleTreatment" is separated into "Sample" and "Treatment"
  ##The 'convert=true' option keeps the values as integers

#'Pivot_wider' helps when there are observations spread across multiple rows, this increases columns and decreases rows
#The "-Treatment" keeps both columns, if you only use "Treatment" then the mass column deletes
#Lastly a column for mass was created with 'pivot_wider'


Part1_tibble %>% full_join(Part2_tibble, by = "Sample") -> Combined_Data
#Initially I tried a partial_join, but the NA values were not exported, so I ended up using the full_join command

write_csv(Combined_Data, "Results/Combined_Assn5_Data.csv")
#Export the combined data above



Combined_Data %>% transmute(Sex=Sex, Group=Group, resid_mass=mass/body_length) %>% 
  group_by(Group, Sex) %>% 
  summarize(mean_mass = mean(resid_mass, na.rm = TRUE), SD_mass = sd(resid_mass, na.rm = TRUE)) -> Residual_Mass 
#Transmute adds a new variable and drops the existing ones
  ##Want to keep sex, group and the new variable residual mass (mass divided by body length)
#The rows are grouped by sex and group type
#Then mean_mass is calculated by the mean of residual mass and then the standard deviation of the residual mass is calculated
  ## 'na.rm = TRUE' skips over the NA values


write_csv(Residual_Mass, "Results/Final_Residual_Mass.csv")
#Export file

