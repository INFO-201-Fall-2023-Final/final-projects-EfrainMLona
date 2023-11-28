#EFRAIN MORENO LONA and GEORGE GARCIA
#emore@uw.edu
#I collaborated with George Garcia on this assignment

#all library statements must be at the top of the page!
library(dplyr)
library(stringr)

# Overview ---------------------------------------------------------------------

# Homework 4: Climate Change Analysis
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to calculate the answer.
# For grading, it's important that you store your answers in the variable names
# listed with each question in `backtics`. Make sure you DO NOT hardcode values
# unless specified to do so in the instructions! 
# BEFORE YOU SUBMIT YOUR ASSIGNMENT, MAKE SURE YOU COMMENT OUT ANY PLOT() STATEMENTS

# Loads in your datasets
drug_df <- read.csv("overdose.csv") #DO NOT CHANGE!
crime_df <- read.csv("crime.csv") #DO NOT CHANGE!

#Let's clean some data
#We don't need the following from the crime dataframe:
#Report.Number, Offense.ID, Offense.Start, Offense.End, Group, Offense.Code, Precinct, Sector,
#Beat, MCPP, X100, Longitude, Latitude, let's start by dropping that.
crime_df$Offense.ID <- NULL
crime_df$Offense.Start.DateTime <- NULL
crime_df$Offense.End.DateTime <- NULL
crime_df$Report.Number <- NULL
crime_df$Group.A.B <- NULL
crime_df$Offense.Code <- NULL
crime_df$Precinct <- NULL
crime_df$Sector <- NULL
crime_df$Beat <- NULL
crime_df$MCPP <- NULL
crime_df$X100.Block.Address <- NULL
crime_df$Longitude <- NULL
crime_df$Latitude <- NULL

# Now that we've cleaned a lot of the unnecessary columns, we need to have a column that we can join
# on for drug_df, we see that drug_df has "year" so let's try to change Report.DateTime to match that format
crime_df <- mutate(crime_df, Year = as.numeric(str_sub(Report.DateTime, 7,10)))

# We don't need Report.DateTime now
crime_df$Report.DateTime <- NULL

# Let's drop any crime data from after the year 2020 or before 2010
crime_df <- crime_df %>% filter(between(Year, 2008, 2020))
# Let's do the same for overdose data
# First we need to convert the year to numeric type
drug_df <- transform(drug_df, year = as.numeric(year))
# Now we can filter
drug_df <- drug_df %>% filter(between(year, 2008, 2020))
# Now that we have data within the same year for both groups, let's lower the amount of rows for crime_df
# We want to filter to only have drug related crimes
crime_df <- crime_df %>% filter(Offense.Parent.Group == "DRUG/NARCOTIC OFFENSES")
# We no longer need the crime.against or offense
crime_df$Crime.Against.Category <- NULL
crime_df$Offense <- NULL


#Let's join the two datasets
names(crime_df)[names(crime_df) == 'Year'] <- 'year'

df_2 <- drug_df %>% group_by(year, drug_type) %>% summarize(
  count = sum(count),
  age_adj_rate = mean(age_adj_rate),
  population = sum(population),
  .groups = 'drop')

df_3 <- crime_df %>% group_by(year) %>% summarize(
  crimes = length(Offense.Parent.Group),
  .groups = 'drop'
)

df_4 <- merge(df_3, df_2)

