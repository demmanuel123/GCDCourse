library(reshape2)
library(Hmisc)
library(dplyr)

## Script to analyse the wearable computing results 
## Author : Daina Emmanuel  

## Set the working directory where the observation files are available
setwd("C://Personal//Coursera//CleaningData//GCD Course Project//getdata-projectfiles-UCI HAR Dataset//UCI HAR Dataset")

## Reading the relevant files as table 
## features.txt <- this will be an input for the variables names 
## The observation are of 2 category : 1 recorded for training and the other for test.
## subject_train/subject_test <- input for the subject corresponding to whom the observation is collected
## X_train/X_test <- the measures of the observed variables 
## Y_train/Y_test <- value corresponding to the activity for which the measures are recorded

feature <- read.table("features.txt",stringsAsFactors=FALSE,colClasses = 'character')
subject_train <- read.table("train//subject_train.txt",stringsAsFactors=FALSE,colClasses = 'character')
X_train <- read.table("train//X_train.txt",stringsAsFactors=FALSE)
Y_train <- read.table("train//y_train.txt",stringsAsFactors=FALSE,colClasses = 'character')
subject_test <- read.table("test//subject_test.txt",stringsAsFactors=FALSE,colClasses = 'character')
X_test <- read.table("test//X_test.txt",stringsAsFactors=FALSE,colClasses = 'character')
Y_test <- read.table("test//y_test.txt",stringsAsFactors=FALSE,colClasses = 'character')

##
# Appropriately labels the data set with descriptive variable names. 

nameCol <- c((make.names(feature$V2, unique=TRUE, allow_ = TRUE)))
names(X_train) <- nameCol

## preserving the measure names from the features file , 
## this will help in easier interpretation of data

names(Y_train)<- "obsActivity"
names(subject_train) <- "obsID"
names(X_test) <- nameCol
names(Y_test)<- "obsActivity"
names(subject_test) <- "obsID"

## generating training data 
train_ind_data <- cbind(subject_train
#                        ,obsType="train"
                        ,Y_train
                        ,X_train

)
# generating test data 
test_ind_data <- cbind(subject_test
#                       ,obsType="test"
                        ,Y_test
                        ,X_test
)
# Merges the training and the test sets to create one data set.

observation_table1 <- rbind(train_ind_data,test_ind_data)


# Extracts only the measurements on the mean and standard deviation for each measurement. 
## subset by mean and std and bind the same
mean_subset <-select(observation_table1, contains("mean"))
std_subset <- select(observation_table1, contains("std"))

observation_table2 <- cbind(observation_table1$obsID,
#                (observation_table1$obsType),
                observation_table1$obsActivity,
                mean_subset,
                std_subset)

names(observation_table2)[1] <-"obsID"
#names(observation_table2)[2] <-"obsType"
names(observation_table2)[2] <-"obsActivity"
observation_table2 <- tbl_df(observation_table2)

# Uses descriptive activity names to name the activities in the data set

observation_table2$obsActivity_str <- "TBD"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="1")] <- "WALKING"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="2")] <- "WALKING_UPSTAIRS"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="3")] <- "WALKING_DOWNSTAIRS"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="4")] <- "SITTING"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="5")] <- "STANDING"
observation_table2$obsActivity_str[which(as.character(observation_table2$obsActivity) =="6")] <- "LAYING"


# From the data set in step 4, creates a second, independent tidy data set
# with the average of each variable for each activity and each subject.

id <- c(3:88) 
observation_table2[,id] <- as.numeric(as.character(unlist(observation_table2[,id]),
                                                   drop=FALSE,recursive =FALSE,
                                                   use.names=FALSE))
observation_table2 <- observation_table2[,c(1,89,3:88)]

observation_table2 <- group_by(observation_table2,obsID,obsActivity_str) %>%
  summarise_each(funs(mean))%>% arrange(obsID,obsActivity_str)

print(dim(observation_table2))

View(observation_table2)


