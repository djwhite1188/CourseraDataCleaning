#Criteria
#  - The submitted data set is tidy.
#  - The Github repo contains the required scripts.
#  - GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
#  - The README that explains the analysis files is clear and understandable.
#  - The work submitted for this project is the work of the student who submitted it.

#Submit
#  - a tidy data set as described below
#  - a link to a Github repository with your script for performing the analysis
#  - a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
#  - README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

#  - CHECK! Merges the training and the test sets to create one data set.
#  - CHECK! Extracts only the measurements on the mean and standard deviation for each measurement.
#  - CHECK! Uses descriptive activity names to name the activities in the data set
#  - CHECK! Appropriately labels the data set with descriptive variable names.
#  - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Data Description
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#6 Activities (located in the activity_labels file) - walking / walking upstairs / walking downstairs / sitting / standing / laying
#From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
#A 561-feature vector with time and frequency domain variables. 

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

#Download the file
link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
saved <- "UCI.zip"
download.file(link, saved)

#Unzip the file
#install.packages("utils")
library(utils)
unzip(saved)

#MERGE TRAIN AND TEST
x_test <- read.csv("~/UCI HAR Dataset/test/x_test.txt", header = FALSE, sep = "")
y_test <- read.csv("~/UCI HAR Dataset/test/y_test.txt", header = FALSE)
x_train <- read.csv("~/UCI HAR Dataset/train/x_train.txt", header = FALSE, sep = "")
y_train <- read.csv("~/UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_test <- read.csv("~/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subject_train <- read.csv("~/UCI HAR Dataset/train/subject_train.txt", header = FALSE)

#!!!NOTE!!!! Y table is a column describing activity - X is a wide table that has the metrics/features of each activity - they match on row count

x <- rbind(x_test, x_train)
y <- rbind(y_test, y_train)
sub <- rbind(subject_test,subject_train)

a <- as.data.frame(seq(from=1, to=nrow(x), by = 1))
colnames(a) <- "key"


y_key <- cbind(y,a)
sub_key <- cbind(sub,a)

#Add in descriptive labels

features <- read.csv("~/UCI HAR Dataset/features.txt", header = FALSE, sep = "")

feat <- as.character(features[,2])

colnames(x) <- feat
x_key <- cbind(x,a)
colnames(sub_key) <- c("Subject","key")

#Make Real Labels by combining activity and y (activities)
activity_labels <- read.csv("~/UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")
colnames(activity_labels) <- c("b", "Activity")

y_w_label <- merge(y_key, activity_labels, by.x = "V1", by.y = "b")

#install.packages("dplyr")
library(dplyr)
y_fin <- y_w_label %>%
    arrange(key) %>%
    select(key,Activity)

#  - Extracts only the measurements on the mean and standard deviation for each measurement.

# mean(): Mean value
# std(): Standard deviation

x_key_names <- as.character(names(x_key))

x_mean_std_names <- grep("mean\\()|std()|key", x_key_names, value = TRUE)

x_fin <- x_key[,x_mean_std_names]

#f is for frequency / t is for time
names(x_fin) <- sub("f","freq-", sub("t","time-", names(x_fin)))

#combine final dataset

big_fin <- merge(merge(x_fin, y_fin, by = "key"), sub_key, by = "key")

#  - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Activity - y / subject - x / 66 activities - y

#install.packages("tidyr")
library(tidyr)
data_long <- gather(big_fin, feature, calc, 2:67, factor_key=TRUE)

#!!!!!!!!NOTE!!!!!!!!!!!! the below calculates the average of each feature
avg_avg <- data_long %>% 
            group_by(Activity, Subject, feature) %>%
            summarize(avg = mean(calc))

#Return dataset to wide tidy
data_wide <- spread(avg_avg, feature, avg)

View(data_wide)

#Write dataset
write.table(data_wide,"~/Wearable_Tidy_Coursera_Assignment.txt", row.name=FALSE)  
