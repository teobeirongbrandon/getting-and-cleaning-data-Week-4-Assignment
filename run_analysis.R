

library(dplyr)



#reading data

#training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

#test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

#read labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


#Merge the training and the test sets 

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

#column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


#Extract the measurements on the mean and std dev

#determine data set to keep
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

#keeping data in these columns 
humanActivity <- humanActivity[, columnsToKeep]


#Use descriptive names to name the activities 

#replace activity values
humanActivity$activity <- factor(humanActivity$activity, 
  levels = activities[, 1], labels = activities[, 2])


#label data set with descriptive variable names

#getting column names
humanActivityCols <- colnames(humanActivity)

#removing special chars
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

#expanding abbreviations 
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "Standard Deviation", humanActivityCols)


#use new labels as column names
colnames(humanActivity) <- humanActivityCols


#Create a second, independent tidy set with the average of each variable for each activity and each subject

#group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

#output
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
