# packages
library(data.table)
library(dplyr)

# read train data
subjectTrain <- read.table('UCI HAR Dataset/train/subject_train.txt', header = FALSE)
activityTrain <- read.table('UCI HAR Dataset/train/y_train.txt', header = FALSE)
featuresTrain <- read.table('UCI HAR Dataset/train/X_train.txt', header = FALSE)

# read test data
subjectTest <- read.table('UCI HAR Dataset/test/subject_test.txt', header = FALSE)
activityTest <- read.table('UCI HAR Dataset/test/y_test.txt', header = FALSE)
featuresTest <- read.table('UCI HAR Dataset/test/X_test.txt', header = FALSE)

# read supporting text file
featureNames <- read.table('UCI HAR Dataset/features.txt')
activityLabels <- read.table('UCI HAR Dataset/activity_labels.txt', header = FALSE)

# Merge train and test file 
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# set names
names(subject)<-c('subject')
names(activity)<- c('activity')
names(features)<- featureNames$V2

# creating complete Data
Data <- cbind(features,activity,subject)

#########################################################################
#########################################################################
#########################################################################

# Extract the column indices that have either mean or std
selected_columns <- grep('.*Mean.*|.*Std.*', names(Data), ignore.case=TRUE)

# add activity and subjects
required_columns <- c(selected_columns, 562:563)

# create selected_Data and take a look on its dimension
selected_Data <- Data[ , required_columns]
dim(selected_Data)

#########################################################################
#########################################################################
#########################################################################

# transform activity in factor variable
Data$activity <- as.character(Data$activity)
for (i in 1:6){
                Data$activity[Data$activity == i] <- as.character(activityLabels[i,2])
                } 

Data$activity <- as.factor(Data$activity)

#########################################################################
#########################################################################
#########################################################################

# take a look on actual names of the dataset
names(Data)

# transform name in the following way
names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

# take a look on actual names of the dataset after transformation
names(Data)

#########################################################################
#########################################################################
#########################################################################

# factorizing subject variable
Data$subject <- as.factor(Data$subject)
Data <- data.table(Data)

# create a new tidy data
new_Data <- aggregate(. ~subject + activity, Data, mean)
new_Data <- new_Data[order(new_Data$subject,new_Data$activity),]
write.table(new_Data, file = "Tidy.txt", row.names = FALSE)

