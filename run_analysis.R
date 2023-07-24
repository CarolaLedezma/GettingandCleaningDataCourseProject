if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

require("data.table")
require("reshape2")

library(data.table)
library(reshape2)

################################################################
# Get the data
# Download the file and put the file in the data folder

if(!file.exists("./Data")){dir.create("./Data")}
fileUrl        <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Data/Dataset.zip",method="wininet")

unzip(zipfile="./Data/Dataset.zip",exdir="./Data")

################################################################
# GOAL 1 Merges the training and the test sets to create one data set.
################################################################
# Load and process train
train.x        <- read.table("./Data/UCI HAR Dataset/train/X_train.txt")
train.y        <- read.table("./Data/UCI HAR Dataset/train/y_train.txt")
train.s        <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt")


# Load and process test
test.x        <- read.table("./Data/UCI HAR Dataset/test/X_test.txt")
test.y        <- read.table("./Data/UCI HAR Dataset/test/y_test.txt")
test.s        <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt")


#############################################################

#Reading feature vector:

features     <- read.table('./Data/UCI HAR Dataset/features.txt')

#Reading activity labels:

activityLabels = read.table('./Data/UCI HAR Dataset/activity_labels.txt')


#Assigning column names:

colnames(train.x) <- features[,2]
colnames(train.y) <-"activityId"
colnames(train.s) <- "subjectId"

colnames(test.x)  <- features[,2] 
colnames(test.y)  <- "activityId"
colnames(test.s)  <- "subjectId"



colnames(activityLabels) <- c('activityId','activityType')

############################################################
# Bind the rows for each of the data sets together
data.train        <- cbind(train.y,train.s,train.x)
data.test         <- cbind(test.y, test.s,test.x)
allData           <- rbind(data.train, data.test)


#dim(allData)

################################################################
# GOAL 2 Extracts only the measurements on the mean and standard deviation for each measurement.
################################################################

# Reading column names:

colNames <- colnames(allData)


#Create vector for defining ID, mean and standard deviation:

mean.std <- (grepl("activityId" , colNames) | 
               grepl("subjectId" , colNames) | 
               grepl("mean.." , colNames) | 
               grepl("std.." , colNames) 
)

#Making nessesary subset from allData:

mean.std.f <- allData[ , mean.std == TRUE]



################################################################
#GOAL 3 Uses descriptive activity names to name the activities in the data set
################################################################

activities.names <- merge(mean.std.f, activityLabels,
                          by='activityId',
                          all.x=TRUE)


################################################################
#GOAL 4  Appropriately labels the data set with descriptive variable names. 
################################################################

#Done in the previous steps

################################################################
#GOAL 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
################################################################

TidySet <- aggregate(. ~subjectId + activityId, activities.names, mean)

warnings()


TidySet <- TidySet[order(TidySet$subjectId, TidySet$activityId),]

#Writing second tidy data set in txt file

write.table(TidySet, "TidySet.txt", row.name=FALSE)



