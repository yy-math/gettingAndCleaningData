if(!require("data.table")){
   install.packages("data.table") 
}
if(!require("dplyr")){
    install.packages("dplyr")
  }
require("data.table")
require("dplyr")

# gets labels
featureLabels <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

# gets training data
trainingSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
trainingActivity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
trainingFeatures <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# gets test data
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
testActivity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
testFeatures <-read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# bind training and test data together
subject <- rbind(trainingSubject, testSubject)
activity <- rbind(trainingActivity, testActivity)
features <- rbind(trainingFeatures, testFeatures)

# assigns labels to tables #poet
colnames(features) <- t(featureLabels[,2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"

# merges all tables
dataFull <- cbind(features, activity, subject)

# finds the columns that have Mean or STD(lulz) in them, then extracts them into new table
hasMeanORStd <- grep(".*Mean.*|.*Std.*", names(dataFull), ignore.case=TRUE)
dataExtract <- dataFull[, c(hasMeanORStd, 562, 563)]

# tidies up activity labels, makes Activity into factor
dataExtract$Activity <- as.character(dataExtract$Activity)
for (i in 1:6){
  dataExtract$Activity[dataExtract$Activity == i] <- as.character(activityLabels[i,2])
}
dataExtract$Activity <- as.factor(dataExtract$Activity)

# makes descriptive variable names
names(dataExtract)<-gsub("Acc", "Accelerometer", names(dataExtract))
names(dataExtract)<-gsub("Gyro", "Gyroscope", names(dataExtract))
names(dataExtract)<-gsub("BodyBody", "Body", names(dataExtract))
names(dataExtract)<-gsub("Mag", "Magnitude", names(dataExtract))
names(dataExtract)<-gsub("^t", "Time", names(dataExtract))
names(dataExtract)<-gsub("^f", "Frequency", names(dataExtract))
names(dataExtract)<-gsub("tBody", "TimeBody", names(dataExtract))
names(dataExtract)<-gsub("-mean()", "Mean", names(dataExtract), ignore.case = TRUE)
names(dataExtract)<-gsub("-std()", "STD", names(dataExtract), ignore.case = TRUE)
names(dataExtract)<-gsub("-freq()", "Frequency", names(dataExtract), ignore.case = TRUE)
names(dataExtract)<-gsub("angle", "Angle", names(dataExtract))
names(dataExtract)<-gsub("gravity", "Gravity", names(dataExtract))

# makes Subject factor
dataExtract$Subject <- as.factor(dataExtract$Subject)

# makes dataExtract into data table
dataExtract <- data.table(dataExtract)

# creates tidyData table with average of each variable for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, dataExtract, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

# writes tidyData.txt
write.table(tidyData, file = "tidyData.txt", row.names = FALSE)
