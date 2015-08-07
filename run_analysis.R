# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Load Info data
features_labels <- read.table("features.txt") ## for col names in x_test/x_train
activity_labels <- read.table("activity_labels.txt") ## to label rows in y_test/y_train
feature<- as.character(features_labels[,2])
activity <- as.character(activity_labels[,2])
findmean <- grep( "mean()", feature, fixed = TRUE)
findstd <- grep("std()", feature, fixed = TRUE)
featuresofInterest <- sort(c(findmean, findstd))

featurename<- c()
for (i in (featuresofInterest)) {
  featurename[i]<- feature[i]
  i = i + 1
}
featurename <- featurename[complete.cases(featurename)]
featurename <- gsub("()", "",featurename, fixed = TRUE)

## Load Test data 
subject_test <- read.table("test/subject_test.txt", col.names = c("Subject"))
y_test <- read.table("test/y_test.txt", col.names = c("Activity"))

mycols <- rep("NULL", length(feature))
mycols[featuresofInterest] <- "numeric"
x_test <- read.table("test/X_test.txt", colClasses = mycols)
colnames(x_test) <- featurename

for(i in 1:nrow(y_test)){
        if (y_test[i,1] == 1) y_test[i,1] <-  activity[1]
        else if (y_test[i,1] == 2) y_test[i,1] <-  activity[2]
        else if (y_test[i,1] == 3) y_test[i,1] <-  activity[3]
        else if (y_test[i,1] == 4) y_test[i,1] <-  activity[4]
        else if (y_test[i,1] == 5) y_test[i,1] <-  activity[5]
        else if (y_test[i,1] == 6) y_test[i,1] <-  activity[6]
}

Time<- rep("TEST", nrow(subject_test))
mergedtest <- cbind(Time, subject_test, y_test, x_test)

##Load Train Data
subject_train <- read.table("train/subject_train.txt", col.names = c("Subject"))
y_train <- read.table("train/y_train.txt", col.names = c("Activity"))
x_train <- read.table("train/X_train.txt", colClasses = mycols)
colnames(x_train) <- featurename

for(i in 1:nrow(y_train)){
  if (y_train[i,1] == 1) y_train[i,1] <-  activity[1]
  else if (y_train[i,1] == 2) y_train[i,1] <-  activity[2]
  else if (y_train[i,1] == 3) y_train[i,1] <-  activity[3]
  else if (y_train[i,1] == 4) y_train[i,1] <-  activity[4]
  else if (y_train[i,1] == 5) y_train[i,1] <-  activity[5]
  else if (y_train[i,1] == 6) y_train[i,1] <-  activity[6]
}

Time<- rep("TRAIN", nrow(subject_train))
mergedtrain <- cbind(Time, subject_train, y_train, x_train)
alldata<- rbind(mergedtrain, mergedtest)

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean2 <- c()
for (i in 4:69){
        mean1<- mean2 
        mean2 <-  sapply(split(alldata[,i], alldata[,c('Activity','Subject')]), mean)
        mean2 <- cbind(mean1, mean2)
        i = i+1
}
mean2data <- as.data.frame(mean2)
colnames(mean2data) <- featurename
Activity<- rep((sort(activity)),30)
Subject <- sort(rep(1:30, 6))
tidy <-cbind(Subject, Activity, mean2data)

