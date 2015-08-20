library(reshape2)
library(plyr)
library(tidyr)

# load reference tables
activity_labels <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("./features.txt", stringsAsFactors = FALSE)

# load training data
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")

# load test data
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")

# Step #1: Merge the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)

# Step #4: Appropriately label the data set with descriptive variable names.
colnames(subject) <- "subject"
colnames(y) <- "activity"
colnames(X) <- features$V2
colnames(activity_labels) <- c("activity", "activity_desc")

# clean-up: remove the tables created specifically for training and test data
rm(list = grep("train|test",ls(), value = TRUE))

# Step #2: Extract only the measurements on the mean and standard deviation for each measurement.
# choose only mean and standard deviation columns
ixList <- grep("mean\\(|std\\(", features$V2)
selectedX <- X[, ixList]

# Step #3: Use descriptive activity names to name the activities in the data set.
# combine all the different data tables into one data set
# Step #4: Already accomplished, right after step #1. Further, clean up the names.
ylabels <- activity_labels[y$activity, 2]
cboundX <- cbind(activity = ylabels, subject, selectedX)
colnames(cboundX) <- gsub("[-\\(\\)]", "", colnames(cboundX))
colnames(cboundX) <- gsub("mean", "Mean", colnames(cboundX))
colnames(cboundX) <- gsub("std", "Std", colnames(cboundX))

# Step #5: Create a new, independent tidy data set with the average of each variable for
# each activity and each subject.
meanX <- aggregate(. ~ activity + subject, data = cboundX, mean)
write.table(meanX, file = "./meanX.txt", row.names = FALSE)

# read the tidy data back as follows
# meanX <- read.table("./meanX.txt", header = TRUE)