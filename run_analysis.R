library(data.table)
library(dplyr)

# Download the zip file and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- getwd()
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# Load the activity labels
activity_labels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt"),
                         col.names = c("id", "activity"))
# Load and select the necessary features
features <- fread(file.path(path, "UCI HAR Dataset/features.txt"),
                  col.names = c("id", "feature"))
necessary_features_index <- grep("(mean|std)\\(\\)", features[, feature])
necessary_features <- features[necessary_features_index, feature]
necessary_features <- gsub("[()]", '',  necessary_features)

# Load the train data
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"), 
               select = necessary_features_index,
               col.names = necessary_features)
train_activities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt"),
                          col.names = "activity")
train_subject <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"),
                       col.names = "subject_no")
train <- cbind(train_subject, train_activities, train)

# Load test data
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"), 
               select = necessary_features_index,
               col.names = necessary_features)
test_activities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt"),
                          col.names = "activity")
test_subject <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"),
                       col.names = "subject_no")
test <- cbind(test_subject, test_activities, test)

# Merge data
merged <- rbind(train, test)
merged$subject_no <- as.factor(merged[, subject_no])

# Match the activity id to the activity description
merged$activity <- factor(merged[, activity], levels = activity_labels$id,
                          labels = activity_labels$activity)

# Melt and cast the merged data into tidy data
reshaped_data <- data.table::melt(merged, c("subject_no", "activity"))
reshaped_data <- data.table::dcast(reshaped_data, subject_no + activity ~ variable, mean)

# Write data into a txt file
write.table(reshaped_data, file = "tidy_data.txt", quote = FALSE, 
            row.names = FALSE, sep = ',')