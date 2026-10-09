## I used dplyr package for this purpose
library(dplyr)

## before completing the task, the data was downloaded
zip_file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_file <- "UCI_HAR_Dataset.zip"

if(!file.exists(zip_file)) {
  download.file(zip_file_url, destfile = zip_file, method = "curl")
}

## downloaded file needs to be unzipped
if(!file.exists("UCI HAR Dataset")) {
  unzip(zip_file)
}

## Since “UCI HAR Dataset” is a new folder containing various types of information, it should be defined as a separate path.
path <- "UCI HAR Dataset"

## The data from this folder is now being imported and assigned to the corresponding names
## In addition, column names are assigned in this step to provide a clearer overview
features <- read.table(file.path(path, "features.txt"), col.names = c("id", "feature_name"))
activity_labels <- read.table(file.path(path, "activity_labels.txt"), col.names = c("activity_id", "activity_name"))

# Task1: Merges the training and the test sets to create one data set.
## This is necessary, as the data is stored seperatly in the given folder. The measured values (here, x), the activities (here, y), and the subject IDs (here, subject) are merged using the `rbind` command and then combined column by column into a single, combined dataset using `cbind`.
##first the test-data is imported and assigned to the corresponding names
x_test <- read.table(file.path(path, "test", "X_test.txt"))
y_test <- read.table(file.path(path, "test", "Y_test.txt"), col.names = "activity_id")
subject_test <- read.table(file.path(path, "test", "subject_test.txt"), col.names = "subject")

## now, the training-data is imported and assigned to the corresponding names
x_train <- read.table(file.path(path, "train", "X_train.txt"))
y_train <- read.table(file.path(path, "train", "Y_train.txt"), col.names = "activity_id")
subject_train <- read.table(file.path(path, "train", "subject_train.txt"), col.names = "subject")

## the now loaded training and test datasets are merged in the next step
x_combined <- rbind(x_train, x_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)

## the column name for the values is set
colnames(x_combined) <- features$feature_name

## Create the entire dataset using column linking
complete_data_all <- cbind(subject_combined, y_combined, x_combined)

# Task2: Extracts only the measurements on the mean and standard deviation for each measurement. 
## The task requires you to specifically search for variables whose names include functions such as “mean” or “std.”
selected_values <- grep("mean\\(\\) | std\\(\\)", features$feature_name, ignore.case = TRUE)

## the data set is now filtered so only the needed information in kept. 
extracted_data <- complete_data_all[, c(1, 2, selected_values +2)]

# Task3: Uses descriptive activity names to name the activities in the data set
## the activity id's should be replaced by logic names instead of the number 1 to 6
extracted_data$activity_id <- factor(extracted_data$activity_id, levels = activity_labels$activity_id, labels = activity_labels$activity_name)

## the activity_id column is calles activity now
colnames(extracted_data)[2] <- "activity"

# Task4: Appropriately labels the data set with descriptive variable names.
col_names <- colnmaes(extracted_data)
col_names <- gsub("-mean\\(\\)", "Mean", col_names)
col_names <- gsub("-std\\(\\)", "STD", col_names)
col_names <- gsub("^t", "Time", col_names)
col_names <- gsub("Gyro", "Gyroscope", col_names)
col_names <- gsub("Mag", "Magnitude", col_names)
col_names <- gsub("-freq\\(\\)", "Frequency", col_names)
col_names <- gsub("^f", "Frequency", col_names)
col_names <- gsub("Acc", "Accelerometer", col_names)
colnames(extracted_data) <- col_names

# Task5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
clean_data <- extracted_data %>%
  +     group_by(subject, activity) %>%
  +     summarize_all(mean)

## results are written in a new file
write.table(clean_data, "clean_data.txt", row.name = FALSE)