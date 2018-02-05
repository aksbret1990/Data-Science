library(plyr)

#1.Merges the training and test sets to create one data set

x_train_data <- read.table("train/X_train.txt")
y_train_data <- read.table("train/y_train.txt")
subject_train_data <- read.table("train/subject_train.txt")

x_test_data <- read.table("test/X_test.txt")
y_test_data <- read.table("test/y_test.txt")
subject_test_data <- read.table("test/subject_test.txt")

# create combined 'x' data set by combining test and train data
x_combined_data <- rbind(x_train_data, x_test_data)

# create combined 'y' data set by combining test and train data
y_combined_data <- rbind(y_train_data, y_test_data)

# create combined 'subject' data set by combining test and train data
subject_combined_data <- rbind(subject_train_data, subject_test_data)

#2.Extract only the measurements on the mean and standard deviation for each measurement

features <- read.table("features.txt")

#extract columns with mean() or std()
mean_and_std_only <- grep("-(mean|std)\\(\\)", features[, 2])

#get only desired columns
x_combined_data <- x_combined_data[, mean_and_std_only]

#rename columns
names(x_combined_data) <- features[mean_and_std_only, 2]

#3.Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")

#modify to correct activity names
y_combined_data[, 1] <- activities[y_combined_data[, 1], 2]

#change column name
names(y_combined_data) <- "activity"

#4.Appropriately labels the data set with descriptive variable names

#change column name
names(subject_combined_data) <- "subject"

# bind together
final_data <- cbind(subject_combined_data,y_combined_data,x_combined_data)

#5.From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

final_averages_data <- ddply(final_data, .(subject, activity), function(x) colMeans(x[, 3:68]))

write.table(final_averages_data, "final_averages_data.txt", row.name=FALSE)