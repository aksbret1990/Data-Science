# Description

The script `run_analysis.R`performs the 5 steps described in the project.

* Firstly, all the similar data is merged by `rbind()`.
* After that we choose only those columns with the mean and standard deviation measures from the whole dataset. After extracting these columns, they are assigned correct names, with the help of `features.txt`.
* Since activities are assigned by values 1:6, we take the activity names and IDs from `activity_labels.txt` and they are replaced in the dataset.
* We bind together subject data, activity label data and actual data
* We generate a new dataset with all the average measures for each subject and activity type. 
* The output file is called `final_averages_data.txt`.
* In this script `ddply()` function from the plyr package is used to apply `colMeans()`.

# Variables and Data

* `x_train_data`, `y_train_data`, `x_test_data`, `y_test_data`, `subject_train_data` and `subject_test_data` contain the data from the downloaded files.
* `x_combined_data`, `y_combined_data` and `subject_combined_data` combine the previous datasets.
* `features` has the appropriate names for the `x_combined_data` applied to the column names stored in `mean_and_std_only`.
* Similar is the case for activity names through the `activities` variable.
* `final_data` is formed by merging `subject_combined_data`, `y_combined_data` and `x_combined_data`.
* `final_averages_data` contains the averages which will be stored in a `.txt` file. 

