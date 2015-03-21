## Read Me
## -------
## 1. Merges the training and the test sets to create one 
##    data set. Files include "subject_text.txt", 
##    "subject_train.txt", "X_test.txt", "X_train.txt", 
##    "y_test.txt"and "y_train.txt".
## 2. Extracts only the measurements on the mean and 
##    standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities 
##    in the data set i.e. WALKING, WALKING_UPSTAIRS, 
##    WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
## 4. Appropriately labels the data set with descriptive 
##    variable names. 
## 5. From the data set in step 4, creates a second, 
##    independent tidy data set with the average of each 
##    variable for each activity and each subject.
## 
## 
## Assumumptions
## -------------
## 1. "dplyr" package is installed and loaded before
##    run_analysis() function is called
## 2. The data files "subject_text.txt", "subject_train.txt" 
##    , "X_test.txt", "X_train.txt", "y_test.txt"and
##    "y_train.txt" are in the same folder as this script



## create function called run_analysis()

run_analysis <- function(name) {
    ## read the training set data
    
    data_train <- read.table(file = "x_train.txt"
                             , header = FALSE
                             , na.strings = ""
                             , stringsAsFactors = F)
    activity_train <- read.table(file = "y_train.txt"
                                 , header = FALSE
                                 , col.names = "Activity"
                                 , na.strings = ""
                                 , stringsAsFactors = F)
    subject_train <- read.table(file = "subject_train.txt"
                                , header = FALSE
                                , col.names = "Subject"
                                , na.strings = ""
                                , stringsAsFactors = F)
    
    ## read the test set data
    
    data_test <- read.table(file = "x_test.txt"
                            , header = FALSE
                            , na.strings = ""
                            , stringsAsFactors = F)
    activity_test <- read.table(file = "y_test.txt"
                                , header = FALSE
                                , col.names = "Activity"
                                , na.strings = ""
                                , stringsAsFactors = F)
    subject_test <- read.table(file = "subject_test.txt"
                               , header = FALSE
                               , col.names = "Subject"
                               , na.strings = ""
                               , stringsAsFactors = F)
    
    ## merge the training and test datasets together
    
    data_merged <- rbind(data_train, data_test)
    activity_merged <- rbind(activity_train, activity_test)
    subject_merged <- rbind(subject_train, subject_test)
    
    ## Extracts only the measurements on the mean and standard
    ## deviation for each measurement from the merged dataset
    
    data_merged_subset <- data_merged[, c(1:6, 41:46, 81:86
                                          , 121:126, 161:166
                                          , 201, 202, 214
                                          , 215, 227, 228
                                          , 240, 241, 253
                                          , 254, 266:271
                                          , 345:350, 424:429
                                          , 503, 504, 516
                                          , 517, 529, 530
                                          , 542, 543)]
    
    ## add the subject, activity columns to the dataset
    
    final_data <- cbind(subject_merged, activity_merged
                        , data_merged_subset)
    
    ## update the Activity column to use descriptive names
    
    final_data$Activity[final_data$Activity==1] <- "WALKING"
    final_data$Activity[final_data$Activity==2] <- "WALKING_UPSTAIRS"
    final_data$Activity[final_data$Activity==3] <- "WALKING_DOWNSTAIRS"
    final_data$Activity[final_data$Activity==4] <- "SITTING"
    final_data$Activity[final_data$Activity==5] <- "STANDING"
    final_data$Activity[final_data$Activity==6] <- "LAYING"
    
    ## label the dataset with descriptive variable names
    
    colnames(final_data) <- c("Subject", "Activity"
                  , "time.Body.Acc.mean.X"
                  , "time.Body.Acc.mean.Y"
                  , "time.Body.Acc.mean.Z"
                  , "time.Body.Acc.std.X"
                  , "time.Body.Acc.std.Y"
                  , "time.Body.Acc.std.Z"
                  , "time.Gravity.Acc.mean.X"
                  , "time.Gravity.Acc.mean.Y"
                  , "time.Gravity.Acc.mean.Z"
                  , "time.Gravity.Acc.std.X"
                  , "time.Gravity.Acc.std.Y"
                  , "time.Gravity.Acc.std.Z"
                  , "time.Body.Acc.Jerk.mean.X"
                  , "time.Body.Acc.Jerk.mean.Y"
                  , "time.Body.Acc.Jerk.mean.Z"
                  , "time.Body.Acc.Jerk.std.X"
                  , "time.Body.Acc.Jerk.std.Y"
                  , "time.Body.Acc.Jerk.std.Z"
                  , "time.Body.Gyro.mean.X"
                  , "time.Body.Gyro.mean.Y"
                  , "time.Body.Gyro.mean.Z"
                  , "time.Body.Gyro.std.X"
                  , "time.Body.Gyro.std.Y"
                  , "time.Body.Gyro.std.Z"
                  , "time.Body.Gyro.Jerk.mean.X"
                  , "time.Body.Gyro.Jerk.mean.Y"
                  , "time.Body.Gyro.Jerk.mean.Z"
                  , "time.Body.Gyro.Jerk.std.X"
                  , "time.Body.Gyro.Jerk.std.Y"
                  , "time.Body.Gyro.Jerk.std.Z"
                  , "time.Body.Acc.Mag.mean"
                  , "time.Body.Acc.Mag.std"
                  , "time.Gravity.Acc.Mag.mean"
                  , "time.Gravity.Acc.Mag.std"
                  , "time.Body.Acc.Jerk.Mag.mean"
                  , "time.Body.Acc.Jerk.Mag.std"
                  , "time.Body.Gyro.Mag.mean"
                  , "time.Body.Gyro.Mag.std"
                  , "time.Body.Gyro.Jerk.Mag.mean"
                  , "time.Body.Gyro.Jerk.Mag.std"
                  , "frequency.Body.Acc.mean.X"
                  , "frequency.Body.Acc.mean.Y"
                  , "frequency.Body.Acc.mean.Z"
                  , "frequency.Body.Acc.std.X"
                  , "frequency.Body.Acc.std.Y"
                  , "frequency.Body.Acc.std.Z"
                  , "frequency.Body.Acc.Jerk.mean.X"
                  , "frequency.Body.Acc.Jerk.mean.Y"
                  , "frequency.Body.Acc.Jerk.mean.Z"
                  , "frequency.Body.Acc.Jerk.std.X"
                  , "frequency.Body.Acc.Jerk.std.Y"
                  , "frequency.Body.Acc.Jerk.std.Z"
                  , "frequency.Body.Gyro.mean.X"
                  , "frequency.Body.Gyro.mean.Y"
                  , "frequency.Body.Gyro.mean.Z"
                  , "frequency.Body.Gyro.std.X"
                  , "frequency.Body.Gyro.std.Y"
                  , "frequency.Body.Gyro.std.Z"
                  , "frequency.Body.Acc.Mag.mean"
                  , "frequency.Body.Acc.Mag.std"
                  , "frequency.Body.Body.Acc.Jerk.Mag.mean"
                  , "frequency.Body.Body.Acc.Jerk.Mag.std"
                  , "frequency.Body.Body.Gyro.Mag.mean"
                  , "frequency.Body.Body.Gyro.Mag.std"
                  , "frequency.Body.Body.Gyro.Jerk.Mag.mean"
                  , "frequency.Body.Body.Gyro.Jerk.Mag.std")
    
    ## 5. From the data set in step 4, creates a second, 
    ##    independent tidy data set with the average of each 
    ##    variable for each activity and each subject.
    
    final_data_group <- group_by(final_data, Subject, Activity)
    final_data_summarized <- summarize(final_data_group
               , mean(time.Body.Acc.mean.X)
               , mean(time.Body.Acc.mean.Y)
               , mean(time.Body.Acc.mean.Z)
               , mean(time.Body.Acc.std.X)
               , mean(time.Body.Acc.std.Y)
               , mean(time.Body.Acc.std.Z)
               , mean(time.Gravity.Acc.mean.X)
               , mean(time.Gravity.Acc.mean.Y)
               , mean(time.Gravity.Acc.mean.Z)
               , mean(time.Gravity.Acc.std.X)
               , mean(time.Gravity.Acc.std.Y)
               , mean(time.Gravity.Acc.std.Z)
               , mean(time.Body.Acc.Jerk.mean.X)
               , mean(time.Body.Acc.Jerk.mean.Y)
               , mean(time.Body.Acc.Jerk.mean.Z)
               , mean(time.Body.Acc.Jerk.std.X)
               , mean(time.Body.Acc.Jerk.std.Y)
               , mean(time.Body.Acc.Jerk.std.Z)
               , mean(time.Body.Gyro.mean.X)
               , mean(time.Body.Gyro.mean.Y)
               , mean(time.Body.Gyro.mean.Z)
               , mean(time.Body.Gyro.std.X)
               , mean(time.Body.Gyro.std.Y)
               , mean(time.Body.Gyro.std.Z)
               , mean(time.Body.Gyro.Jerk.mean.X)
               , mean(time.Body.Gyro.Jerk.mean.Y)
               , mean(time.Body.Gyro.Jerk.mean.Z)
               , mean(time.Body.Gyro.Jerk.std.X)
               , mean(time.Body.Gyro.Jerk.std.Y)
               , mean(time.Body.Gyro.Jerk.std.Z)
               , mean(time.Body.Acc.Mag.mean)
               , mean(time.Body.Acc.Mag.std)
               , mean(time.Gravity.Acc.Mag.mean)
               , mean(time.Gravity.Acc.Mag.std)
               , mean(time.Body.Acc.Jerk.Mag.mean)
               , mean(time.Body.Acc.Jerk.Mag.std)
               , mean(time.Body.Gyro.Mag.mean)
               , mean(time.Body.Gyro.Mag.std)
               , mean(time.Body.Gyro.Jerk.Mag.mean)
               , mean(time.Body.Gyro.Jerk.Mag.std)
               , mean(frequency.Body.Acc.mean.X)
               , mean(frequency.Body.Acc.mean.Y)
               , mean(frequency.Body.Acc.mean.Z)
               , mean(frequency.Body.Acc.std.X)
               , mean(frequency.Body.Acc.std.Y)
               , mean(frequency.Body.Acc.std.Z)
               , mean(frequency.Body.Acc.Jerk.mean.X)
               , mean(frequency.Body.Acc.Jerk.mean.Y)
               , mean(frequency.Body.Acc.Jerk.mean.Z)
               , mean(frequency.Body.Acc.Jerk.std.X)
               , mean(frequency.Body.Acc.Jerk.std.Y)
               , mean(frequency.Body.Acc.Jerk.std.Z)
               , mean(frequency.Body.Gyro.mean.X)
               , mean(frequency.Body.Gyro.mean.Y)
               , mean(frequency.Body.Gyro.mean.Z)
               , mean(frequency.Body.Gyro.std.X)
               , mean(frequency.Body.Gyro.std.Y)
               , mean(frequency.Body.Gyro.std.Z)
               , mean(frequency.Body.Acc.Mag.mean)
               , mean(frequency.Body.Acc.Mag.std)
               , mean(frequency.Body.Body.Acc.Jerk.Mag.mean)
               , mean(frequency.Body.Body.Acc.Jerk.Mag.std)
               , mean(frequency.Body.Body.Gyro.Mag.mean)
               , mean(frequency.Body.Body.Gyro.Mag.std)
               , mean(frequency.Body.Body.Gyro.Jerk.Mag.mean)
               , mean(frequency.Body.Body.Gyro.Jerk.Mag.std))
    
    ## update the column names to remove the "mean()" text
    
    colnames(final_data_summarized) <- c("Subject", "Activity"
                 , "time.Body.Acc.mean.X"
                 , "time.Body.Acc.mean.Y"
                 , "time.Body.Acc.mean.Z"
                 , "time.Body.Acc.std.X"
                 , "time.Body.Acc.std.Y"
                 , "time.Body.Acc.std.Z"
                 , "time.Gravity.Acc.mean.X"
                 , "time.Gravity.Acc.mean.Y"
                 , "time.Gravity.Acc.mean.Z"
                 , "time.Gravity.Acc.std.X"
                 , "time.Gravity.Acc.std.Y"
                 , "time.Gravity.Acc.std.Z"
                 , "time.Body.Acc.Jerk.mean.X"
                 , "time.Body.Acc.Jerk.mean.Y"
                 , "time.Body.Acc.Jerk.mean.Z"
                 , "time.Body.Acc.Jerk.std.X"
                 , "time.Body.Acc.Jerk.std.Y"
                 , "time.Body.Acc.Jerk.std.Z"
                 , "time.Body.Gyro.mean.X"
                 , "time.Body.Gyro.mean.Y"
                 , "time.Body.Gyro.mean.Z"
                 , "time.Body.Gyro.std.X"
                 , "time.Body.Gyro.std.Y"
                 , "time.Body.Gyro.std.Z"
                 , "time.Body.Gyro.Jerk.mean.X"
                 , "time.Body.Gyro.Jerk.mean.Y"
                 , "time.Body.Gyro.Jerk.mean.Z"
                 , "time.Body.Gyro.Jerk.std.X"
                 , "time.Body.Gyro.Jerk.std.Y"
                 , "time.Body.Gyro.Jerk.std.Z"
                 , "time.Body.Acc.Mag.mean"
                 , "time.Body.Acc.Mag.std"
                 , "time.Gravity.Acc.Mag.mean"
                 , "time.Gravity.Acc.Mag.std"
                 , "time.Body.Acc.Jerk.Mag.mean"
                 , "time.Body.Acc.Jerk.Mag.std"
                 , "time.Body.Gyro.Mag.mean"
                 , "time.Body.Gyro.Mag.std"
                 , "time.Body.Gyro.Jerk.Mag.mean"
                 , "time.Body.Gyro.Jerk.Mag.std"
                 , "frequency.Body.Acc.mean.X"
                 , "frequency.Body.Acc.mean.Y"
                 , "frequency.Body.Acc.mean.Z"
                 , "frequency.Body.Acc.std.X"
                 , "frequency.Body.Acc.std.Y"
                 , "frequency.Body.Acc.std.Z"
                 , "frequency.Body.Acc.Jerk.mean.X"
                 , "frequency.Body.Acc.Jerk.mean.Y"
                 , "frequency.Body.Acc.Jerk.mean.Z"
                 , "frequency.Body.Acc.Jerk.stdX"
                 , "frequency.Body.Acc.Jerk.stdY"
                 , "frequency.Body.Acc.Jerk.stdZ"
                 , "frequency.Body.Gyro.mean.X"
                 , "frequency.Body.Gyro.mean.Y"
                 , "frequency.Body.Gyro.mean.Z"
                 , "frequency.Body.Gyro.std.X"
                 , "frequency.Body.Gyro.std.Y"
                 , "frequency.Body.Gyro.std.Z"
                 , "frequency.Body.Acc.Mag.mean"
                 , "frequency.Body.Acc.Mag.std"
                 , "frequency.Body.Body.Acc.Jerk.Mag.mean"
                 , "frequency.Body.Body.Gyro.Mag.mean"
                 , "frequency.Body.Body.Gyro.Mag.std"
                 , "frequency.Body.Body.Gyro.Jerk.Mag.mean")
    
    ## write the cleaned up data into the file "final_data.txt"
    
    write.table(final_data_summarized, file="final_data.txt"
                , row.names=FALSE, na="", sep=",")
}