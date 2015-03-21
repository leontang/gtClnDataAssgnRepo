## This is the repository for the assignment in getting and cleaning data module

The R script "run_analysis.R" does the following:
- Merges the training and the test sets to create one data set. Files include "subject_text.txt", "subject_train.txt", "X_test.txt", "X_train.txt", "y_test.txt"and "y_train.txt".
- Extracts only the measurements on the mean and standard deviation for each measurement. 
- Uses descriptive activity names to name the activities in the data set i.e. WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
- Appropriately labels the data set with descriptive variable names. 
- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The R script generates an output file "final_data.txt"

For more information on the variables in the output file, refer to the code book.pdf in the same repository


Assumptions
=============
- "dplyr" package is installed and loaded before run_analysis() function is called
- The data files "subject_text.txt", "subject_train.txt", "X_test.txt", "X_train.txt", "y_test.txt"and "y_train.txt" are in the same folder as this script


Acknowledgement
===============
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
