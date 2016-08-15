                                                 CODE BOOK
												 
subject                 : Subject who performed the activity. Values ranges from 1 to 30.
activity                : The activity performed.
                            1  WALKING
		                    2  WALKING_UPSTAIRS
		                    3  WALKING_DOWNSTAIRS
		                    4  SITTING
		                    5  STANDING
		                    6  LAYING
avg_tBodyAccX           : Average tBodyAcc-X signal
avg_tBodyAccY           : Average tBodyAcc-Y signal
avg_tBodyAccZ           : Average tBodyAcc-Z signal
stddev_tBodyAccX        : Average standard deviation tBodyAcc-X signal
stddev_tBodyAccY        : Average standard deviation tBodyAcc-Y signal
stddev_tBodyAccZ        : Average standard deviation tBodyAcc-Z signal
avg_tGravityAccX        : Average tGravityAcc-X signal
avg_tGravityAccY        : Average tGravityAcc-Y signal
avg_tGravityAccZ        : Average tGravityAcc-Z signal
stddev_tGravityAccX     : Average standard deviation tGravityAcc-X signal
stddev_tGravityAccY     : Average standard deviation tGravityAcc-Y signal
sttdev_tGravityAccZ     : Average standard deviation tGravityAcc-Z signal
avg_tBodyAccJerkX       : Average tBodyAccJerk-X signal
avg_tBodyAccJerkY       : Average tBodyAccJerk-Y signal
avg_tBodyAccJerkZ       : Average tBodyAccJerk-Z signal
stddev_tBodyAccJerkX    : Average standard deviation tBodyAccJerk-X signal
stddev_tBodyAccJerkY    : Average standard deviation tBodyAccJerk-Y signal
stddev_tBodyAccJerkZ    : Average standard deviation tBodyAccJerk-Z signal
avg_tBodyGyroX          : Average tBodyGyro-X signal
avg_tBodyGyroY          : Average tBodyGyro-Y signal
avg_tBodyGyroZ          : Average tBodyGyro-Z signal
stddev_tBodyGyroX       : Average standard deviation tBodyGyro-X signal
stddev_tBodyGyroY       : Average standard deviation tBodyGyro-Y signal
stddev_tBodyGyroZ       : Average standard deviation tBodyGyro-Z signal
avg_tBodyGyroJerkX      : Average tBodyGyroJerk-X signal
avg_tBodyGyroJerkY      : Average tBodyGyroJerk-Y signal
avg_tBodyGyroJerkZ      : Average tBodyGyroJerk-Z signal
stddev_tBodyGyroJerkX   : Average standard deviation tBodyGyroJerk-X signal
stddev_tBodyGyroJerkY   : Average standard deviation tBodyGyroJerk-Y signal
stddev_tBodyGyroJerkZ   : Average standard deviation tBodyGyroJerk-Z signal
avg_tBodyAccMag         : Average tBodyAccMag signal
stddev_tBodyAccMag      : Average standard deviation tBodyAccMag signal
avg_tGravityAccMag      : Average tGravityAccMag signal
stddev_tGravityAccMag   : Average standard deviation tGravityAccMag signal
avg_tBodyAccJerkMag     : Average tBodyAccJerkMag signal
stddev_tBodyAccJerkMag  : Average standard deviation tBodyAccJerkMag signal
avg_tBodyGyroMag        : Average tBodyGyroMag signal
stddev_tBodyGyroMag     : Average standard deviation tBodyGyroMag signal
avg_tBodyGyroJerkMag    : Average tBodyGyroJerkMag signal
stddev_tBodyGyroJerkMag : Average standard deviation tBodyGyroJerkMag signal
avg_fBodyAccX           : Average fBodyAcc-X signal
avg_fBodyAccY           : Average fBodyAcc-Y signal
avg_fBodyAccZ           : Average fBodyAcc-Z signal
stddev_fBodyAccX        : Average standard deviation fBodyAcc-X signal
stddev_fBodyAccY        : Average standard deviation fBodyAcc-Y signal
stddev_fBodyAccZ        : Average standard deviation fBodyAcc-Z signal
avg_fBodyJerkX          : Average fBodyJerk-X signal
avg_fBodyJerkY          : Average fBodyJerk-Y signal
avg_fBodyJerkZ          : Average fBodyJerk-Z signal
stddev_fBodyJerkX       : Average standard deviation fBodyJerk-X signal
stddev_fBodyJerkY       : Average standard deviation fBodyJerk-Y signal
stddev_fBodyJerkZ       : Average standard deviation fBodyJerk-Z signal
avg_fBodyGyroX          : Average fBodyGyro-X signal
avg_fBodyGyroY          : Average fBodyGyro-Y signal
avg_fBodyGyroZ          : Average fBodyGyro-Z signal
stddev_fBodyGyroX       : Average standard deviation fBodyGyro-X signal
stddev_fBodyGyroY       : Average standard deviation fBodyGyro-Y signal
stddev_fBodyGyroZ       : Average standard deviation fBodyGyro-Z signal
avg_fBodyAccMag         : Average fBodyAccMag signal
stddev_fBodyAccMag      : Average standard deviation fBodyAccMag signal
avg_fBodyAccJerkMag     : Average fBodyAccJerkMag signal
stddev_fBodyAccJerkMag  : Average standard deviation fBodyAccJerkMag signal
avg_fBodyGyroMag        : Average fBodyGyroMag signal
stddev_fBodyGyroMag     : Average standard deviation fBodyGyroMag signal
avg_fBodyGyroJerkMag    : Average fBodyGyroJerkMag signal
stddev_fBodyGyroJerkMag : Average standard deviation fBodyGyroJerkMag signal

Dataset is derived from the Human Activity Recognition Using Smartphones Dataset. The following transformations were performed on the dataset:
  1. Merge dataset in train/X_train.txt and test/X_train.txt into one dataset
  2. Create a new data frame with the mean and standard deviation of the following fields from the dataset created in [1]:
         tBodyAcc-XYZ
         tGravityAcc-XYZ
         tBodyAccJerk-XYZ
         tBodyGyro-XYZ
         tBodyGyroJerk-XYZ
         tBodyAccMag
         tGravityAccMag
         tBodyAccJerkMag
         tBodyGyroMag
         tBodyGyroJerkMag
         fBodyAcc-XYZ
         fBodyAccJerk-XYZ
         fBodyGyro-XYZ
         fBodyAccMag
         fBodyAccJerkMag
         fBodyGyroMag
         fBodyGyroJerkMag
  3. Merge dataset in train/subject_train.txt and test/subject_test.txt into one dataset
  4. Merge dataset in train/Y_train.txt and test/Y_train.txt into one dataset
  5. Merge the datasets created in [3], [4], and [2] into one dataset.
  6. Create another dataset by computing the average of each field in [5] grouped by the subject and the activity.