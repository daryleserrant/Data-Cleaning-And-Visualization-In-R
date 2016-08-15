#---------------------------------------------------------------------------
# 
# This file prepares a tidy data set using data collected from the 
# accelerometers from the Samsung Galaxy S smartphone.
#
# URL: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# Written By: Daryle J. Serrant
#---------------------------------------------------------------------------

library(dplyr)

# Merge the training and test set into one data sets
df.rawData <- rbind(read.table("train\\X_train.txt"),read.table("test\\X_test.txt"))

# Extract the mean and std of each measurement
tBodyAccIdx <- c(1:6)
names(tBodyAccIdx) <- c("mean_tBodyAccX","mean_tBodyAccY","mean_tBodyAccZ","std_tBodyAccX",
                        "std_tBodyAccY","std_tBodyAccZ")

tGravityAccIdx <- c(41:46)
names(tGravityAccIdx) <- c("mean_tGravityAccX","mean_tGravityAccY","mean_tGravityAccZ"
                           ,"std_tGravityAccX","std_tGravityAccY","std_tGravityAccZ")

tBodyAccJerkIdx <- c(81:86)
names(tBodyAccJerkIdx) <- c("mean_tBodyAccJerkX","mean_tBodyAccJerkY","mean_tBodyAccJerkZ",
                            "std_tBodyAccJerkX","std_tBodyAccJerkY","std_tBodyAccJerkZ")

tBodyGyroIdx <- c(121:126)
names(tBodyGyroIdx) <- c("mean_tBodyGyroX","mean_tBodyGyroY","mean_tBodyGyroZ","std_tBodyGyroX",
                         "std_tBodyGyroY","std_tBodyGyroZ")

tBodyGyroJerkIdx <- c(161:166)
names(tBodyGyroJerkIdx) <- c("mean_tBodyGyroJerkX","mean_tBodyGyroJerkY","mean_tBodyGyroJerkZ",
                             "std_tBodyGyroJerkX","std_tBodyGyroJerkY","std_tBodyGyroJerkZ")

tBodyAccMagIdx <- c(201,202)
names(tBodyAccMagIdx) <- c("mean_tBodyAccMag","std_tBodyAccMag")

tGravityAccMagIdx <- c(214,215)
names(tGravityAccMagIdx) <- c("mean_tGravityAccMag","std_tGravityAccMag")

tBodyAccJerkMagIdx <- c(227,228)
names(tBodyAccJerkMagIdx) <- c("mean_tBodyAccJerkMag","std_tBodyAccJerkMag")

tBodyGyroMagIdx <- c(240,241)
names(tBodyGyroMagIdx) <- c("mean_tBodyGyroMag","std_tBodyGyroMag")

tBodyGyroJerkMagIdx <- c(253,254)
names(tBodyGyroJerkMagIdx) <- c("mean_tBodyGyroJerkMag","std_tBodyGyroJerkMag")

fBodyAccIdx <- c(266:271)
names(fBodyAccIdx) <- c("mean_fBodyAccX","mean_fBodyAccY","mean_fBodyAccZ",
                        "std_fBodyAccX", "std_fBodyAccY","std_fBodyAccZ")

fBodyJerkIdx <- c(345:350)
names(fBodyJerkIdx) <- c("mean_fBodyJerkX","mean_fBodyJerkY","mean_fBodyJerkZ",
                         "std_fBodyJerkX","std_fBodyJerkY","std_fBodyJerkZ")

fBodyGyroIdx <- c(424:429)
names(fBodyGyroIdx) <- c("mean_fBodyGyroX","mean_fBodyGyroY","mean_fBodyGyroZ",
                         "std_fBodyGyroX","std_fBodyGyroY","std_fBodyGyroZ")

fBodyAccMagIdx <- c(503,504)
names(fBodyAccMagIdx) <- c("mean_fBodyAccMag","std_fBodyAccMag")

fBodyBodyAccJerkMagIdx <- c(516,517)
names(fBodyBodyAccJerkMagIdx) <- c("mean_fBodyAccJerkMag","std_fBodyAccJerkMag")

fBodyBodyGyroMagIdx <- c(529,530)
names(fBodyBodyGyroMagIdx) <- c("mean_fBodyGyroMag","std_fBodyGyroMag")

fBodyBodyGyroJerkMagIdx <- c(542,543)
names(fBodyBodyGyroJerkMagIdx) <- c("mean_fBodyGyroJerkMag","std_fBodyGyroJerkMag")

colIndexes <- c(tBodyAccIdx,tGravityAccIdx,tBodyAccJerkIdx,tBodyGyroIdx,tBodyGyroJerkIdx,
                tBodyAccMagIdx,tGravityAccMagIdx,tBodyAccJerkMagIdx,tBodyGyroMagIdx,
                tBodyGyroJerkMagIdx,fBodyAccIdx,fBodyJerkIdx,fBodyGyroIdx,fBodyAccMagIdx,
                fBodyBodyAccJerkMagIdx,fBodyBodyGyroMagIdx,fBodyBodyGyroJerkMagIdx)
colNames <- c(names(tBodyAccIdx),names(tGravityAccIdx),names(tBodyAccJerkIdx),names(tBodyGyroIdx),
              names(tBodyGyroJerkIdx),names(tBodyAccMagIdx),names(tGravityAccMagIdx),
              names(tBodyAccJerkMagIdx),names(tBodyGyroMagIdx),names(tBodyGyroJerkMagIdx),
              names(fBodyAccIdx),names(fBodyJerkIdx),names(fBodyGyroIdx),names(fBodyAccMagIdx),
              names(fBodyBodyAccJerkMagIdx),names(fBodyBodyGyroMagIdx),names(fBodyBodyGyroJerkMagIdx))

df.dataExtract <- df.rawData[colIndexes]
names(df.dataExtract) <- colNames

# Read in the activity labels
df.labels <- rbind(read.table("train\\Y_train.txt"),read.table("test\\Y_test.txt"))
df.labels$V1 <- factor(df.labels$V1, levels=1:6,
                          labels=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS",
                                   "SITTING","STANDING","LAYING"))
names(df.labels) <- c("activity")

# Read in the subject labels
df.subjects <- rbind(read.table("train\\subject_train.txt"),read.table("test\\subject_test.txt"))
names(df.subjects) <- c("subject")

# Combine the activty and subject with the data
df.dataExtract <- cbind(df.dataExtract,df.labels)
df.dataExtract <- cbind(df.dataExtract,df.subjects)

df.tidyData <- df.dataExtract %>%
   group_by(subject,activity) %>%
   summarise(avg_tBodyAccX = mean(mean_tBodyAccX), avg_tBodyAccY = mean(mean_tBodyAccY),avg_tBodyAccZ = mean(mean_tBodyAccZ),
             stddev_tBodyAccX = mean(std_tBodyAccX), stddev_tBodyAccY = mean(std_tBodyAccY), stddev_tBodyAccZ = mean(std_tBodyAccZ),
             avg_tGravityAccX = mean(mean_tGravityAccX), avg_tGravityAccY = mean(mean_tGravityAccY), avg_tGravityAccZ = mean(mean_tGravityAccZ),
             stddev_tGravityAccX = mean(std_tGravityAccX), stddev_tGravityAccY = mean(std_tGravityAccY), stddev_tGravityAccZ = mean(std_tGravityAccZ),
             avg_tBodyAccJerkX = mean(mean_tBodyAccJerkX), avg_tBodyAccJerkY = mean(mean_tBodyAccJerkY), avg_tBodyAccJerkZ = mean(mean_tBodyAccJerkZ),
             stddev_tBodyAccJerkX = mean(std_tBodyAccJerkX), stddev_tBodyAccJerkY = mean(std_tBodyAccJerkY), stddev_tBodyAccJerkZ = mean(std_tBodyAccJerkZ),
             avg_tBodyGyroX = mean(mean_tBodyGyroX), avg_tBodyGyroY = mean(mean_tBodyGyroY), avg_tBodyGyroZ = mean(mean_tBodyGyroZ),
             stddev_tBodyGyroX = mean(std_tBodyGyroX), stddev_tBodyGyroY = mean(std_tBodyGyroY), stddev_tBodyGyroZ = mean(std_tBodyGyroZ),
             avg_tBodyGyroJerkX = mean(mean_tBodyGyroJerkX), avg_tBodyGyroJerkY = mean(mean_tBodyGyroJerkY), avg_tBodyGyroJerkZ = mean(mean_tBodyGyroJerkZ),
             stddev_tBodyGyroJerkX = mean(std_tBodyGyroJerkX), stddev_tBodyGyroJerkY = mean(std_tBodyGyroJerkY), stddev_tBodyGyroJerkZ = mean(std_tBodyGyroJerkZ),
             avg_tBodyAccMag = mean(mean_tBodyAccMag), stddev_tBodyAccMag = mean(std_tBodyAccMag),
             avg_tGravityAccMag = mean(mean_tGravityAccMag), stddev_tGravityAccMag = mean(std_tGravityAccMag),
             avg_tBodyAccJerkMag = mean(mean_tBodyAccJerkMag), stddev_tBodyAccJerkMag = mean(std_tBodyAccJerkMag),
             avg_tBodyGyroMag = mean(mean_tBodyGyroMag), stddev_tBodyGyroMag = mean(std_tBodyGyroMag),
             avg_tBodyGyroJerkMag = mean(mean_tBodyGyroJerkMag), stddev_tBodyGyroJerkMag = mean(std_tBodyGyroJerkMag),
             avg_fBodyAccX = mean(mean_fBodyAccX), avg_fBodyAccY = mean(mean_fBodyAccY), avg_fBodyAccZ = mean(mean_fBodyAccZ),
             stddev_fBodyAccX = mean(std_fBodyAccX), stddev_fBodyAccY = mean(std_fBodyAccY), stddev_fBodyAccZ = mean(std_fBodyAccZ),
             avg_fBodyJerkX = mean(mean_fBodyJerkX), avg_fBodyJerkY = mean(mean_fBodyJerkY), avg_fBodyJerkZ = mean(mean_fBodyJerkZ),
             stddev_fBodyJerkX = mean(std_fBodyJerkX), stddev_fBodyJerkY = mean(std_fBodyJerkY), stddev_fBodyJerkZ = mean(std_fBodyJerkZ),
             avg_fBodyGyroX = mean(mean_fBodyGyroX), avg_fBodyGyroY = mean(mean_fBodyGyroY), avg_fBodyGyroZ = mean(mean_fBodyGyroZ),
             stddev_fBodyGyroX = mean(std_fBodyGyroX), stddev_fBodyGyroY = mean(std_fBodyGyroY), stddev_fBodyGyroZ = mean(std_fBodyGyroZ),
             avg_fBodyAccMag = mean(mean_fBodyAccMag), stddev_fBodyAccMag = mean(std_fBodyAccMag),
             avg_fBodyAccJerkMag = mean(mean_fBodyAccJerkMag), stddev_fBodyAccJerkMag = mean(std_fBodyAccJerkMag),
             avg_fBodyGyroMag = mean(mean_fBodyGyroMag), stddev_fBodyGyroMag = mean(std_fBodyGyroMag),
             avg_fBodyGyroJerkMag = mean(mean_fBodyGyroJerkMag), stddev_fBodyGyroJerkMag = mean(std_fBodyGyroJerkMag)
             )

write.table(df.tidyData,"tidyData.txt", row.name=FALSE)