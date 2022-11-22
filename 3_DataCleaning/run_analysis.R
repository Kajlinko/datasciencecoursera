## load packages

library(dplyr)

## load data

subjecttest <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/test/subject_test.txt", sep = "\t"
  )
subjecttrain <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/train/subject_train.txt", sep = "\t"
)
xtest <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/test/X_test.txt", sep = "\t"
  )
xtrain <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/train/X_train.txt", sep = "\t"
)
features <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/features.txt", sep = "\n"
  )

## import activities data and change to labelled factor variable

activitylabels <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/activity_labels.txt", sep = "\n"
  )
activitylabels <- activitylabels[,1]
activitylabels <- gsub("([0-9 ])", "", activitylabels)
activitylabels <- gsub("([_])", "", activitylabels)
activitylabels <- tolower(activitylabels)

# TEST DATA
## Tidy activities data for test set

activities <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/test/y_test.txt", sep = "\t"
  )
activities <- unlist(activities)
activities <- factor(
  as.character(activities),
  levels = c("1","2","3","4","5","6"),
  labels = activitylabels
  )

## Select mean and standard deviation from xtest elements

vector <- grepl(
  "[Mm]ean\\(\\)", features[,1]) | grepl("[Ss]td\\(\\)", features[,1]
  )
featuresvect <- features[,1][vector]

cleanup <- function(data, column = 1) {
  df <- data.frame(matrix(nrow = 0, ncol = length(featuresvect)))
  colnames(df) <- featuresvect
  for(i in 1:nrow(data)){
    list <- strsplit(data[i,column], split = " ") 
    vect <- unlist(list)
    vect <- vect[!vect == ""]    
    vect <- as.numeric(vect[vector])
    df[i,] <- vect
  }
  assign("xt", df, pos = ".GlobalEnv")
}

cleanup(xtest)

## Redefine xtest to only include desired variables

xtest <- xt

## Bind subject, activity and mean and standard deviation data

df <- cbind(subjecttest, activities)
alltest <- cbind(df, xtest)

## TRAIN DATA
## Tidy activities data for test set

activities <- read.table(
  "./3_DataCleaning/UCI HAR Dataset/train/y_train.txt", sep = "\t"
)
activities <- unlist(activities)
activities <- factor(
  as.character(activities),
  levels = c("1","2","3","4","5","6"),
  labels = activitylabels
)

## Select mean and standard deviation from xtest elements

cleanup(xtrain)

## Redefine xtest to only include desired variables

xtrain <- xt

## Bind subject, activity and mean and standard deviation data

df <- cbind(subjecttrain, activities)
alltrain <- cbind(df, xtrain)

#COMBINE DATA
## Bind test and train data

alldata <- rbind(alltest,alltrain)