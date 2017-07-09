##This is the course project for the Getting and Cleaning Data Coursera course. The R script, run_analysis.R, does the following:
##Download the dataset if it does not already exist in the working directory
##Load the activity and feature info
##Loads both the training and test datasets, keeping only those columns which reflect a mean or standard deviation
##Loads the activity and subject data for each dataset, and merges those columns with the dataset
##Merges the two datasets
##Converts the activity and subject columns into factors
##Creates a tidy dataset that consists of the average (mean) value of each variable for each subject and activity pair.
##The end result is shown in the file tidy.txt

##  Initial Commit ::  Vikas Maggo
##    9th July 2017

# Clean up workspace
rm(list=ls())

## Download and unzip the dataset:
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="libcurl")

##unzip file 
unzip(zipfile="./data/Dataset.zip",exdir="./data")

## Change work directory 
setwd("C:/Users/ervik/Desktop/R Programming/data/UCI HAR Dataset")

# Read in the data from files
features     = read.table('./features.txt'); #imports features.txt
activityType = read.table('./activity_labels.txt'); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt'); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt'); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt'); #imports y_train.txt

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# cCreate the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

testData = cbind(yTest,subjectTest,xTest);
finalData = rbind(trainingData,testData);
colNames  = colnames(finalData); 
logicalVector = grepl(".*activity.*|.*subject.*|.*mean.*|.*std.*",colNames)

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
}    

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

#reate a new table, finalDataNoActivityType without the activityType column
final  = finalData[,names(finalData) != 'activityType'];
# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(final[,names(final) != c('activityId','subjectId')],
                        by=list(activityId=final$activityId,
                        subjectId = final$subjectId),mean);
# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
setwd("C:/Users/ervik/Desktop/R Programming/data/UCI HAR Dataset")
write.table(tidyData, './tidyData.txt',row.name=FALSE,sep="\t")

