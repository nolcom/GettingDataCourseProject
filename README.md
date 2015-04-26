# GettingDataCourseProject
Course Project for Getting Data. 

#This is the R script for the Course Project - "Getting and Cleaning Data"
##You should create one R script called run_analysis.R that does the following. 

##1. Merges the training and the test sets to create one data set.##
####################################################################

#Packages needed
library(plyr)
library(reshape2)

#This part get all the test data.
xtest    <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/test/X_test.txt")
ytestdata    <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/test/y_test.txt")         
testsubjects <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/test/subject_test.txt") 

#This part get all the train data.
xtraindata    <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/train/X_train.txt")       
ytraindata    <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/train/y_train.txt")      
trainsubjects <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/train/subject_train.txt")

#Here I merge "x" data - activity
ActivityData <- rbind(xtest,xtrain)


#Here I merge "y" data - labels
LabelSets    <- rbind(ytest,ytrain)


#Here I merge "subject" data - subjects
SubjectCodes <- rbind(testsubjects,trainsubjects)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

#we need to understand the features names to check which of them are std and mean.
featurenames   <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/features.txt")

#Here we look only to the ones that includes mean or std on the name.
meanandstddevfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)

#We check this values and put it into a new data table.
filtereddata <- ActivityData[, which(meanandstddevfeatures == TRUE)]

#3. Uses descriptive activity names to name the activities in the data set

featurenames   <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/features.txt")

meanandstddevfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)

filtereddata <- ActivityData[, which(meanandstddevfeatures == TRUE)]

activityLabels  <- read.table("C:/Users/Adolfo/Desktop/DATA SCIENCE/3. Cleaning Data/Data/project/UCI HAR Dataset/activity_labels.txt")

activity <- as.factor(LabelSets$V1)

levels(activity) <- activityLabels$V2

subject <- as.factor(SubjectCodes$V1)

filtereddata <- cbind(subject,activity,filtereddata)


filteredfeatures <- (cbind(featurenames,meanandstddevfeatures)[meanandstddevfeatures==TRUE,])$V2

cleaner <- function(featurename) {
  tolower(gsub("(\\(|\\)|\\-)","",featurename))
}
filteredfeatures <- sapply(filteredfeatures,cleaner)

names(filtereddata)[3:ncol(filtereddata)] <- filteredfeatures

write.table(filtereddata, "dataset1.txt", sep="\t")

library(reshape2)

m <- melt(filtereddata,id.vars=c("subject","activity"))

tidyset <- dcast(m,subject + activity ~ variable,mean)

write.table(tidyset, "dataset2.txt", sep="\t",row.name=FALSE)



#4. Appropriately labels the data set with descriptive variable names. 

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

