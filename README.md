---
title: "README"
author: "Andre Coleman"
date: "1/15/2021"
output:
  pdf_document: default
  html_document: default
---
# Background
This script cleans the UCI HAR dataset per the instructions within the Coursera - Getting and Cleaning Data Course Project.
Inputs: UCI HAR Dataset (uncompressed)
Outputs: TXT file of the cleaned data (test/training sets merged, mean and std extracted, descriptive activity names added, labels added and creation of a second dataset with the average for each activity/subject)

# Reading in Data Sets

The following code block reads in all of the UCI HAR data sets pertinent to the analysis by changing the active directory and reading in the files via read.fwf.

```{r}
## Read in all datasets
basewd <- getwd() # Recording base directory
w <- c(rep(16,times=561)) # Building width vector

setwd(paste0(getwd(), "/UCI HAR Dataset/test")) # Set working directory (test)

xtest <- read.fwf("X_test.txt", w)
ytest <- read.fwf("y_test.txt", 1)
subtest <- read.fwf("subject_test.txt", 2)

setwd(basewd) # Set working directory (base)
setwd(paste0(getwd(), "/UCI HAR Dataset/train")) # Set working directory (train)

xtrain <- read.fwf("X_train.txt", w)
ytrain <- read.fwf("y_train.txt", 1)
subtrain <- read.fwf("subject_train.txt", 2)

setwd(basewd) # Set working directory (base)
```

# Creating Summary Table (Step 1)
The following code block collates the subject, y and X data sets together for the train and test data and then collates those two data sets into one data set.

```{r}
## Creating summary tables and merging
test <- cbind(subtest, ytest, xtest) # Create summary table (test)
train <- cbind(subtrain, ytrain, xtrain) # Create summary table (train)

testtrain <- rbind(test, train) # Merged table (Step 1)
```

# Renaming Headers and Extracting Mean/Standard Deviation
The following code block reads in the feature list in order to rename the collated data set header. Following the renaming, the code block extracts the mean() and std() columns and places those columns into another data frame.

```{r}
## Renaming headers and extracting mean and std
setwd(paste0(getwd(), "/UCI HAR Dataset")) # Set working directory (top level)
features <- read.delim("features.txt", header=FALSE) # Reading in features
features <- rbind(c("Subject"), c("Activity"), features) # Add addtl columns
features <- as.vector(features$V1) # Converting to vector
setwd(basewd) # Set working directory (base)
colnames(testtrain) <- features # Rename columns

testtrainextract <- testtrain[,1:2]
namehold <- rbind(c("Subject"), c("Activity"))

for (i in 3:ncol(testtrain)) {
        test <- grepl("mean()|std()", colnames(testtrain[i])) # Find mean & std
        if (test == TRUE) {
                testtrainextract <- cbind(testtrainextract, testtrain[,i])
                namehold <- rbind(namehold,colnames(testtrain[i]))
        }
}
namehold <- as.vector(namehold) # Converting to vector
colnames(testtrainextract) <- namehold # Column extract (Step 2)
```

# Naming Activities
The following code block performs a series a modification steps to tidy up the headers and activity labels including removing numbers, underscores and adding appropriate spacing. The code then modifies abbreviations and in the process corrects all spacing to one space.

```{r}
## Naming activities
setwd(paste0(getwd(), "/UCI HAR Dataset")) # Set working directory (top level)
alabels <- read.delim("activity_labels.txt", header=FALSE) # Read activity labels
setwd(basewd) # Set working directory (base)

# Converting activity labels
alabels <- as.vector(alabels$V1)
alabels <- gsub("^[1-6] ","",alabels) # Remove numbers
alabels <- gsub("_"," ",alabels) # Remove _

# Adding activity labels
for (i in 1:nrow(testtrainextract)) {
        hold <- as.numeric(testtrainextract[i,"Activity"])
        testtrainextract[i,"Activity"] <- alabels[hold] # Set label (Step 3)
}

## Feature rename
extractname <- colnames(testtrainextract)
extractname <- gsub("^(.*)[0-9] ","", extractname) # Remove numbers
extractname <- gsub("^t","Time ", extractname) # Change t
extractname <- gsub("^f","Frequency ", extractname) # Change f
extractname <- gsub("Acc"," Acceleration ", extractname) # Change Acc
extractname <- gsub("Gyro"," Gyroscope ", extractname) # Change Gyro
extractname <- gsub("Mag"," Magnitude ", extractname) # Change Mag
extractname <- gsub("meanFreq", " Frequency Mean ", extractname) # Change mean
extractname <- gsub("mean", " Mean ", extractname) # Change mean
extractname <- gsub("std", " Standard Deviation ", extractname) # Change std
extractname <- gsub("Jerk", " Jerk ", extractname) # Change Jerk
extractname <- gsub("[()]", "", extractname) # Remove ()
extractname <- gsub("-X", "- X", extractname) # Change -X
extractname <- gsub("-Y", "- Y", extractname) # Change -Y
extractname <- gsub("-Z", "- Z", extractname) # Change -Z
extractname <- gsub("  ", " ", extractname) # Fixing spacing (2)
extractname <- gsub("   ", " ", extractname) # Fixing spacing (3)

colnames(testtrainextract) <- extractname # Apply names to testtrainextract (Step 3)
```

# Creating Tidy Data Set
The final code block creates the tidy data set by systematically going thorugh each subject to return the means of all columns following by the means of each activty. The data is outputted as a text file and csv file.

```{r}
## Mean dataset generation (Step 4)

Step4 <- data.frame()

# Subject-based
for (i in 1:30) {
        meandf <- data.frame()
        # For every row, find matching subjects
        for (j in 1:nrow(testtrainextract)) {
                hold <- testtrainextract[j,"Subject"]
                if (hold == i) {
                        meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
                }
        }
        colmeanhold <- colMeans(meandf) # Calculate mean of data set
        colmeanhold <- append(colmeanhold, c("ALL"), after=0) # Add identifiers
        colmeanhold <- append(colmeanhold, c(i), after=0)
        Step4 <- rbind(Step4, colmeanhold)
}

# Activity-based
meandf <- data.frame()
# For every row, find matching activity (WALKING)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "WALKING") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("WALKING"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

meandf <- data.frame()
# For every row, find matching activity (WALKING UPSTAIRS)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "WALKING UPSTAIRS") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("WALKING UPSTAIRS"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

meandf <- data.frame()
# For every row, find matching activity (WALKING DOWNSTAIRS)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "WALKING DOWNSTAIRS") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("WALKING DOWNSTAIRS"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

meandf <- data.frame()
# For every row, find matching activity (SITTING)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "SITTING") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("SITTING"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

meandf <- data.frame()
# For every row, find matching activity (STANDING)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "STANDING") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("STANDING"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

meandf <- data.frame()
# For every row, find matching activity (LAYING)
for (j in 1:nrow(testtrainextract)) {
        hold <- testtrainextract[j,"Activity"]
        if (hold == "LAYING") {
                meandf <- rbind(meandf, testtrainextract[j,3:ncol(testtrainextract)])
        }
}

colmeanhold <- colMeans(meandf) # Calculate mean of data set
colmeanhold <- append(colmeanhold, c("LAYING"), after=0) # Add identifiers
colmeanhold <- append(colmeanhold, c("ALL"), after=0)
Step4 <- rbind(Step4, colmeanhold)

## Cleaning up and exporting
colnames(Step4) <- extractname # Adjust colnames
write.table(Step4, file = "Project Output.txt") # Output as txt (Step 4)
write.csv(Step4, file = "Project Output.csv") # Output as csv
```

