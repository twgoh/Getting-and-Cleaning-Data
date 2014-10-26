#########################################
###### STEP 0: READING IN DATASETS ######
#########################################

# Read in the subjects who performed the activities
subjecttest <- read.table("subject_test.txt")
subjecttrain <- read.table("subject_train.txt")

# Read in the test and training data
xtest <- read.table("x_test.txt")
xtrain <- read.table("x_train.txt")

# Read in the test and training labels
ytest <- read.table("y_test.txt")
ytrain <- read.table("y_train.txt")

# Read in the features text file which contains the list of variable names for the training and test sets above
features <- read.table("features.txt")

##########################################################################################
###### LABEL DATA SET WITH THE DESCRIPTIVE VARIABLE NAMES IN THE FEATURES TEXT FILE ######
##########################################################################################

names(xtest)<-features[[2]]
names(xtrain)<-features[[2]]

#############################################################################
###### STEP 1: MERGE THE TRAINING AND TEST SETS TO CREATE ONE DATA SET ######
#############################################################################

# Combine the subject, test labels and test set
colnames(subjecttest) <- c("subject")
colnames(ytest) <- c("activitynumber")
subject_y_x_test <- cbind(subjecttest,ytest,xtest)

# Combine the subject, training labels and training set
colnames(subjecttrain) <- c("subject")
colnames(ytrain) <- c("activitynumber")
subject_y_x_train <- cbind(subjecttrain,ytrain,xtrain)

# Merge the training and test sets to create one data set
combineddata <- rbind(subject_y_x_test,subject_y_x_train)

########################################################################
###### STEP 2: EXTRACT ONLY MEAN AND STANDARD DEVIATION VARIABLES ######
########################################################################

# Keep only variables containing "mean"
data_mean <- combineddata[grep("mean", colnames(combineddata))]

# Variables that contain "meanFreq" removed
data_mean <- data_mean[-grep("meanFreq", colnames(data_mean))]

# Keep only variables containing "std"
data_std <- combineddata[grep("std", colnames(combineddata))]

# Combined dataset that contains variables containing "mean()" or "std()"
data_vars <- cbind(subject=combineddata$subject, activitynumber=combineddata$activitynumber, data_mean, data_std)

###########################################################################
###### STEP 3: USE DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES ######
###########################################################################

# Merge the test and training labels
ytesttrain <- rbind(ytest,ytrain)
colnames(ytesttrain) <- c("activitynumber")

# Add the activity numbers to the combined dataset
data_vars_act <- cbind(data_vars,ytesttrain)

# Read the activity labels
activitylabels <- read.table("activity_labels.txt")
colnames(activitylabels) <- c("activitynumber","activity")

# Merge the activity labels into the combined data set
data_vars_actdesc <- merge(activitylabels,data_vars,by.x="activitynumber",by.y="activitynumber")
data_vars_actdesc$activitynumber <- NULL

######################################################################
###### STEP 4: LABEL THE DATASET WITH DESCRITIVE VARIABLE NAMES ######
######################################################################

# Variables have been labelled before Step 1

#########################################################################################
###### STEP 5: CREATE TIDY DATA SET WITH THE AVERAGE FOR EACH ACTIVITY AND SUBJECT ######
#########################################################################################

# Compute average for each activity and subject using group_by() and summarise_each() in dplyr 
library(dplyr)
tidy <- tbl_df(data_vars_actdesc)
by_subject_activity <- group_by(tidy, subject, activity)
tidy_data <- summarise_each(by_subject_activity, funs(mean))

# Export data set as a txt file using write.table
write.table(tidy_data, file="tidy_data.txt", row.names = FALSE)

# Demonstration of how to read in the txt file
test <- read.table("tidy_data.txt", header=TRUE)
