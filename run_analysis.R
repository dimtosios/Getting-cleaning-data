#Step 1    " Read the data from the unziped file "
######################################################

setwd("C:/Users/dimitris/Desktop/R programming/UCI HAR Dataset")

features     = read.table('./features.txt',header=FALSE); #imports features.txt
activitylabels = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjecttrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xtrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
ytrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
subjecttest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xtest = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
ytest = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt




# Change the labels names so that the data names are correct
colnames(activitylabels)  = c('activityId','activityType')
colnames(subjecttrain)  = "subjectId"
colnames(xtrain)        = features[,2]
colnames(ytrain)        = "activityId"

colnames(subjecttest) = "subjectId"
colnames(xtest)       = features[,2]
colnames(ytest)       = "activityId"


trainingdata = cbind(ytrain,subjecttrain,xtrain)  # Create the wholle data train set

testdata = cbind(ytest,subjecttest,xtest) # Create the wholle data test set


#  finaldata= Train+test data set 

finaldata = rbind(trainingdata,testdata)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finaldata) 

############################################################


# Step 2 (Create the final data set based on the mean and std data)
####################################################################

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalvector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

finaldata = finaldata[logicalvector==TRUE]

# Step 3 Update the finaldata data frame with the activitylabels vector
########################################################################

# Merge the finalData set with the acitivitylabels table
finaldata = merge(finaldata,activitylabels,by='activityId',all.x=TRUE)

# Updating the colNames of the finaldata set
colNames  = colnames(finaldata)


# Step 4 (make the colnames readable)
##########################################################


for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

# Update the finaldata new names
colnames(finaldata) = colNames
######################################################################


# Step 5 Create a tidy data set with the average of each variable for each activity and each subject
################################################################################################################

# Create a new table without activityType 
finalData  = finaldata[,-21]

# Summarizing the finalData to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalData[,names(finalData) != c('activityId','subjectId')],by=list(activityId=finalData$activityId,subjectId = finalData$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activitylabels,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, 'C:/Users/dimitris/Desktop/R programming/UCI HAR Dataset/tidyData.txt',row.names=TRUE,sep='\t')