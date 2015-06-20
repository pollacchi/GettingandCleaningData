
#File downloaded from
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#creating data tables
testlabels <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt")
test <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
subjecttest <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

trainlabels <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt")
train <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
subjecttrain <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

activitylabels <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
featurestext <- read.table("gettingandcleaningdataproject/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")

#Adding labels

colnames(activitylabels)  = c('activityId','activityType');
colnames(subjecttrain)  = "subjectId";
colnames(subjecttest)  = "subjectId";

colnames(train)        = featurestext[,2]; 
colnames(test)        = featurestext[,2]; 
colnames(trainlabels)        = "activityId";
colnames(testlabels)        = "activityId";

trainingData = cbind(train,trainlabels,subjecttrain);
testData = cbind(test,testlabels,subjecttest);


combinedData = rbind(trainingData,testData)

colNames  = colnames(combinedData)


logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset combined to keep only required columns
combinedData = combinedData[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the combined data set 
combinedData = merge(combinedData,activitylabels,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(combinedData)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
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
};

# Reassigning the new descriptive column names to the finalData set
colnames(combinedData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = combinedData[,names(combinedData) != 'activityType']

# Summarise the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activitylabels,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')

