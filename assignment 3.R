library(reshape2)
library(stringr)
#read data
subject_test<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/test/subject_test.txt",sep = "",col.names = "SubjectNo",colClasses = "character")
subject_train<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/train/subject_train.txt",sep = "",col.names = "SubjectNo",colClasses = "character")
X_test<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/test/X_test.txt",header=FALSE,sep = "",colClasses = "numeric")
y_test<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/test/y_test.txt",header = FALSE,sep = "",col.names = "Activity")
X_train<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/train/X_train.txt",header = FALSE,sep = "",colClasses = "numeric")
y_train<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/train/y_train.txt",header = FALSE,sep = "",col.names = "Activity")
features<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/features.txt",header = FALSE,sep = "")
activity_labels<-read.table("C:/Users/acer/Documents/data analysis coursera/course 3 assignment/activity_labels.txt",header = FALSE,sep = "")


#set column names
features$V2<-gsub("-","",features$V2)
names(X_test)<-features$V2
names(X_train)<-features$V2

#add category name to "test" and "train" data set that tells whether test were performed on them or training was done
cat1<-rep("test",times=nrow(X_test))
cat2<-rep("train",times=nrow(X_train))

#data test
test<-cbind(subject_test,cat1,y_test,X_test)
names(test)[2]<-"category"

#data train
train<-cbind(subject_train,cat2,y_train,X_train)
names(train)[2]<-"category"

# unique(test$SubjectNo)
# unique(train$SubjectNo)
# unique(test$Activity)
# unique(train$Activity)

#merged data
merge<-rbind(test,train)
merge<-merge[order(merge[,1]),]
names(merge)<-gsub("\\(\\)","",names(merge))

#descriptive activity names
# str_replace_all(merge$Activity,c("1"="walking","2"="walking_upstairs","3"="walking_downstairs","4"="sitting","5"="standing","6"="laying"))
merge$Activity[merge$Activity==1]<-"walking"
merge$Activity[merge$Activity==2]<-"walking_upstairs"
merge$Activity[merge$Activity==3]<-"walking_downstairs"
merge$Activity[merge$Activity==4]<-"sitting"
merge$Activity[merge$Activity==5]<-"standing"
merge$Activity[merge$Activity==6]<-"laying"


newVar<-grep("[Mm]ean(.*)|[Ss]td(.*)",names(merge),value = TRUE)
mergeMeanStd<-subset(merge,select = c("SubjectNo","category","Activity",newVar))
#new variable that only contains measurements on the mean and standard deviation for each variable.


#creating new tidy data with average of each variable for each activity and each subject

mergeMelt<-melt(mergeMeanStd,id = c("Activity","SubjectNo"),measure.vars=newVar)
mergeAverageActivity<-dcast(mergeMelt,Activity~variable,mean)
names(mergeAverageActivity)[1]<-"id"
mergeAverageSubjectNo<-dcast(mergeMelt,SubjectNo~variable,mean)
names(mergeAverageSubjectNo)[1]<-"id"
mergeAverage<-rbind(mergeAverageActivity,mergeAverageSubjectNo)
tidyData<-write.table(mergeAverage,file = "C:/Users/acer/Documents/data analysis coursera/tidydata.txt",row.names = FALSE)
