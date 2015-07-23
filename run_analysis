run_anlaysis <- 
{
   #DOWNLOADS ZIP FILE FOR ASSIGNMENT
   if(!file.exists("project_data.zip"))
   {
    download.file(url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "project_data.zip")
   }
  
   #CHECK IF FOLDER project3_data EXISTS AND IF NOT CREATES
   if(!file.exists("project3_data"))
   {
     dir.create("project3_data")    
   }   
     
   #CHECK IF FOLDER project_data.zip EXISTS IN WORKING DIRECTORY AND IF SO UNZIPS TO FOLDER project3_data
   if(file.exists("project_data.zip"))
   {
     unzip(zipfile = "project_data.zip", exdir = "./project3_data")  
   }  
   
   #READ FILES INTO SEPARATE VARIABLES
   activity_labels <- read.table("./project3_data/UCI HAR Dataset/activity_labels.txt", header = FALSE) 
   colnames(activity_labels)[1] = "activity_id"
   colnames(activity_labels)[2] = "activity"

   #UPDATES y_test WITH ACTIVITY LABELS
   y_test <- read.table("./project3_data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
   colnames(y_test)[1] = "activity_id"
   y_test <- merge(y_test, activity_labels, by.y = "activity_id", by.x = "activity_id")
     
   #UPDATE X_test WITH FEATURES   
   features <- read.table("./project3_data/UCI HAR Dataset/features.txt", header = FALSE)
   X_test <- read.table("./project3_data/UCI HAR Dataset/test/X_test.txt", header = FALSE)      
   colnames(X_test) <- features[,"V2"]

   #COMBINES TEST SUBJECTS WITH DATASET  
   subject_test <- read.table("./project3_data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
   colnames(subject_test)[1] = "subject"      

   #UPDATES y_train WITH ACTIVITY LABELS
   y_train <- read.table("./project3_data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
   colnames(y_train)[1] = "activity_id"
   y_train <- merge(y_train, activity_labels, by.y = "activity_id", by.x = "activity_id")
   
   #UPDATE X_train WITH FEATURES   
   X_train <- read.table("./project3_data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
   colnames(X_train) <- features[,"V2"]   
   
   #COMBINES SUBJECTS WITH DATASET  
   subject_train <- read.table("./project3_data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
   colnames(subject_train)[1] = "subject"      
      
   #COMBINE DATA INTO ONE DATASET
   test <- cbind(X_test, y_test, subject_test)
   train <- cbind(X_train, y_train, subject_train)
   combined_data <- rbind(train, test)
   combined_data_df <- as.data.frame(combined_data)
   combined_data_melted <- melt(combined_data, id = c("subject", "activity"))
   combined_data_melted_filtered <- filter(combined_data_melted, grepl('mean()|std()', variable))
   run_analysis_summary <- aggregate(value ~ subject + activity + variable, combined_data_melted_filtered, mean)
}

