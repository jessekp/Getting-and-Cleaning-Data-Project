# 1 Setup
  
  # 1.1 set working directory
  #setwd("C:/Users/...")
  
  # 1.2 load required packages  
  if(!is.element("plyr", installed.packages()[,1])){
    install.packages("plyr")
  }
  library(plyr)

# 2 Functions

  # 2.1 Get data
  file <- "data.zip"
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  filePath <- "UCI HAR Dataset"
  resultFolder <- "results"
  if(!file.exists(file)){ download.file(url,file, mode = "wb")}
  if(!file.exists(resultFolder)){ dir.create(resultFolder)} 
  
  # 2.2 Creates a data table 
  getTable <- function (filename,cols = NULL){
    f <- unz(file, paste(filePath,filename,sep="/"))
    data <- data.frame()
    
    if(is.null(cols)){ data <- read.table(f,sep="",stringsAsFactors=F) } 
    else { data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols) }
    data
  }
  
  # 2.3 creates a complete data set
  getData <- function(type, features){
    subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
    y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
    x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
    return (cbind(subject_data,y_data,x_data)) 
  }
  
  # 2.4 saves the data into the result folder
  saveResult <- function (data,name){
    file <- paste(resultFolder, "/", name,".csv" ,sep="")
    write.csv(data,file)
  }

# 3 Main program
  
  # 3.1 apply descriptive names to name the activities
  features <- getTable("features.txt")
  
  # 3.2 merges training and test datasets into one dataset
  train <- getData("train",features)
  test <- getData("test",features)
  data <- rbind(train, test)
  data <- arrange(data, id)
  
  # 3.3 label the dataset with descriptive activity names
  activity_labels <- getTable("activity_labels.txt")
  data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)
  
  # 3.4 extract the mean and standard deviation for each measurement. 
  dataset1 <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
  saveResult(dataset1,"meanAndStandardDeviationDataset")
  
  # 3.5 creates a tidy dataset with the average of each variable for each activity and each subject. 
  dataset2 <- ddply(dataset1, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
  colnames(dataset2)[-c(1:2)] <- paste(colnames(dataset2)[-c(1:2)], "_mean", sep="")
  saveResult(dataset2,"tidyDataset")