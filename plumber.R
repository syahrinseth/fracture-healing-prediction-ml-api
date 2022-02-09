#* Echo back the input
#* @param msg The message to echo
#* @get /echo
calculate_prediction <- function( bonePart, angulation, fractureDiameter, age ) {
  
  library(caret)
  
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  # get svr raw fracture data
  data <- read.csv ( file="SVR_raw_fracture.csv")
  #data <- read.delim("SelRF.txt", sep = '\t', header = TRUE)
  set.seed(241)
  # normalize data
  normalize_data <- as.data.frame(lapply(data, normalize))
  normalize_data
  
  # train heal time weeks data
  intrain <- createDataPartition(data$HealTimeWeeks, p= 0.7, list = FALSE)
  training <- data[intrain,]
  testing <- data[-intrain,]
  
  capture.output(testing,file='testingRandomSampling.txt', append=TRUE)
  
  x <- training[,-5]
  y <- training[,5]
  
  CV_Folds <- createMultiFolds(y, k = 3, times = 3)
  
  R_model <- train(data.frame(x),y, method="svmRadial",tuneLength=5,
                   trControl=trainControl(method='repeatedCV',index=CV_Folds))
  R_model
  
  predresult <- predict(R_model, testing[,-5])
  
  predresult
  
  #predicted <- ((predresult*10)+2)
  #Actual <- (((testing$HealTimeWeeks)*10)+2)
  
  #plot(Actual,predicted)
  
  #rmse <- sqrt(sum((Actual - predicted)^2)/nrow(testing))
  
  #rmse
  
  saveRDS(R_model, "./fmodel1y.rds")
  ###new testing
  
  
  
  
  # start a different file plumber.r
  # load saved machine learning prediction model
  
  model_rfinhosp <- readRDS("./fmodel1y.rds")

  
  
  #create a data frame capture from php form
 
  x <- data.frame(sapply(bonePart, as.numeric), sapply(angulation, as.numeric), sapply(fractureDiameter, as.numeric), sapply(age, as.integer), stringsAsFactors = FALSE)

  #make sure they are numeric
  x <<- as.data.frame(t(sapply(x, as.numeric)))
 
  colnames(x) <- c("bonePart", "angulation", "fractureDiameter", "age")
 
#make prediction using the saved rds file
  pred_weeks <<- predict(model_rfinhosp, x)


#put results in data frame
  resultfracture <<- data.frame(pred_weeks)


  denom_resultfracture <- ((resultfracture*10)+2)

  colnames(denom_resultfracture) <- c("healing_weeks")
 
  paste(as.character(denom_resultfracture$healing_weeks), sep = ",")
  #paste('1')
}



#* Echo back the input
#* @param msg The message to echo
#* @get /healing-weeks-prediction
healingWeeksPrediction <- function( bonePart, angulation, fractureDiameter, age ) {
  
  # test script - - - - - --  --  - - - - - - - -
  
  library(caret)
  
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  denormalize <- function(z,mintest,maxtest)
  {
    z*(maxtest-mintest) + mintest
  }
  
  normalizePhpData <- function(x, min, max){
    return((x-min)/(max-min))
  }
  # sapply(normalize_data, min)
  # sapply(normalize_data, max)
  
  ## Get Raw Data
  # get svr raw fracture data
  data <- read.csv ( file="SVR_raw_fracture.csv")
  #data <- read.delim("SelRF.txt", sep = '\t', header = TRUE)
  set.seed(241)
  
  # get min and max data
  min_data <- sapply(data, min)
  max_data <- sapply(data, max)
  
  ## Normalize Raw Data
  # normalize data
  normalize_data <- as.data.frame(lapply(data, normalize))
  normalize_data
  
  # train heal time weeks data
  intrain <- createDataPartition(normalize_data$HealTimeWeeks, p= 0.7, list = FALSE)
  training <- normalize_data[intrain,]
  testing <- normalize_data[-intrain,]
  
  capture.output(testing,file='testingRandomSampling.txt', append=TRUE)
  
  x <- training[,-5] # train other data
  y <- training[,5] # train healing weeks
  
  CV_Folds <- createMultiFolds(y, k = 3, times = 3)
  
  R_model <- train(data.frame(x),y, method="svmRadial",tuneLength=5,
                   trControl=trainControl(method='repeatedCV',index=CV_Folds))
  R_model
  
  predresult <- predict(R_model, testing[,-5])
  
  predresult
  
  #predicted <- ((predresult*10)+2)
  #Actual <- (((testing$HealTimeWeeks)*10)+2)
  
  #plot(Actual,predicted)
  
  #rmse <- sqrt(sum((Actual - predicted)^2)/nrow(testing))
  
  #rmse
  
  saveRDS(R_model, "./fmodel1y.rds")
  ###new testing
  
  # start a different file plumber.r
  # load saved machine learning prediction model
  
  model_rfinhosp <- readRDS("./fmodel1y.rds")
  
  ## get data from PHP
  #create a data frame capture from php form 3,0,15.2,14 3,10,10.8,14,8
  # bonePart <- 1
  # angulation <- 10
  # fractureDiameter <- 140
  # age <- 15
  
  ## Normalization data frame from php
  normBonePart <- normalizePhpData(as.double(bonePart), as.double(min_data['bonePart']), as.double(max_data['bonePart']))
  normAngulation <- normalizePhpData(as.double(angulation), as.double(min_data['angulationAP']), as.double(max_data['angulationAP']))
  normfractureDiameter <- normalizePhpData(as.double(fractureDiameter), as.double(min_data['fractureDistanceAP']), as.double(max_data['fractureDistanceAP']))
  normAge <- normalizePhpData(as.double(age), as.double(min_data['age']), as.double(max_data['age']))
  
  x <- data.frame(sapply(normBonePart, as.numeric), sapply(normAngulation, as.numeric), sapply(normfractureDiameter, as.numeric), sapply(normAge, as.integer), stringsAsFactors = FALSE)
  
  #make sure they are numeric
  x <<- as.data.frame(t(sapply(x, as.numeric)))
  
  colnames(x) <- c("bonePart", "angulation", "fractureDiameter", "age")
  
  #make prediction using the saved rds file
  pred_weeks <<- predict(model_rfinhosp, x)
  
  
  #put results in data frame
  resultfracture <<- data.frame(pred_weeks)
  
  # denorm the result
  #denom_resultfracture <- ((resultfracture*10)+2)
  denom_resultfracture <- denormalize(resultfracture, as.double(min_data['HealTimeWeeks']), as.double(max_data['HealTimeWeeks']))
  
  colnames(denom_resultfracture) <- c("healing_weeks")
  
  paste(as.character(denom_resultfracture$healing_weeks), sep = ",")
  #paste('1')
  
}



#* Echo back the input
#* @param msg The message to echo
#* @get /healing-weeks-prediction-denorm-algo
healingWeeksPredictionDenormAlgo <- function( bonePart, angulation, fractureDiameter, age ) {
  
  # test script - - - - - --  --  - - - - - - - -
  
  library(caret)
  
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  
  denormalize <- function(z,mintest,maxtest)
  {
    z*(maxtest-mintest) + mintest
  }
  
  normalizePhpData <- function(x, min, max){
    return((x-min)/(max-min))
  }
  # sapply(normalize_data, min)
  # sapply(normalize_data, max)
  
  ## Get Raw Data
  # get svr raw fracture data
  data <- read.csv ( file="SVR_raw_fracture.csv")
  #data <- read.delim("SelRF.txt", sep = '\t', header = TRUE)
  set.seed(241)
  
  ## Normalize Raw Data
  # get min and max data
  min_data <- sapply(data, min)
  max_data <- sapply(data, max)
  
  # normalize data
  # normalize_data <- as.data.frame(lapply(data, normalize))
  # normalize_data
  
  # train heal time weeks data
  intrain <- createDataPartition(data$HealTimeWeeks, p= 0.7, list = FALSE)
  training <- data[intrain,]
  testing <- data[-intrain,]
  
  capture.output(testing,file='testingRandomSampling.txt', append=TRUE)
  
  x <- training[,-5] # train other data
  y <- training[,5] # train healing weeks
  
  CV_Folds <- createMultiFolds(y, k = 3, times = 3)
  
  R_model <- train(data.frame(x),y, method="svmRadial",tuneLength=5,
                   trControl=trainControl(method='repeatedCV',index=CV_Folds))
  R_model
  
  predresult <- predict(R_model, testing[,-5])
  
  predresult
  
  #predicted <- ((predresult*10)+2)
  #Actual <- (((testing$HealTimeWeeks)*10)+2)
  
  #plot(Actual,predicted)
  
  #rmse <- sqrt(sum((Actual - predicted)^2)/nrow(testing))
  
  #rmse
  
  saveRDS(R_model, "./fmodel1y.rds")
  ###new testing
  
  # start a different file plumber.r
  # load saved machine learning prediction model
  
  model_rfinhosp <- readRDS("./fmodel1y.rds")
  
  ## get data from PHP
  #create a data frame capture from php form 3,0,15.2,14 3,10,10.8,14,8
  # bonePart <- 1
  # angulation <- 10
  # fractureDiameter <- 140
  # age <- 15
  
  ## Normalization data frame from php
  # normBonePart <- normalizePhpData(as.double(bonePart), as.double(min_data['bonePart']), as.double(max_data['bonePart']))
  # normAngulation <- normalizePhpData(as.double(angulation), as.double(min_data['angulationAP']), as.double(max_data['angulationAP']))
  # normfractureDiameter <- normalizePhpData(as.double(fractureDiameter), as.double(min_data['fractureDistanceAP']), as.double(max_data['fractureDistanceAP']))
  # normAge <- normalizePhpData(as.double(age), as.double(min_data['age']), as.double(max_data['age']))
  
  x <- data.frame(sapply(bonePart, as.numeric), sapply(angulation, as.numeric), sapply(fractureDiameter, as.numeric), sapply(age, as.integer), stringsAsFactors = FALSE)
  
  #make sure they are numeric
  x <<- as.data.frame(t(sapply(x, as.numeric)))
  
  colnames(x) <- c("bonePart", "angulation", "fractureDiameter", "age")
  
  #make prediction using the saved rds file
  pred_weeks <<- predict(model_rfinhosp, x)
  
  
  #put results in data frame
  resultfracture <<- data.frame(pred_weeks)
  
  # denorm the result
  #denom_resultfracture <- ((resultfracture*10)+2)
  # denom_resultfracture <- denormalize(resultfracture, as.double(min_data['HealTimeWeeks']), as.double(max_data['HealTimeWeeks']))
  
  colnames(resultfracture) <- c("healing_weeks")
  
  paste(as.character(resultfracture$healing_weeks), sep = ",")
  #paste('1')
  
}


