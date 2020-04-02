





#* Echo back the input
#* @param msg The message to echo
#* @get /echo
calculate_prediction <- function( bonePart, angulation, fractureDiameter, age ) {
  
  library(caret)
  data <- read.csv ( file="/Users/syahrinseth/documents/projects/UpperLimb/FYP_UpperLimb/SelRF.csv")
  #data <- read.delim("SelRF.txt", sep = '\t', header = TRUE)
  set.seed(240)
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
  
  predicted <- ((predresult*10)+2)
  Actual <- (((testing$HealTimeWeeks)*10)+2)
  
  plot(Actual,predicted)
  
  rmse <- sqrt(sum((Actual - predicted)^2)/nrow(testing))
  
  rmse
  
  saveRDS(R_model, "./fmodel1y.rds")
  ###new testing
  
  
  
  
  # start a different file plumber.r
  # load saved machine learning prediction model
  
  model_rfinhosp <- readRDS("./fmodel1y.rds")

  
  
  #create a data frame capture from php form
 
  x <- data.frame(sapply(bonePart, as.integer), sapply(angulation, as.integer), sapply(fractureDiameter, as.integer), sapply(age, as.integer), stringsAsFactors = FALSE)

  #make sure they are numeric
  x <<- as.data.frame(t(sapply(x, as.integer)))
 
  colnames(x) <- c("bonePart", "angulation", "fractureDiameter", "age")
 
#make prediction using the saved rds file
  pred_weeks <<- predict(model_rfinhosp, x)


#put results in data frame
  resultfracture <<- data.frame(pred_weeks)


  denom_resultfracture <- ((resultfracture*10)+2)

  colnames(denom_resultfracture) <- c("healing_weeks")
 
  paste(as.character(denom_resultfracture$healing_weeks), sep = ",")
  }
