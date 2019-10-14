library(caret)
data <- read.csv ( "SelRF.csv")
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

model_rfinhosp <- readRDS("fmodel1y.rds")


#create a data frame capture from php form 

function_( bonePart, angulation, fractureDiameter, age ) {

 
  
x <- data.frame(bonePart, angulation, fractureDiameter, age, stringsAsFactors = FALSE)

  #make sure they are numeric
  x <<- as.data.frame(t(sapply(x, as.integer)))
  
  colnames(x) <- c("bonePart", "angulation", "fractureDiameter", "age")
  
#make prediction using the saved rds file 
  pred_weeks <<- predict(model_rfinhosp, x, type="prob")


#put results in data frame 
  resultfracture <<- data.frame(pred_weeks)



denom_resultfracture <- ((resultfracture*10)+2)

  colnames(y) <- c("healing weeks")
  


  }
  