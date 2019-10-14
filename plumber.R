
# script name:
# plumber.R

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Machine Learning Probability Calculator for ACS Mortality 
#' @apiDescription This API takes as patient data and returns a probability of survival and non-survival
#' with values between 0 and 1.

# can combine all parameters in one plumber.R file
# but must have different parameter names for different models (.rds files)

# load model
# this path would have to be adapted if you would deploy this
model_rfinhosp <- readRDS("fmodelinhosp.rds")
model_rf30d <- readRDS("fmodel30d.rds")
model_rf1y <- readRDS("fmodel1y.rds")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' predict survival probability of ACS patients with Random Forest model
#' @param age:numeric years
#' @param heartrate:numeric Norm: 
#' @param bpsys:numeric Norm: 
#' @param killipclass:numeric Norm:
#' @param tc:numeric Norm:
#' @param fbg:numeric Norm:
#' 
#' @param bpdias:numeric Norm:
#' @param cardiacarrest:numeric Norm:
#' @param hdlc:numeric Norm:
#' @param ldlc:numeric Norm:
#' @param tg:numeric Norm:
#' @param bb:numeric Norm:
#' @param lipidla:numeric Norm:
#' @param acei:numeric Norm:
#' 
#' 

#' @get /predict
#' @html

#' @response 200 Returns the probability prediction of survival and non-survival of patients
#' Random Forest model; values are between 0 and 1
calculate_prediction <- function(age, heartrate, bpsys, killipclass, tc, fbg, bpdias, cardiacarrest, hdlc, ldlc, tg, bb, lipidla, acei) {
  
  #make dataframe for IN HOSPITAL
  input_datainhosp <<- data.frame(age, heartrate, bpsys, bpdias, killipclass, cardiacarrest, tc, hdlc, ldlc, tg, fbg, bb, stringsAsFactors = FALSE)
  
  #make sure they are numeric
  input_datainhosp <<- as.data.frame(t(sapply(input_datainhosp, as.integer)))
  
  colnames(input_datainhosp) <- c("age", "heartrate", "bpsys", "bpdias", "killipclass", "cardiacarrest", "tc", "hdlc", "ldlc", "tg", "fbg", "bb")

  pred_rfinhosp <<- predict(model_rfinhosp, input_datainhosp, type="prob")
  result_inhosp <<- data.frame(pred_rfinhosp)
  colnames(result_inhosp) <- c("survival", "Nonsurvival")
  #paste("", as.character(result$survival), sep = "")
  
  
  
  #make dataframe for 30DAYS
  input_data30 <<- data.frame(age, heartrate, bpsys, bpdias, killipclass, tc, tg, fbg, lipidla, stringsAsFactors = FALSE)
  
  #make sure they are numeric
  input_data30 <<- as.data.frame(t(sapply(input_data30, as.integer)))
  
  colnames(input_data30) <- c("age", "heartrate", "bpsys", "bpdias", "killipclass", "tc", "tg", "fbg", "lipidla")
  
  pred_rf30 <<- predict(model_rf30d, input_data30, type="prob")
  result30 <<- data.frame(pred_rf30)
  colnames(result30) <- c("survival", "Nonsurvival")
  
  
  
  
  #make dataframe for 30DAYS
  input_data1y <<- data.frame(age, heartrate, bpsys, killipclass, tc, fbg, acei, lipidla, stringsAsFactors = FALSE)
  
  #make sure they are numeric
  input_data1y <<- as.data.frame(t(sapply(input_data1y, as.integer)))
  
  colnames(input_data1y) <- c("age", "heartrate", "bpsys", "killipclass", "tc", "fbg", "acei", "lipidla")
  
  pred_rf1y <<- predict(model_rf1y, input_data1y, type="prob")
  result1y <<- data.frame(pred_rf1y)
  colnames(result1y) <- c("survival", "Nonsurvival")
  

  paste(as.character(result_inhosp$survival), as.character(result30$survival), as.character(result1y$survival), sep = ",")
  
}


