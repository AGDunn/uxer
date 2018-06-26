# ###############################################################
# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
# ADD KEYWORDS FIELD LATER
#  4/ more sophisticated selection of returned columns
#  5/ work out how to handle errors in data
# ###############################################################
# Lewis and Sauro (2009) have 2 subscales:
#   `Usable' = 1, 2, 3, 5, 6, 7, 8, 9 [rescale factor = 3.125]
#   `Learnable' = 4, 10 [rescale factor = 12.5]
# ###############################################################

#' Calculate participants' SUS questionnaire scores
#' 
#' Calculate the SUS score per questionnaire participant.  By default, will
#' rescale it to 0-100 range.
#'
#' @param myData no default; data frame of questionnaire results.  See example
#'   for naming convention expected.
#' @param user_id default TRUE; does the data frame have a column for user ID?
#' @param rescale default TRUE; transform main scale and subscales to 0-100
#'   range?
#' @concept questionnaire, scale
#' @export
score_sus <- function(myData, user_id=TRUE, rescale=TRUE){
  if (ncol(myData) == 10 + user_id){ # don't run if wrong number of columns
    if (rescale){
      main_rescale = 2.5; use_rescale = 3.125; learn_rescale = 12.5
    } else {
      main_rescale = 1; use_rescale = 1; learn_rescale = 1
    }
    myData <- myData %>% mutate(sus= 
       (Q1 - 1) + (5 - Q2) +
       (Q3 - 1) + (5 - Q4) +
       (Q5 - 1) + (5 - Q6) +
       (Q7 - 1) + (5 - Q8) +
       (Q9 - 1) + (5 - Q10)
     ) %>% mutate(sus=sus * main_rescale)
     if (user_id){
       return(myData %>% select(participant, sus))
     } else {
       return(myData %>% select(sus))
     }  
  } else {
   print("wrong number of columns in data")
  }
}

# once a few been done, make an overall wrapper for them
# score_responses()

