# ###############################################################
# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
#  2/ add rescale yes/no option
# ADD KEYWORDS FIELD LATER
#  4/ more sophisticated selection of returned columns
# ###############################################################
# Lewis and Sauro (2009) have 2 subscales:
#   `Usable' = 1, 2, 3, 5, 6, 7, 8, 9 [rescale factor = 3.125]
#   `Learnable' = 4, 10 [rescale factor = 12.5]
# ###############################################################

#' Calculate participants' SUS questionnaire scores
#' 
#' Calculate the SUS score per questionnaire participant.  At the
#' moment, this function does not rescale to 0-100.
#'
#' @param myData no default; data frame of questionnaire results; see example
#'   for naming convention expected.
#' @param user_id default TRUE; does the data frame have a column for user ID?
#' @concept questionnaire, scale
#' score_sus()
score_sus <- function(myData, user_id=TRUE, rescale=TRUE){
  if (ncol(myData) == 10 + user_id){ # don't run if wrong number of columns
    myData <- myData %>% mutate(sus= 
       (Q1 - 1) + (5 - Q2) +
       (Q3 - 1) + (5 - Q4) +
       (Q5 - 1) + (5 - Q6) +
       (Q7 - 1) + (5 - Q8) +
       (Q9 - 1) + (5 - Q10)
     ) 
     if(user_id){
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

