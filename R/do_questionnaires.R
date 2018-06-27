# ###############################################################
# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
# ADD KEYWORDS FIELD LATER
#  4/ more sophisticated selection of returned columns
#  5/ work out how to handle errors in data
# ###############################################################
# Lewis and Sauro (2009) have 2 subscales:
# ###############################################################

#' Calculate participants' SUS questionnaire scores
#' 
#' Calculate the SUS score per questionnaire participant.  By default, will
#'   rescale it to 0-100 range.  Also calculates the `usable' and `learnable'
#'   subscales if asked to.
#'
#' @param myData no default; data frame of questionnaire results.  See example
#'   for naming convention expected.
#' @param user_id default TRUE; does the data frame have a column for user ID?
#' @param rescale default TRUE; transform main scale and subscales to 0-100
#'   range?
#' @param default FALSE; calculate subscales `Usable' and `Learnable'?
#' @concept questionnaire, scale
#' @export
score_sus <- function(myData, user_id=TRUE, rescale=TRUE, subscales=FALSE){
  if (ncol(myData) == 10 + user_id){ # don't run if wrong number of columns
    if (rescale){
      main_rescale = 2.5; use_rescale = 3.125; learn_rescale = 12.5
    } else {
      main_rescale = 1; use_rescale = 1; learn_rescale = 1
    }
    if (user_id){
      id_list <- myData %>% pull(participant)
    }
    holder <- myData %>% mutate(
       sus = 
         (Q1 - 1) + (5 - Q2) +
         (Q3 - 1) + (5 - Q4) +
         (Q5 - 1) + (5 - Q6) +
         (Q7 - 1) + (5 - Q8) +
         (Q9 - 1) + (5 - Q10)
     ) 
     if (subscales){
       holder <- holder %>% mutate(
         usable =
           (Q1 - 1) + (5 - Q2) + (Q3 - 1) + (Q5 - 1) +
           (5 - Q6) + (Q7 - 1) + (5 - Q8) + (Q9 - 1),
         learnable = (5 - Q4) + (5 - Q10)
       ) %>% select(
          usable * use_rescale, learnable * learn_rescale, sus * main_rescale
         )
     } else {
       holder <- holder %>% select(sus * main_rescale)
     if (user_id){
       holder <- holder %>% add_column(id_list)
     }
     return(holder)
     }
  } else {
   print("wrong number of columns in data")
  }
}

# once a few been done, make an overall wrapper for them
# score_responses()

