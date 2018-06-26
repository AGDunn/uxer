# ###############################################################
# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
#  2/ add rescale yes/no option
#  3/ make calculation non-silly (main scale route via subscales)
# ADD KEYWORDS FIELD LATER
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
score_sus <- function(myData, user_id=TRUE){
if (ncol(myData) == 10 + user_id){ # don't run if wrong number of columns
    myData %>% mutate(sus= 
       (Q1 - 1) + (5 - Q2) +
       (Q3 - 1) + (5 - Q4) +
       (Q5 - 1) + (5 - Q6) +
       (Q7 - 1) + (5 - Q8) +
       (Q9 - 1) + (5 - Q10)
     ) %>%
     select(participant, sus) # return sus scores per participant, unscaled
  } else {
   print("wrong number of columns in data")
  }
}

# once a few been done, make an overall wrapper for them
# score_responses()

