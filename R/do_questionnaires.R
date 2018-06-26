# ###############################################################
# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
#  2/ add rescale yes/no option
#  3/ make calculation non-silly (main scale route via subscales)
# ###############################################################
# Lewis and Sauro (2009) have 2 subscales:
#   `Usable' = 1, 2, 3, 5, 6, 7, 8, 9 [rescale factor = 3.125]
#   `Learnable' = 4, 10 [rescale factor = 12.5]
# ###############################################################

#' Calculate participants' SUS questionnaire scores.
#' @param myData, no default; data frame of questionnaire results; see example
#'   for naming convention expected.
#' @param user_id, default TRUE; does the data frame have a column for user ID?
#' @keywords 
#' @concept questionnaire, scale
#' score_sus()
score_sus <- function(myData, user_id=TRUE){
if (ncol(myData) == 10 + user_id){ # won't run if 
    myData %>%               # wrong number of columns
      mutate(
        normalQ=(            # total score for 
          (Q1 - 1) +         # normal-scored questions
          (Q3 - 1) + 
          (Q5 - 1) + 
          (Q7 - 1) + 
          (Q9 - 1)
        ), reverseQ=(        # total score for 
          (5 - Q2) +         # reverse-score questions 
          (5 - Q4) +
          (5 - Q6) +
          (5 - Q8) +
          (5 - Q10)
        )
      ) %>% 
      mutate(sus=((normalQ + reverseQ) * 2.5)) %>% # calculate sus 0-100
      select(participant, normalQ, reverseQ, sus)  # create summary df
  } else {
   print("wrong number of columns in data")        # warn if data wrong shape
  }
}

# once a few been done, make an overall wrapper for them
# score_responses
