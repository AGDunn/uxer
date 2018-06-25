# TODO BEFORE RUNNING DOCUMENTATION:
#  1/ add subscales
#  2/ add rescale yes/no option
#  3/ make calculation non-silly (main scale route via subscales)
# ###############################################################
#' Calculate participants' scores on SUS scales.
#' @param 
#' @keywords 
#' @concept questionnaire, scale
#' score_sus()
score_sus <- function(myData){
if (length(myData) == 11){ # won't run if 
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
