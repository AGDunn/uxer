#' Calculate participants' SUS questionnaire scores
#' 
#' Calculate the SUS score per questionnaire participant.  By default, will
#'   rescale it to 0-100 range.  Also calculates the `usable' and `learnable'
#'   subscales if asked to.
#'
#' @param myData no default.  Data frame of questionnaire results.  See example
#'   for naming convention expected.
#' @param user_id default TRUE.  Does the data frame have a column for user ID?
#' @param rescale default TRUE.  Transform main scale and subscales to 0-100
#'   range?
#' @param subscales default FALSE.  Calculate subscales
#'   `Usable' and `Learnable'?
#' @concept questionnaire, scale
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr transmute
#' @references there should be a reference here
#' @export
score_sus <- function(myData, user_id=TRUE, rescale=TRUE, subscales=FALSE){
  if (ncol(myData) == 10 + user_id){ # don't run if wrong number of columns
    myData <- as_tibble(myData)
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
       ) %>% transmute(
          usable = usable * use_rescale,
          learnable = learnable * learn_rescale,
          sus = sus * main_rescale
         )
     } else {
       holder <- holder %>% transmute(sus = sus * main_rescale)
     }
     if (user_id){
       holder <- holder %>% add_column(id_list, .before=1)
     }
     return(holder)
  } else {
   print("wrong number of columns in data")
  }
}

#' Calculate participants' scores on technology acceptance model
#'
#' Calculate participants' scores on full technology acceptance model.  There
#'   are two subscales: `perceived usefulness' and `perceived ease of use'.
#'   Currently assumes participant ID is first column (if present),
#'   then usefulness scale, then ease scale.
#'   It's based just off the original paper and no later extensions (yet).
#'
#' @param myData a data frame with (optional) participant ID,
#'   then (mandatory) usefulness scale questions followed by
#'   (mandatory) ease-of-use scale questions.
#' @param user_id boolean for presence or absence of user ID column as first
#'   column.
#' @param usef_start left-most column of usefulness scale, default is 1 
#'   higher than user_id.
#' @param ease_start left-most column of ease-of-use scale, default is 7 higher
#'   than user_id.
#' @concept questionnaire, scale
#' @references Davis, Fred D. (1989) ``Perceived Usefulness, Perceived Ease
#'   of Use, and User Acceptance of Information'' \emph{MIS Quarterly} 13 (3)
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
score_tam <- function(myData, user_id=TRUE,
  usef_start=user_id+1, ease_start=user_id+7){

  # coerce to tibble, for the sake of pull()
  myData <- as_tibble(myData)
  
  # rename variables for sake of later selection
  names(myData)[usef_start:(usef_start + 5)] <- 
    c("u1", "u2", "u3", "u4", "u5", "u6")
  names(myData)[ease_start:(ease_start + 5)] <- 
    c("e1", "e2", "e3", "e4", "e5", "e6")
  
  # calculate the scale results
  output_holder <- myData %>% mutate(
    usefulness = u1 + u2 + u3 + u4 + u5 + u6,
    ease = e1 + e2 + e3 + e4 + e5 + e6
    ) %>% select(usefulness, ease)
  if (user_id){
    output_holder <- output_holder %>%
      add_column(participant=pull(myData[, 1]), .before = 1)
  } 
  return(output_holder)
}

# 00000000000000000000000000000000000000000000000000000000000000000000000000000
# once a few been done, make an overall wrapper for them
# score_responses()
# 00000000000000000000000000000000000000000000000000000000000000000000000000000

#' Convert Likert items from text to number
#'
#' Returns an integer vector (1--5) if given text responses to Likert items.
#' 
#' Converts text responses to Likert items to integer responses.  Requires a
#' character or factor vector as an input; maps this onto the 1-5 scale to
#' create the output vector.  Will work for 5- and 4-point Likert items.
#' 
#' @param vector_in vector of responses to Likert item; default NULL.
#'   should be factor or (ideally) character.  Using a factor vector will
#'   return a message but will still work; any other type except characters
#'   will halt with an error.
#' @param text_finish logical; default FALSE.  If TRUE, will return abbreviated
#'   text instead of integers.  
#' @param exclude_middle logical; default FALSE.  Set to TRUE if working with
#'   a 4-point Likert item.  Not yet implemented.
#' @param full_caution logical; default FALSE.  If TRUE, will halt and throw an
#'   an error if there's any values that can't be converted.
#' @return integer vector 
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom purrr is_vector
#' @importFrom stringr str_squish
#' @importFrom stringr str_to_lower
clean_likert <- function(vector_in = NULL,
                         text_finish = FALSE,
                         exclude_middle = FALSE,
                         full_caution = FALSE
                        ){

  # check the input object is the right kind and fix if possible --------------
  # check there is an input object
  if (is.null(vector_in)) {
    stop("must give the function a character or factor vector to read")
  }

  # make sure input is a vector with is_vector()
  if (!is_vector(vector_in)) {
    stop("this function needs a character or factor vector")
  }
  
  # if factor, coerce to character
  if (class(vector_in) == "factor") {
    vector_in <- as.character(vector_in)
    message("the input was a factor; this function can work\n ",
      "but you might want to change the vector's class")
  }
  
  # stop if the input class isn't character
  if (!class(vector_in) == "character") {
    stop("input must be a character or factor vector")
  }
  # ---------------------------------------------------------------------------

  # standardize character strings ---------------------------------------------
  # make strings lower-case and remove duplicated whitespace
  vector_abbrev <- vector_in %>%
    str_to_lower() %>%
    str_squish()
  
  # abbreviate 
  vector_abbrev <- case_when(
    vector_abbrev == "strongly agree" ~ "SA",
    vector_abbrev == "agree" ~ "A",
    vector_abbrev == "neither agree nor disagree" ~ "NAD", 
    vector_abbrev == "disagree" ~ "D", 
    vector_abbrev == "strongly disagree" ~ "SD"
  ) 
  
  # check how many failed to abbreviate
  fault_count <- sum(is.na(vector_abbrev))
  # ---------------------------------------------------------------------------

  # if full_caution, stop on faults; else report count of faults --------------
  if (fault_count > 0) {
    # stop and report error count if full_caution TRUE
    if (full_caution) {
      stop("there were", fault_count, "failures to convert text;\n ",
        "either fix the input or set full_caution to FALSE")
    } else {
      warning("there were", fault_count, "failures to convert text;\n ",
        "you should check the input and output")
    }
  }
  # ---------------------------------------------------------------------------


  # convert characters to numbers ---------------------------------------------
  # end: fully converted into numbers; turn oddities into NA.
  vector_out <- case_when(
    vec_abbrev == "SA"  ~ 5,
    vec_abbrev == "A"   ~ 4,
    vec_abbrev == "NAD" ~ 3,
    vec_abbrev == "D"   ~ 2,
    vec_abbrev == "SA"  ~ 1
    # TRUE                ~ NA_real_ # it will NA unrecognised anyway
  )
  # ---------------------------------------------------------------------------

  # print count of NA if > 0 --------------------------------------------------
  if (sum(is.na(vector)) > 0) {
    print(sum(is.na(vector)), "NAs produced")
  }
  # ---------------------------------------------------------------------------

  # return vector_out, a numeric vector ---------------------------------------
  return(vector_out)
}
