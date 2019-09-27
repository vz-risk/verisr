#' Convert logit to probability
#' @param logit a vector of logits
#' @return a vector of probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}