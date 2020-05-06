#' Determine a hypothesis about Ea and Eb
#' 
#' Use to test statements like "Hacking actions were more common than Malware"
#' For example, you might run:
#' vcdb %>%
#'     dplyr::filter(plus.dbir_year == 2018) %>%
#'     dplyr::filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'     verisr::getenumCI("action")
#' and get:
#'            enum   x   n    freq
#' 1         Error 130 448 0.29018
#' 2        Misuse 123 448 0.27455
#' 3       Hacking  87 448 0.19420
#' 4      Physical  82 448 0.18304
#' 5        Social  42 448 0.09375
#' 6       Malware  25 448 0.05580
#' 7 Environmental   0 448 0.00000
#' 8       Unknown   6  NA      NA
#' 
#' You want to write in a report "Errors are more common in breaches
#' than Misuse.", but how do you validate that? You run:
#'
#'  chunk %>%
#'    verisr::test_veris_hypothesis("action.Error", "action.Misuse", "greater")
#'
#' which would return `FALSE` as they are simply too to be significantly different
#' 
#' Technically instead of 'true/false', the language should really 
#' be along the lines of "we have evidence for the alternative  
#' hypothesis ..." or "we do not have evidence to go against our 
#' original null hypothesis ...", but for simplicity we have left
#' it the way it is.
#' 
#' WARNING: This currently only works with 'logical' columns
#' 
#' @param chunk getenumCI() object
#' @param Ea Enumeration A.  e.g. "action.Error"
#' @param Eb Enumeration B.  e.g. "action.Misuse"
#' @param direction the direction to test ("greater" or "less")
#' @param ci.level the confidence level to test against
#' @param reps number of simulations to conduct
#' @param quietly do not produce textual output
#' @param visualize produce visual output
#' @return a logical TRUE/FALSE to the hypothesis
#' @export
#' @examples 
#' \dontrun{
#' tmp <- tempfile(fileext = ".dat")
#' download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
#' load(tmp, verbose=TRUE)
#' # test "Errors are more common in breaches than Misuse.
#' vcdb %>%
#'    filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'    verisr::getenumCI2020("action") %>%
#'    verisr::test_veris_hypothesis("Error", "Misuse", "greater")
#' # test "Partner actors are less common in breaches than external actors"
#' vcdb %>%
#'    filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'    verisr::getenumCI2020("actor") %>%
#'    verisr::test_veris_hypothesis("External", "Partner", "greater")
#' }
test_veris_hypothesis <- function(chunk, Ea, Eb, direction, ci.level=0.05, reps=1000, quietly=FALSE, visualize=FALSE) {
  
  if (length(setdiff(c(Ea,Eb), chunk$enum)) > 0) {
    stop("Both 'Ea' and 'Eb' must both be in the 'enum' column of 'chunk'.")
  }
  if (length(setdiff(c('x', 'n'), names(chunk))) > 0) {
    stop("Both 'x' and 'n' must be columns in 'chunk'.")
  }
  
  chunk <- chunk[chunk$enum %in% c(Ea,Eb), c('enum', 'x', 'n')]
  
  if (nrow(chunk) > 2) {stop("Ensure there is only 1 row for Ea and Eb.")}
  
  if ( any(is.na(c(chunk[["x"]], chunk[["n"]]))) ) {
    stop("Both Ea and Eb must have numbers for x and n.  If this is a small sample size, you may need to set 'force=TRUE' in verisr::getenumCI()")
  }
  
  if ( any((chunk[['n']] - chunk[['x']]) < 0) ) { stop("'x' must be larger than 'n' for both Ea and Eb.") }
  
  enum_order <- c(Ea, Eb)
  
  ## 3) Calculate the statistics
  # create samples
  chunk <- do.call(rbind, lapply(c(Ea,Eb), function(enum) {
    data.frame(enum = rep(enum, chunk[chunk$enum == enum, ][['n']]), result = c(rep(TRUE, chunk[chunk$enum == enum, ][['x']]), rep(FALSE, chunk[chunk$enum == enum, ][['n']] - chunk[chunk$enum == enum, ][['x']])))
  })) 
  # calculate observed stat, the difference in percentages between the two enums
  # obs_diff_prop is the observed difference proportion. The difference between the two enum percents
  obs_diff_prop <- infer::specify(chunk, formula = result ~ enum, success = "TRUE")
  # below replace enum1 with the greater enum and enum2 with the lesser enum
  obs_diff_prop <- infer::calculate(obs_diff_prop, stat = "diff in props", order = enum_order)
  # calculate the null distribution, assuming no difference in the median between the two enums
  # the null distribution is the distribution of enum percent differences if they were really the same
  null_distribution <- infer::specify(chunk, formula = result ~ enum, success = "TRUE")
  # 'independence' for testing enum1 > enum2
  null_distribution <- infer::hypothesize(null_distribution, null = "independence")
  null_distribution <- infer::generate(null_distribution, rep = reps, type = "permute")
  null_distribution <- infer::calculate(null_distribution, stat = "diff in props", order = enum_order)
  
  ## 4) Viauzize the results
  if (visualize) {
    gg <- infer::visualize(null_distribution, bins=20)
    gg <- gg + infer::shade_p_value(obs_stat = obs_diff_prop, direction = direction)
    grid::grid.draw(gg)
  }
  
  ## 5) calculate the p-value and compare to the significance level (`a`)
  p_value <- infer::get_p_value(null_distribution, obs_stat = obs_diff_prop, direction = direction)
  
  if (!quietly) {
    message(paste0("The hypothesis is ", p_value$p_value < ci.level,  " because the p value of ", scales::percent(p_value$p_value, accuracy=0.01), 
                   " is ", ifelse(p_value <= ci.level, "less than or equal to", "greater than"), 
                   " the confidence level of ", scales::percent(ci.level, accuracy=1), "."))
  }
  
  return(p_value$p_value < ci.level)
}


#' Determine if Ea is greater than/less than/equal to a set value
#' 
#' Use to test statements like "Everything Else is greater than 20% of breaches"
#' chunk <- vcdb %>%
#'      dplyr::filter(plus.dbir_year %in% 2019) %>%
#'      dplyr::filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'      verisr::getenumCI2020("pattern.Everything Else", force=TRUE, ci.method="bootstrap") 
#' chunk
#'              enum   x    n    freq    method   lower   upper
#' 1 Everything Else  42  271 0.15498 bootstrap  0.1141 0.19982
#' dbirR::test_veris_proportion(chunk, "Everything Else", .11, "greater")
#' The hypothesis is TRUE because the confidence of 98.90% at 11% is greater than the confidence level of 95%.
#' [1] TRUE
#' 
#' You want to write in a report "Everything else is equal to 21% of breaches.
#' chunk <- vcdb %>%
#'      dplyr::filter(plus.dbir_year %in% 2019) %>%
#'      dplyr::filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'      verisr::getenumCI2020("pattern.Everything Else", force=TRUE, ci.method="bootstrap") 
#' chunk
#'              enum   x    n    freq    method   lower   upper
#' 1 Everything Else  42  271 0.15498 bootstrap  0.1141 0.19982
#' dbirR::test_veris_proportion(chunk, "Everything Else", .15, "equal")
#' The hypothesis is TRUE because the proportion of 15% is in the 95% range between 11.44% and 19.93%.
#' [1] TRUE
#' 
#' Technically instead of 'true/false', the language should really 
#' be along the lines of "we have evidence for the alternative  
#' hypothesis ..." or "we do not have evidence to go against our 
#' original null hypothesis ...", but for simplicity we have left
#' it the way it is.
#' 
#' WARNING: This currently only works with 'logical' columns
#' 
#' @param chunk getenumCI() object
#' @param Ea Enumeration A.  e.g. "action.Error"
#' @param direction the direction to test ("greater", "less", )
#' @param prop the value to test against
#' @param ci.level the confidence level to test against
#' @param reps number of simulations to conduct
#' @param quietly do not produce textual output
#' @param visualize produce visual output
#' @return a logical TRUE/FALSE to the hypothesis
#' @export
#' @examples 
#' \dontrun{
#' tmp <- tempfile(fileext = ".dat")
#' download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
#' load(tmp, verbose=TRUE)
#' vcdb %>%
#'   dplyr::filter(plus.dbir_year %in% 2019) %>%
#'   dplyr::filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'   verisr::getenumCI2020("pattern.Everything Else", force=TRUE, ci.method="bootstrap") %>%
#'   verisr::test_veris_proportion("Everything Else", .11, "greater")
#' }
test_veris_proportion <- function(chunk, Ea, prop, direction, ci.level=0.05, reps=1000, quietly=FALSE, visualize=FALSE) {
  
  if (!Ea %in% chunk$enum) {
    stop("'Ea' must both be in the 'enum' column of 'chunk'.")
  }
  if (length(setdiff(c('x', 'n'), names(chunk))) > 0) {
    stop("Both 'x' and 'n' must be columns in 'chunk'.")
  }
  
  if (direction == "equal") {direction <- "two_sided"}
  
  conf.level <- 1 - ci.level # this is useful for our directional portion
  
  ### Build the chunk
  chunk <- chunk[chunk$enum == Ea, c('enum', 'x', 'n')]
  
  if (nrow(chunk) > 1) {stop("Ensure there is only 1 row for Ea.")}
  
  if ( any(is.na(c(chunk[["x"]], chunk[["n"]]))) ) {
    stop("Ea must have numbers for x and n.  If this is a small sample size, you may need to set 'force=TRUE' in verisr::getenumCI()")
  }
  
  if ( any((chunk[['n']] - chunk[['x']]) < 0) ) { stop("'x' must be larger than 'n' for Ea.") }
  
  # create samples
  chunk <- data.frame(enum=Ea,  result = c(rep(TRUE, chunk[chunk$enum == Ea, ][['x']]), rep(FALSE, chunk[chunk$enum == Ea, ][['n']] - chunk[chunk$enum == Ea, ][['x']])))
  
  ### 3. Calculate statistics 
  # specify that we variables to use and what their levels mean
  bootstrap_distribution <- infer::specify(chunk, response = result, success="TRUE")
  # sample with replacement (bootstrap) to create our bootstrap distributions
  bootstrap_distribution <- infer::generate(bootstrap_distribution, reps = reps, type = "bootstrap")
  # calculate a proportion per bootsrap distribution rep
  bootstrap_distribution <- infer::calculate(bootstrap_distribution, stat = "prop") 
  
  
  ## 4. Viauzize the results
  if (visualize) {
    gg <- infer::visualize(bootstrap_distribution, bins=20)
    gg <- gg + infer::shade_p_value(obs_stat = prop, direction = direction)
    grid::grid.draw(gg)
  }
  
  # 5. compare the bootstrap distribution of proportions to our observed stat (the stat we want to prove greater/less/etc than)
  p_value <- infer::get_p_value(bootstrap_distribution, obs_stat = prop, direction = direction)
  
  percentile_ci <- infer::get_confidence_interval(bootstrap_distribution, level=conf.level, type="percentile")
  
  if (!quietly) {
    if (direction == "two_sided") {
      message(paste0("The hypothesis is ", prop >= percentile_ci[[1]] & prop <= percentile_ci[[2]], " because the proportion of ", scales::percent(prop, accuracy=1),
                     " is", ifelse(prop >= percentile_ci[[1]] & prop <= percentile_ci[[2]], " ", " not "),  "in the ", scales::percent(1-ci.level, accuracy=1), " range between ",  scales::percent(percentile_ci[[1]], accuracy=0.01), " and ",
                     scales::percent(percentile_ci[[2]], accuracy=0.01), "."))
      return(prop >= percentile_ci[[1]] & prop <= percentile_ci[[2]])
    } else {
      message(paste0("The hypothesis is ", p_value$p_value >= conf.level,  " because the confidence of ", scales::percent(p_value$p_value, accuracy=0.01), 
                     " at ", scales::percent(prop, accuracy=1), " is ", ifelse(p_value <= ci.level, "less than or equal to", "greater than"), 
                     " the confidence level of ", scales::percent(conf.level, accuracy=1), "."))
      return(p_value$p_value >= conf.level)
    }
    
    error("This code location should not be reached.")
    return(NULL)
  }
  
}


#' Test whether two enumerations are basically equal
#' 
#' This is most useful for testing an enumerations whether an 
#' enumeration has stayed the same since last year.  It 
#' functions by creating an null distribution around the 
#' observed proportion, and then checking the probability of 
#' the proportion being 0 (i.e. no change).
#' 
#' Technically instead of 'true/false', the language should really 
#' be along the lines of "we have evidence for the alternative  
#' hypothesis ..." or "we do not have evidence to go against our 
#' original null hypothesis ...", but for simplicity we have left
#' it the way it is.
#' 
#' @param chunk getenumCI() object
#' @param Ea Enumeration A.  e.g. "action.Error"
#' @param Eb Enumeration B.  e.g. "action.Misuse"
#' @param ci.level the confidence level to test against
#' @param reps number of simulations to conduct
#' @param quietly do not produce textual output
#' @param visualize produce visual output
#' @return a logical TRUE/FALSE to the hypothesis
#' @export
#' @examples 
#' \dontrun{
#' tmp <- tempfile(fileext = ".dat")
#' download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
#' load(tmp, verbose=TRUE)
#' vcdb %>%
#'   filter(timeline.incident.year %in% 2015:2020) %>%
#'   filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'   verisr::getenumCI2020(
#'     "attribute.confidentiality.data.variety", 
#'     by="plus.dbir_year") %>%
#'   filter(!is.na(by)) %>%
#'   filter(enum == "Bank")  %>%
#'   select(-enum) %>%
#'   rename(enum = by)  %>%
#'   verisr::test_veris_consistency(Ea="2019", Eb="2020")
#' }
test_veris_consistency <- function(chunk, Ea, Eb, ci.level=0.05, reps=1000, quietly=FALSE, visualize=FALSE) {
  
  
  enum_order = c(Ea, Eb)
  
  
  samples <- do.call(rbind, lapply(enum_order, function(enum){
    data.frame(enum=rep(enum, chunk[chunk$enum==enum,][['n']]), 
               result = c(rep(TRUE, chunk[chunk$enum==enum,][['x']]),
                          rep(FALSE, chunk[chunk$enum==enum,][['n']] - chunk[chunk$enum==enum,][['x']])))
  }))
  
  
  null_distribution <- infer::specify(samples, formula = result ~ enum, success="TRUE")
  null_distribution <- infer::generate(null_distribution, rep=reps, type = "bootstrap")
  null_distribution <- infer::calculate(null_distribution, stat="diff in props", order= enum_order)
  
  if (visualize) {
    gg <- ggplot2::ggplot()
    gg <- gg + ggplot2::geom_density(ggplot2::aes(x=null_distribution$stat))
    gg <- gg + ggplot2::geom_vline(ggplot2::aes(xintercept=0), color="red")
    grid::grid.draw(gg)
  }
  
  #    p_value <- infer::get_p_value(null_distribution, obs_diff_prop, direction="two_sided") %>%
  #        pull(p_value)
  
  p_value <- infer::get_p_value(null_distribution, 0, direction="two_sided") %>%
    pull(p_value)
  
  if (!quietly) {
    message(paste0("The hypothesis that the values ", Ea, " and ", Eb, " are equal is NOT ", p_value <= ci.level, 
                   " because the probability of getting a value more extreme than 0 by random chance of ",
                   scales::percent(p_value), " is ", ifelse(p_value <= ci.level, "less", "greater"),  " than our threshold of ", scales::percent(ci.level)))
  }
  
  return(!(p_value <= ci.level))
  
}


#' Determine if the enumeration is changing over time
#' 
#' This is used by analyzing a dataset over time for an enumeration.
#' It functions by fitting a straight line to the values and 
#' checking if the confidence interval on the slope contains zero.
#' 
#' Technically instead of 'true/false', the language should really 
#' be along the lines of "we have evidence for the alternative  
#' hypothesis ..." or "we do not have evidence to go against our 
#' original null hypothesis ...", but for simplicity we have left
#' it the way it is.
#' 
#' @param chunk getenumCI() object
#' @param Ea Enumeration A.  e.g. "Error"
#' @param direction "greater" for increasing, "less" for decreasing, "equal" for "not changing"
#' @param stable DEPRECIATED.  USE 'direction="equal"'. TRUE means "hypothesis is 'not changing'". FALSE means "hypothesis is 'changing'"
#' @param quietly do not produce textual output
#' @param visualize produce visual output
#' @return a logical TRUE/FALSE to the hypothesis
#' @export
#' @examples 
#' \dontrun{
#' tmp <- tempfile(fileext = ".dat")
#' download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
#' load(tmp, verbose=TRUE)
#' chunk <- vcdb %>%
#'   filter(timeline.incident.year %in% 2015:2020) %>%
#'   filter(attribute.confidentiality.data_disclosure.Yes) %>%
#'   verisr::getenumCI2020("pattern.Lost and Stolen Assets", 
#'     by="plus.dbir_year", 
#'     ci.method="bootstrap", force= TRUE) %>%
#'   filter(!is.na(n))
#'   
#'   chunk
#'   
#'   verisr::test_veris_time_stability(chunk, "Lost and Stolen Assets", stable=TRUE)
#' }
test_veris_time_stability <- function(chunk, Ea, stable=NULL, direction=NULL, quietly=FALSE, visualize=FALSE) {
  
  if (is.null(direction) & !is.null(stable)) {
    if (stable) { direction <- "equal" }
  }
  
  if (is.null(direction) | ! direction %in% c("equal", "greater", "less")) {
    stop("Please specify a direction. Use 'equal' for not changing.")
  }
  
  chunk <- chunk[chunk$enum == Ea, ]
  
  if (any(table(chunk$by) != 1)) {stop("There must only be one measurement per year.")}
  
  prop_model <- stats::lm(freq ~ by, data = chunk)
  
  ret <- moderndive::get_regression_table(prop_model)
  
  if (visualize) {
      gg <- ggplot2::ggplot()
      gg <- gg + ggplot2::geom_density(
        ggplot2::aes(x=slope), 
        data=data.frame(
                slope = rnorm(1000, 
                              as.double(ret[ret$term == "by", "estimate"]), 
                              as.double(ret[ret$term == "by", "std_error"]) * sqrt(nrow(chunk)))
              ),
        fill="grey85"
      ) 
      gg <- gg + ggplot2::geom_vline(ggplot2::aes(xintercept=0), color="red")
  }
  

  if (direction == "equal") {
    if (!quietly) {
      res <- as.logical(0 >= ret[ret$term == "by", 'lower_ci'] & 0 <= ret[ret$term == "by", 'upper_ci'])
      message(paste0("The hypothesis of stability is NOT ",
                     !res, 
                     " because the confidence interval of ", 
                     scales::percent(as.double(ret[ret$term == "by", 'lower_ci']), accuracy=0.01), " to ", 
                     scales::percent(as.double(ret[ret$term == "by", 'upper_ci']), accuracy=0.01), " contains 0")
      )
    }
    return(res)
  } else if (direction == "greater") {
    if (!quietly) {
      res <- as.logical(0 < ret[ret$term == "by", 'lower_ci'])
      message(paste0("The hypothesis of increasing values is ",
                     res, 
                     " because the confidence interval of ", 
                     scales::percent(as.double(ret[ret$term == "by", 'lower_ci']), accuracy=0.01), " to ", 
                     scales::percent(as.double(ret[ret$term == "by", 'upper_ci']), accuracy=0.01), " is", 
                     ifelse(res, " ", " not "),  "greater than 0")
      )
    }
    return(res)
  } else if (direction == "less") {
    if (!quietly) {
      res <- as.logical(0 > ret[ret$term == "by", 'upper_ci'])
      message(paste0("The hypothesis of increasing values is ",
                     as.logical(0 > ret[ret$term == "by", 'upper_ci']), 
                     " because the confidence interval of ", 
                     scales::percent(as.double(ret[ret$term == "by", 'lower_ci']), accuracy=0.01), " to ", 
                     scales::percent(as.double(ret[ret$term == "by", 'upper_ci']), accuracy=0.01), " is", 
                     ifelse(res, " ", " not "),  "less than 0")
      )
    }
    return(res)
  }
  
  error("This code location should not be reached.")
  return(NULL)
}