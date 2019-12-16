#' Summarizes veris enumerations from verisr objects
#' 
#' This is the primary analysis function for veris.  It conducts binomial
#'     hypothesistests on veris data to enumerate the frequency of a given 
#'     enumeration or set of enumerations within a feature. (For example, 
#'     'Malware', 'Hacking', etc within 'action').
#'     
#' The 'by' parameter allows enumerating one feature by another, (for example
#'    to count the frequency of each action by year).
#' 
#' Unknowns are generally excluded as 'not tested'.  If 'NA' is an enumeration
#'    in the feature being enumerated, it must be specified with the 'na.rm'
#'    parameter as whether NA should be included or not is highly dependent on
#'    the hypothesis being tested.
#'    
#' This function accurately enumerates single logical columns, character 
#'     feature columns, and features spanning multiple logical columns (such as 
#'     action.*).  It cannot enumerate free-form text columns.  It accurately 
#'     calculates the sample size 'n' as the number of rows (independent of the
#'     number of enumerations present in the feature).
#'     
#' GetenumCI() can also provide binomial confidence intervals for the 
#'     enumerations tested within the features.  See the parameters for details.
#'     
#' While getenumCI() may work on other types of dataframes, it was designed for
#'     verisr dataframes and data.tables.  It is not tested nor recommended for
#'     any other type.
#' 
#' @param veris A verisr object
#' @param enum A veris feature or enumeration to summarize
#' @param by A veris feature or enumeration to group by
#' @param na.rm A boolean of whether to include not applicable in the sample set.
#'     This is REQUIRED if enum has a potential value of NA as there is no 
#'     'default' method for handling NAs.  Instead, it depends on the 
#'     hypothesis being tested.
#' @param unk A boolean referring whether to include 'unknown' in the sample.
#'     The default is 'FALSE' and should rarely be overwritten.
#' @param short.names A boolean identifying whether to use the full enumeration
#'     name or just the last section. (i.e. action.hacking.variety.SQLi vs
#'     just SQLi.)
#' @param top Integer limiting the output to top enumerations.
#' @param ci.method A confidence interval method to use.  Options are "mcmc" or "bootstrap".  "bootstrap"uses the bayes process from the binom package.  "mcmc" uses a binomial model based on rstan, rstanarm, brms.
#' @param cred.mass the amount of probability mass that will be contained in 
#'   reported credible intervals. This argument fills a similar role as 
#'   conf.level in \code{\link{binom.test}}.
#' @param round.freq An integer indicating how many places to round
#'     the frequency value to. (default = 5)
#' @param na DEPRECIATED! Use '\code{na.rm}' parameter.
#' @param ci.level DEPRECIATED! same as \code{cred.mass}.
#' @param force getenumCI() will attempt to enforce sane confidence-based practices (such as hiding x and freq in low sample sizes).  Setting force to 'TRUE' will override these best practices.
#' @param ... A catch all for functions using arguments from previous
#'     versions of getenum.
#' @return A data frame summarizing the enumeration
#' @export
#' @examples 
#' tmp <- tempfile(fileext = ".dat")
#' download.file("https://github.com/vz-risk/VCDB/raw/master/data/verisr/vcdb.dat", tmp, quiet=TRUE)
#' load(tmp, verbose=TRUE)
#' chunk <- getenumCI(vcdb, "action.hacking.variety")
#' chunk
#' chunk <- getenumCI(vcdb, "action.hacking.variety", top=10)
#' chunk <- getenumCI(vcdb, "action.hacking.variety", by="timeline.incident.year")
#' chunk
#' chunk <- getenumCI(vcdb, 
#'                    "action.hacking.variety", 
#'                    by="timeline.incident.year") 
#' reshape2::acast(chunk, by~enum, fill=0)
#' getenumCI(vcdb, "action")
#' getenumCI(vcdb, "asset.variety")
#' getenumCI(vcdb, "asset.assets.variety")
#' getenumCI(vcdb, "asset.assets.variety", ci.method="wilson")
#' getenumCI(vcdb, "asset.cloud", na.rm=FALSE)
#' getenumCI(vcdb, "action.social.variety.Phishing")
#' getenumCI(vcdb, "actor.*.motive", ci.method="wilson", na.rm=FALSE)
#' rm(vcdb)
getenumCI2020 <- function(veris, 
                      enum, 
                      by=NULL,
                      na.rm = NULL, 
                      unk=FALSE, 
                      short.names=TRUE, 
                      ci.method=c(), 
                      cred.mass=0.95,
                      ci.level=NULL, 
                      round.freq=5, 
                      na = NULL, 
                      top = NULL,
                      force = FALSE,
                      ...) {
  # Below this value for 'n', apply ci best practices from https://github.com/vz-risk/dbir-private/issues/43 unless force = TRUE
  ci_n <- 30 # chosen semi-arbitrarily, but it is roughly where frequentist normal approximations break down.
  n_floor <- 5 # chosen semi-arbitrarily.  return empty dataframe
  
  # even though the parameter is 'na.rm', we still use 'na' internally.
  if (!is.null(na.rm)) {
    na = !na.rm  # if na.rm is set, change na to it. (na is the logical opposit of na.rm)
  } else if (!is.null(na)) {
    warning("'na' is depriciated.  please use 'na.rm'.")
  }
  
  # legacy veris objects are data tables, however data tables cause problems.
  if (data.table::is.data.table(veris)) {
    df <- as.data.frame(veris)
  } else {
    df <- veris
  }

  #   
  if (!is.null(ci.level)) {
    cred.mass <- ci.level
  }
  
  # because we aren't keeping the 'method' and don't want to duplicate rows for each method, only 1 allowed.
  if (length(ci.method) > 1) {
    warning("More than one confidence interval method specified. Using first.")
    ci.method <- ci.method[1]
  }
  
  if (!is.null(ci.method) && !ci.method %in% c("mcmc", "bootstrap")) {
    stop(paste0("ci.method ", ci.method, " not one of c('mcmc', 'bootstrap')."))
  }
  
  if (ci.method == "mcmc" && length(intersect(c("brms", "tidybayes"), rownames(installed.packages()))) < 2) {
    ci.method <- "bootstrap"
    warning("ci.method set to mcmc, but 'brms' and 'tidybayes' not both installed.  updating ci.method to 'bootstrap'")
  }
  
  # create a list of enumerations to calculate 'enum' _by_
  if (!is.null(by)) {
    by_enums <- grep(paste0("^",by,"[.][A-Z0-9][^.]*$"), names(df), value=TRUE)
    if (length(by_enums) > 0) {
      by_type <- "multinomial"
      # by_class <- "character"
    } else {
      by_enums <- grep(paste0("^",by,"$"), names(df), value=TRUE)
      if (length(by_enums) == 1) { # could be > 0, but this should help throw errors when not functioning properly. - gdb 090116
        by_enums <- unique(df[[by]])
        by_type <- "single_column"
        # by_class <- class(by_enums)
      } else {
        warning(paste0("No column matching 'by' value ", by, ". Ignoring 'by' value."))
        by_enums <- c(NA)
        by_type <- "none"
        # by_class <- "character"
      }
    }
  } else {
    by_enums <- c(NA)
    by_type <- "none"
    # by_class <- "character"
  }
  
  # we use do.call because we don't know how many things we'll be rbinding.
  #  instead, we just get a list of them using lapply and then rbind
  #  on that list.
  # TODO: Parallelize this as time scales linearly with the number of 'by_enums'
  #chunk <- do.call(rbind, lapply(by_enums, function(x) { # switching as you can end up with the wrong number of columns if CI protection triggers on one subchunk and not another - GDB 201216
  chunks_list <- lapply(by_enums, function(x) {
    
    # subset DF to just the portion we're currently dealing with
    if (by_type == "multinomial") {
      subdf <- df[df[[x]], ]
    } else if (by_type == "single_column") {
      subdf <- df[df[[by]] == x, ]
    } else { # catchall for by_type == 'none', i.e. keep the whole df
      subdf <- df
    }
    
    # select the columns that match the enumeration and characterize it's/their type
    enum_enums <- grep(paste0("^",enum,"[.][A-Z0-9][^.]*$"), names(subdf), value=TRUE)
    if (length(enum_enums) > 0) {
      enum_type <- "multinomial"
    } else {
      enum_enums <- grep(paste0("^",enum,"$"), names(subdf), value=TRUE)
      if (length(enum_enums) == 1) { # could be > 0, but this should help throw errors when not functioning properly. - gdb 090116
        if (is.logical(subdf[[enum_enums]])) {
          enum_type <- "logical"
          short_names <- gsub('^.*[.]([^.]+$)', "\\1", names(subdf))
          logical_enum <- enum_enums
          enum_enums <- grep(paste0("^", gsub('^(.*)[.]([^.]+$)', "\\1", logical_enum), "[.][A-Z0-9][^.]*$"), names(subdf), value=TRUE)
        } else {
          enum_type <- "single_column"
        }
      } else {
        stop(paste0("Enum ", enum, " did not resolve to any columns."))
      }
    }
      
    ## This entire section calculates the sample size.  We do it early so we can adjust 'top' if n < ci_n  
    # This allows us to handle numerical/factor/character and logical enumerations
    if (enum_type == "multinomial" | enum_type == "logical") {
      subdf_for_n <- subdf[, enum_enums]
      
      if (ncol(subdf_for_n) <= 0) { stop(paste(c("No columns matched feature(s) ", enum, " using regex ", paste0("^",enum,"[.][A-Z0-9][^.]*$"), collapse=" ")))}
      
      # we remove unknowns because they should normally not be counted
      if (unk == FALSE) {
        if (short.names) {
          subdf_for_n <- 
            subdf_for_n <- subdf_for_n[, enum_enums][, !grepl("^(.+[.]|)(U|[A-Za-z]{1,3} - [U|u])nknown$", names(subdf_for_n))] # if short names, bla - unknown is removed. See logical section for why. - GDB 17-01-30
        } else {
          subdf_for_n <- subdf_for_n[, !grepl("^(.+[.]|)(U|u)nknown$", names(subdf_for_n))] # if long names, bla - unknown is kept in sample. See logical section for why. - GDB 17-01-30
        }
      }
      
      # Whether to use NAs or not depends on the hypothesis being tested so we require an answer (no default)
      if (is.null(na) & any(grep("[.]NA$", names(subdf_for_n)))) { stop("'na' must be specified if any column names end in .NA")}
      if (!is.null(na)) {
        if (na == FALSE) {
          subdf_for_n <- subdf_for_n[, !grepl(".NA$", names(subdf_for_n)), ]
        }
      }
      
      # number of records that have one of our enumerations
      n <- sum(rowSums(subdf_for_n, na.rm=TRUE) > 0, na.rm=TRUE)
      # count of each enumeration
    } else if (enum_type == "single_column") {
      table_v <- table(subdf[[enum_enums]])
      v <- as.integer(table_v)
      names(v) <- names(table_v)
      
      n <- sum(v, na.rm=TRUE)
      # remove unknowns
      if (unk == FALSE) {
        n <- n - sum(v[grepl("^(.+[.]|)(U|u)nknown$", names(v))], na.rm=TRUE) # Doesn't handle `Bla - unknown` as these bastardized hierarchies shouldn't be in a single character column. - gdb 17-01-30
      }
      
      # remove NAs
      if (!is.null(na)) {
        if (na == FALSE) {
          n <- n - sum(v[grepl("^(.+[.]|)NA$", names(v))], na.rm=TRUE)
        }
      }
    } else {
      stop("class of 'enum' column(s) was not identified, preventing summarization.")
    }
      
    # if we aren't rocing and 'top' was set and the sample size was less than ci_n, rerun but with 'top' off
    if (!force && !is.null(top) && top > 1 && n < ci_n) {
      top <- NULL
      warning(paste0("Parameter 'top' ignored when 'n' < ", ci_n, ".  'Top' can only be used if there is a clear break where confidence intervals between two ordered records don't overlap.  Please calculate interdependance, set  'top' to the appropriate breakpoint, and use 'force=TRUE' to avoid this warning."))
    }
    
    # Subset to top enums
    if (!is.null(top)) {
      if (top < 1) {
        stop(paste0("Top must be 1 or greater, but is (", top, ")."))
      }
      
      if (enum_type == "logical") {
        warning(paste0("Parameter 'top' incompatible with single logical column enumeration ", enum_enums, ". Skipping filtering to top."))

      } else {
        # start by getting a count of each enumeration
        if (enum_type == "single_column") {
          enum_counts <- table(subdf[[enum_enums]])
        } else if (enum_type == "multinomial") {
          enum_counts <- colSums(subdf[ , enum_enums])
        } else {
          stop("class of 'enum' column(s) was not identified, preventing filtering of top items and further processing")
        }
        
        # Remove things that should not be a top enumeration.  This includes 'Other', 'Unknown', and 'na' (if na.rm=TRUE)
        enum_counts <- enum_counts[!grepl("^(.+[.]|)(O|o)ther$", names(enum_counts))]
        if (!is.null(unk)) {
          if (unk == FALSE) {
            enum_counts <- enum_counts[!grepl("^(.+[.]|)(U|u)nknown$", names(enum_counts))]
          }
        }
        if (!is.null(na)) {
          if (na == FALSE) {
            enum_counts <- enum_counts[!grepl("^(.+[.]|)NA$", names(enum_counts))]
          }
        }
        
        # order the enumerations and take the top ones
        # top enums are the actual top enums, plus 'Other', 'Unknown', and potentially NA
        top_enums <- names(enum_counts[rank(-enum_counts, ties.method="min") <= top]) # , grep("^(.+[.]|)(O|o)ther$", enum_enums, value=TRUE)
        top_enums <- intersect(top_enums, names(enum_counts[enum_counts > 0])) # attempt 
        if (!is.null(unk)) {
          if (unk == FALSE) {
            top_enums <- c(top_enums, grep("^(.+[.]|)(U|u)nknown$", enum_enums, value=TRUE))
          }
        }
        if (!is.null(na)) {
          if (na == FALSE) {
            top_enums <- c(top_enums, grep("^(.+[.]|)NA$", enum_enums, value=TRUE))
          }
        }
        
        # when we assign 'other' to things not in the top enum list, make sure to not assign it to blank or NA as well
        not_top_enums <- setdiff(enum_enums, c(top_enums, ''))
        not_top_enums <- not_top_enums[!is.na(not_top_enums)]
        if (length(not_top_enums) > 0) {
          if (enum_type == "single_column") {
            subdf[grepl(paste0("^", paste(not_top_enums, collapse="|")), subdf[[enum_enums]]), enum_enums] <- "Other"
          } else {
            if (length(intersect(not_top_enums, names(subdf))) <= 0) {
              subdf[[paste0(enum, ".Other")]] <- FALSE
            } else if (length(intersect(not_top_enums, names(subdf))) == 1) {
              subdf[[paste0(enum, ".Other")]] <- subdf[[not_top_enums]] # this likely is assigning <enum>.Other to itself, but just in case we'll define it generically.
            } else {
              subdf[[paste0(enum, ".Other")]] <- unlist(apply(subdf[, not_top_enums], MARGIN=1, any))
              
            }
            enum_enums <- c(intersect(top_enums, enum_enums), paste0(enum, ".Other"))
          }
        }
      }
    }
    
    # This allows us to handle numerical/factor/character and logical enumerations
    if (enum_type == "multinomial" | enum_type == "logical") {
      subdf <- subdf[, enum_enums]
      
      if (ncol(subdf) <= 0) { stop(paste(c("No columns matched feature(s) ", enum, " using regex ", paste0("^",enum,"[.][A-Z0-9][^.]*$"), collapse=" ")))}
      
## 'n' now calcualted farther above to allow turning 'top' off for small 'n'.      
#      # we remove unknowns because they should normally not be counted
#      if (unk == FALSE) {
#        if (short.names) {
#          subdf_for_n <- subdf[, !grepl("^(.+[.]|)(U|[A-Za-z]{1,3} - [U|u])nknown$", names(subdf))] # if short names, bla - unknown is removed. See logical section for why. - GDB 17-01-30
#        } else {
#          subdf_for_n <- subdf[, !grepl("^(.+[.]|)(U|u)nknown$", names(subdf))] # if long names, bla - unknown is kept in sample. See logical section for why. - GDB 17-01-30
#        }
#      } else {
#        subdf_for_n <- subdf
#      }
#      
#      # Whether to use NAs or not depends on the hypothesis being tested so we require an answer (no default)
#      if (is.null(na) & any(grep("[.]NA$", names(subdf)))) { stop("'na' must be specified if any column names end in .NA")}
#      if (!is.null(na)) {
#        if (na == FALSE) {
#          subdf_for_n <- subdf_for_n[, !grepl(".NA$", names(subdf_for_n)), ]
#        }
#      }
#      
#      # number of records that have one of our enumerations
#      n <- sum(rowSums(subdf_for_n, na.rm=TRUE) > 0, na.rm=TRUE)
#      # count of each enumeration
      
      #  if short.names, combine columns with short names. (Rather than summing same short name after calculating column sums, which double-counts in 'x'.)
      if (short.names) {
        short_names <- gsub('^.*[.]([^.]+$)', "\\1", names(subdf))
        subdf <- do.call(cbind, lapply(unique(short_names), function(y) { # bind the list of columns returned by lapply
          dups <- grep(paste0("^(",y,")$"), short_names)
          if (length(dups) > 1) {
            #feature <- apply(subdf[ , grep(paste0("^(",y,")$"), short_names)], MARGIN=1, any) # 'grep' selects columns with the name, apply checks if any of the row are true
            feature <- apply(subdf[ , which(y == short_names)], MARGIN=1, any)  # replaced 'grep' with 'which' due to some 'y' containing regex control characters.
          } else {
            #feature <- subdf[ , grep(paste0("^(",y,")$"), short_names)]
            feature <- subdf[, which(y == short_names)] # replaced 'grep' with 'which' due to some 'y' containing regex control characters.
          }
          feature <- data.frame(feature)
          if (ncol(feature) > 0) {
            names(feature) <- y
          }
          feature
        }))
      }
      
      v <- colSums(subdf, na.rm=TRUE)  # used instead of a loop or plyr::count to compute x
      
    } else if (enum_type == "single_column") {
      table_v <- table(subdf[[enum_enums]])
      v <- as.integer(table_v)
      names(v) <- names(table_v)

# 'n' now calcualted above for removing 'top' if n < ci_n      
#      n <- sum(v, na.rm=TRUE)
#      # remove unknowns
#      if (unk == FALSE) {
#        n <- n - sum(v[grepl("^(.+[.]|)(U|u)nknown$", names(v))], na.rm=TRUE) # Doesn't handle `Bla - unknown` as these bastardized hierarchies shouldn't be in a single character column. - gdb 17-01-30
#      }
#      
#      # remove NAs
#      if (!is.null(na)) {
#        if (na == FALSE) {
#          n <- n - sum(v[grepl("^(.+[.]|)NA$", names(v))], na.rm=TRUE)
#        }
#      }
    } else {
      stop("class of 'enum' column(s) was not identified, preventing summarization.")
    }
    
    # create the chunk for this 'by'
    if (short.names) { # if short names, we treat `foo.Bar - unknown` as an unknown, similar to foo.bar.variety.Unknown being truncated to 'Unknown'
      subchunk <- data.frame(enum=names(v), x=v, n=rep(n, length(v)), freq=v/n)
      enum_subchunk <- subchunk[!grepl("^(.+[.]|)(U|[A-Za-z]{1,3} - [U|u])nknown$", subchunk$enum) & !grepl("^(.+[.]|)NA$", subchunk$enum), ]
      unk_subchunk <- subchunk[grepl("^(.+[.]|)(U|[A-Za-z]{1,3} - [U|u])nknown$", subchunk$enum), ]
      na_subchunk <- subchunk[grepl("^(.+[.]|)NA$", subchunk$enum), ]
    } else { # if long names, we include `foo.Bar - unknown` because we wouldn't truncate, and therefor would include, foo.bar.variety.Unknown
      subchunk <- data.frame(enum=names(v), x=v, n=rep(n, length(v)), freq=v/n)
      enum_subchunk <- subchunk[!grepl("^(.+[.]|)(U|u)nknown$", subchunk$enum) & !grepl("^(.+[.]|)NA$", subchunk$enum), ]
      unk_subchunk <- subchunk[grepl("^(.+[.]|)(U|u)nknown$", subchunk$enum), ]
      na_subchunk <- subchunk[grepl("^(.+[.]|)NA$", subchunk$enum), ]
    }
    
    # n is not applicable for Unknown (and potentially na) rows so zero it out
    if (unk == FALSE & nrow(unk_subchunk) > 0) {
      unk_subchunk[ , c("n", "freq")] <- NA
    }
    if (!is.null(na)) {
      if (na == FALSE & nrow(na_subchunk) > 0) {
        na_subchunk[ , c("n", "freq")] <- NA
      }
    }
    
    # Because we will remove 'x' and 'freq', there must be a ci.method set if n < ci_n and force != TRUE
    if (!force & n < ci_n) {
      if (length(ci.method) <= 0) {
        warning(paste0("ci.method must be set if 'n' < ", ci_n, " and force != TRUE.  Setting ci.method to 'bootstrap' for enumeration ", x, ".  To avoid this warning, please set 'ci.method' to either 'mcmc' or 'bootstrap' or force=TRUE."))
        ci.method <- "bootstrap"
      }
    }
    
    # apply the confidence interval.  Apply to NA's and unk separately depending on if selected. (If you try and apply CI's cart blanc to the NA/Unknowns it can error out on binding the columns)
    if (!is.null(ci.method) && ci.method == "bootstrap") {
      if (nrow(enum_subchunk) > 0) {
        # replacing the one-liner with this because binom.bayes has a bug that errors if the ends (0 or n) are included in x and number of rows > 3
        enum_subchunk[['method']] <- "bootstrap"
        enum_subchunk[['lower']] <- NA
        enum_subchunk[['upper']] <- NA
        if (any(enum_subchunk$x != n & enum_subchunk$x != 0)) { # check if any aren't 0 or n. otherwise, calculating a CI will fail.
          enum_subchunk[enum_subchunk$x != n & enum_subchunk$x != 0, c('lower', 'upper') ] <- binom::binom.bayes(as.integer(enum_subchunk[enum_subchunk$x != n & enum_subchunk$x != 0, 'x' ]), as.integer(enum_subchunk[enum_subchunk$x != n & enum_subchunk$x != 0, 'n' ], conf.level=cred.mass))[ , c(7, 8)]
        }  
        ## Commenting out the 'if' blocks as I _think_ they are unnecessary
        #if (0 %in% enum_subchunk$x) {
        enum_subchunk[enum_subchunk$x == 0, c('lower', 'upper')] <- binom::binom.bayes(0, n, conf.level=cred.mass)[, c(7, 8)]
        #}
        #if (n %in% enum_subchunk$x) {
        enum_subchunk[enum_subchunk$x == n, c('lower', 'upper')] <- binom::binom.bayes(n, n, conf.level=0.95)[, c(7, 8)]
        #}
        # enum_subchunk <- cbind(enum_subchunk, data.frame(method="bootstrap"), binom::binom.confint(enum_subchunk$x, enum_subchunk$n, conf.level=cred.mass, methods="bayes")[ , c(5, 6)])
      } else {
        enum_subchunk <- cbind(enum_subchunk, data.frame(method=character(), lower=numeric(), upper=numeric()))
      }
      if (unk == FALSE) {
        unk_subchunk <- cbind(unk_subchunk, data.frame(method=rep(NA, nrow(unk_subchunk)), lower=rep(NA, nrow(unk_subchunk)), upper=rep(NA, nrow(unk_subchunk))))
      } else if (nrow(unk_subchunk) >0) {
        unk_subchunk <- cbind(unk_subchunk, data.frame(method="bootstrap"), binom::binom.confint(unk_subchunk$x, unk_subchunk$n, conf.level=cred.mass, methods="bayes")[ , c(5, 6)])
      } else {
        unk_subchunk <- data.frame()
      }
      if (!is.null(na)) {
        if (na == FALSE) {
          na_subchunk <- cbind(na_subchunk, data.frame(method=rep(NA, nrow(na_subchunk)), lower=rep(NA, nrow(na_subchunk)), upper=rep(NA, nrow(na_subchunk))))
        } else if (nrow(na_subchunk) > 0) {
          na_subchunk <- cbind(na_subchunk, data.frame(method="bootstrap"), binom::binom.confint(na_subchunk$x, na_subchunk$n, conf.level=cred.mass, methods="bayes")[ , c(5, 6)])
        } else {
          na_subchunk <- data.frame()
        }
      }
    } else if (!is.null(ci.method) && ci.method == "mcmc") {
      # An MCMC bayes approach to the confidence interval
      
      # First step is to join all the sections that need to be modeled.  That way we don't have to compile multiple models
      subchunk_to_ci <- enum_subchunk[FALSE, ] # create an initial empty dataframe
      if (nrow(enum_subchunk) > 0) {
        subchunk_to_ci <- rbind(subchunk_to_ci, enum_subchunk)
      }
      if (!is.null(unk) && (!unk)) {
        subchunk_to_ci <- rbind(subchunk_to_ci, unk_subchunk)
      }
      if (!is.null(na) && (!na)) {
        subchunk_to_ci <- rbind(subchunk_to_ci, na_subchunk)
      }
      
      # compile the model  
      if (nrow(subchunk_to_ci) > 0 ) {
        # I use a simple binomial model, but other options may provide better estimates:
        # Also considered family=zero_inflated_binomial()
        # Also considered family=beta_binomial2 from https://cran.r-project.org/web/packages/brms/vignettes/brms_customfamilies.html
        #suppressWarnings(requireNamespace('brms'))
        suppressWarnings(require('rstan')) # because rstan has issues if not loaded.
        m <- suppressWarnings(brms::brm(x | trials(n) ~ (1|enum), 
                       data=subchunk_to_ci, 
                       family = binomial(), 
                       control = list(adapt_delta = .90, max_treedepth=10),
                       silent=TRUE, refresh=0, open_progress=FALSE)) # suppress most messages
        mcmc <- tidybayes::spread_draws(m, b_Intercept, r_enum[enum,])
        mcmc$condition_mean <- logit2prob(mcmc$b_Intercept + mcmc$r_enum)
        mcmc <- tidybayes::median_qi(mcmc, .width=cred.mass)
        # brms rewrites the column names. We're going to _try_ and fix that by mapping them back to the chunk enums.
        # this may or may not work since we can't be _sure_ they sort the same. 
        # As such, this is a hack until a mapping can be extracted from the model object or the same function used internally can be used to create a mapping.
        #mcmc$enum <- plyr::mapvalues(enum, sort(unique(enum)), sort(levels(chunk$enum)))
        # Per https://discourse.mc-stan.org/t/brms-non-standard-variable-name-modification/11412
        mcmc$enum <- plyr::mapvalues(mcmc$enum, gsub("[ \t\r\n]", ".", as.character(unique(subchunk_to_ci$enum))), as.character(unique(subchunk_to_ci$enum))) # it would be nice to not have to import plyr, but oh well. - GDB 191123
      }
      
      # separate the values back into their respective subchunks
      if (nrow(enum_subchunk) > 0) {
        enum_subchunk <- cbind(enum_subchunk, data.frame(method="mcmc", lower=mcmc$condition_mean.lower[match(enum_subchunk$enum, mcmc$enum)], upper=mcmc$condition_mean.upper[match(enum_subchunk$enum, mcmc$enum)]))
        mcmc <- mcmc[(nrow(enum_subchunk)+1):nrow(mcmc), ] #remove the subchunk rows.
      } else {
        enum_subchunk <- cbind(enum_subchunk, data.frame(method=character(), lower=numeric(), upper=numeric()))
      }
      if (unk == FALSE) {
        unk_subchunk <- cbind(unk_subchunk, data.frame(method=rep(NA, nrow(unk_subchunk)), lower=rep(NA, nrow(unk_subchunk)), upper=rep(NA, nrow(unk_subchunk))))
      } else if (nrow(unk_subchunk) >0) {
        unk_subchunk <- cbind(unk_subchunk, data.frame(method="mcmc", lower=mcmc$condition_mean.lower[[1]], upper=mcmc$condition_mean.upper[[1]]))
        mcmc <- mcmc[2:nrow(mcmc), ] #remove the subchunk rows.
      } else {
        unk_subchunk <- data.frame()
      }
      if (!is.null(na)) {
        if (na == FALSE) {
          na_subchunk <- cbind(na_subchunk, data.frame(method=rep(NA, nrow(na_subchunk)), lower=rep(NA, nrow(na_subchunk)), upper=rep(NA, nrow(na_subchunk))))
        } else if (nrow(na_subchunk) > 0) {
          na_subchunk <- cbind(na_subchunk, data.frame(method="mcmc", lower=mcmc$condition_mean.lower[[1]], upper=mcmc$condition_mean.upper[[1]]))
        } else {
          na_subchunk <- data.frame()
        }
      }
    }
    
    # If logical (rather than multinomial), remove all rows other than the one logical
    if (enum_type == "logical") {
      if (short.names) {
        logical_enum_end <- gsub('^(.*)[.]([^.]+$)', "\\2", logical_enum)
        enum_subchunk <- enum_subchunk[enum_subchunk$enum == logical_enum_end, ]
      } else {
        enum_subchunk <- enum_subchunk[enum_subchunk$enum == logical_enum, ]
      }
    }
    
    # recombine the portions of the subchunk
    subchunk <- rbind(enum_subchunk, na_subchunk, unk_subchunk)
    
    # add the 'by' column
    subchunk <- cbind(rep(x, nrow(subchunk)), subchunk)
    names(subchunk)[1] <- "by"
    
    if (!force & n < ci_n) {
      subchunk$x[!is.na(subchunk$n)] <- NA
      subchunk$freq[!is.na(subchunk$n)] <- NA
    }
    
    subchunk # return
  })
  
  ## Since some chunks may be below ci_min
  #if (any(unlist(lapply(chunks_list, function(l) {"lower" %in% names(l)})))) {
  chunks_list <- lapply(chunks_list, function(subchunk) {
    if (!"lower" %in% names(subchunk)) {
      subchunk$method <- "none"
      subchunk$lower <- NA_real_
      subchunk$upper <- NA_real_
    }
    subchunk
  })
  #}

  chunk <- do.call(rbind, chunks_list) # now that we've fixed any column issues, bind the columns again
  
  # if there was no 'by', delete the 'by' column
  if (by_type != "multinomial" && by_type != "single_column") {
    chunk <- chunk[ , -1]
  }
  
  # if short names, only use the bit of the enum name after the last period
  if (short.names) {
    if (is.character(chunk$enum)) {
      # chunk$enum <- as(stringr::str_match(chunk$enum, "[^.]+$"), by_class)
      # chunk$enum <- unlist(regmatches(chunk$enum, regexec("[^.]+$", chunk$enum)))
      chunk$enum <- gsub('^.*[.]([^.]+$)', "\\1", chunk$enum)
    }
    if("by" %in% names(chunk)) {
      if (is.character(chunk$by)) {
        # chunk$by <- as(stringr::str_match(chunk$by, "[^.]+$"), by_class)
        # chunk$by <- unlist(regmatches(chunk$by, regexec("[^.]+$", chunk$by)))
        chunk$by <- gsub('^.*[.]([^.]+$)', "\\1", chunk$by)
      }
    }
  }
  
  if (round.freq>0) {
    chunk$freq <- round(chunk$freq, round.freq)
    if ("lower" %in% names(chunk)) {
      chunk$lower <- round(chunk$lower, round.freq)
      chunk$upper <- round(chunk$upper, round.freq)
    }
  }
  
  # reorder output
  # if !force and n < ci_n, order enum alphabetically, otherwise order by freq.
  if (!force & any(chunk$n < ci_n, na.rm=TRUE)) {
    if ("by" %in% names(chunk)) {
      chunk <- chunk[order(chunk$by, as.character(chunk$enum)), ] 
      chunk$enum <- factor(chunk$enum, levels=rev(unique(chunk$enum)))
    } else {
      chunk <- chunk[order(as.character(chunk$enum)), ]
      chunk$enum <- factor(chunk$enum, levels=rev(unique(chunk$enum)))
    }
  } else {
    if ("by" %in% names(chunk)) {
      chunk <- chunk[order(chunk$by, -chunk$freq), ] 
      chunk$enum <- factor(chunk$enum, levels=rev(unique(chunk$enum)))
    } else {
      chunk <- chunk[order(-chunk$freq), ]
      chunk$enum <- factor(chunk$enum, levels=unique(chunk$enum))
    }
  }
  
  # replace row numbers
  rownames(chunk) <- seq(length=nrow(chunk))
  # rownames(chunk) <- NULL
  
  if (!force) {
    if (any(chunk$n < n_floor, na.rm=TRUE)) {
      warning(paste0("Removing all rows with n < ", n_floor, ".  ", n_floor, " is the smallest samples size appropriate for the DBIR. Use force = TRUE to avoid this removal.  If this leaves n othing but columns with no sample size, those will be removed as well."))
    }
    chunk <- chunk[is.na(chunk$n) | chunk$n >= n_floor, ]
    if (all(is.na(chunk$n))) {
      chunk <- chunk[!is.na(chunk$n), ]
    }
  }
  
  # return
  chunk
}