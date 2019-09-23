#' The main method for summarizes veris enumerations from verisr objects
#' 
#' @inheritDotParams getenumCI2019
#' @export
getenumCI <- function(...) {
  getenumCI2019(...)
}

#' Summarizes veris enumerations from verisr objects
#' 
#' WARNING: This function is incomplete and untested.  DO NOT USE.
#' 
#' This calculates the mean of the source percentages of the enumeration.
#'     This effectively treats each source as a sample and then finds the
#'     center (mean) of the samples.  While this is more statistically
#'     correct, it does not deal with the underlying bias of the samples.
#'     
#' Additionally, unlike getenumCI(), this does not provide a count of
#'     'unknown's and 'na's if they are filtered out as it only shows
#'     percentages which are meaningless for values not included in the 
#'     sample.
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
#' @param source.col Tthe name of the column containing the source of the
#'     record.  Defaults to 'source_id'
#' @param sample.size The minimum sample size per partner to accept. Also
#'     warning given when number of partners is less than this value. Defaults
#'     to 31, (For n-1 or 30 degrees of freedom per student's T test.)
#' @param ci.method A confidence interval method to use.  Current supported
#'     methods are any from boot::boot.ci.  If unsure which to use, 
#'     use "bca". Failing that, use "perc".
#' @param ci.level A number from 0 to 1 representing the width of the 
#'     confidence interval. (default = 0.95)
#' @param boot.r The number of bootstrap replicates to use. Defaults to 999
#' @param round.freq An integer indicating how many places to round
#'     the frequency value to. (default = 5)
#' @param na DEPRECIATED! Use 'na.rm' parameter.
#' @param ... A catch all for functions using arguments from previous
#'     versions of getenum.
#' @return A data frame summarizing the enumeration
#' @export
getenumSRC <- function(veris, 
                       enum, 
                       by=NULL,
                       na.rm = NULL, 
                       unk=FALSE, 
                       short.names=TRUE, 
                       source.col="source_id",
                       sample.size=31, # 30 degrees of freedom
                       ci.method=NULL,
                       ci.level=0.95, 
                       boot.r=999,
                       round.freq=5, 
                       na = NULL, 
                       ...) {
  
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
  
  # because we aren't keeping the 'method' and don't want to duplicate rows for each method, only 1 allowed.
  if (length(ci.method) > 1) {
    warning("More than one confidence interval method specified. Using first.")
    ci.method <- ci.method[1]
  }
  ci.type <- c("normal", "basic", "student", "percent", "bca")
  names(ci.type) <- c("norm", "basic", "stud", "perc", "bca")
  
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
  chunk <- do.call(rbind, lapply(by_enums, function(x) {
    
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
          enum_enums <- enum_enums <- grep(paste0("^", gsub('^(.*)[.]([^.]+$)', "\\1", logical_enum), "[.][A-Z0-9][^.]*$"), names(subdf), value=TRUE)
        } else {
          enum_type <- "single_column"
        }
      } else {
        stop(paste0("Enum ", enum, " did not resolve to any columns."))
      }
    }
    
    # This allows us to handle numerical/factor/character and logical enumerations
    if (enum_type == "multinomial" | enum_type == "logical") {
      subdf <- subdf[, c(enum_enums, source.col)] # added source.col as needed later - 031417 gdb
      if (ncol(subdf) <= 0) { stop(paste(c("No columns matched feature(s) ", enum, " using regex ", paste0("^",enum,"[.][A-Z0-9][^.]*$"), collapse=" ")))}
      
      # get a unique list of sources in this subdf
      sources <- unique(df[[source.col]])
      
      # we remove unknowns because they should normally not be counted
      if (unk == FALSE) {
        if (short.names) {
          subdf <- subdf[, !grepl("^(.+[.]|)(U|[A-Za-z]{1,3} - [U|u])nknown$", names(subdf))] # if short names, bla - unknown is removed. See logical section for why. - GDB 17-01-30
        } else {
          subdf <- subdf[, !grepl("^(.+[.]|)(U|u)nknown$", names(subdf))] # if long names, bla - unknown is kept in sample. See logical section for why. - GDB 17-01-30
        }
      } else {
        subdf <- subdf
      }
      
      # Whether to use NAs or not depends on the hypothesis being tested so we require an answer (no default)
      if (is.null(na) & any(grep("[.]NA$", names(subdf)))) { stop("'na' must be specified if any column names end in .NA")}
      if (!is.null(na)) {
        if (na == FALSE) {
          subdf <- subdf[, !grepl(".NA$", names(subdf)), ]
        }
      }
      
      #  if short.names, combine columns with short names. (Rather than summing same short name after calculating column sums, which double-counts in 'x'.)
      if (short.names) {
        short_names <- gsub('^.*[.]([^.]+$)', "\\1", names(subdf))
        subdf <- do.call(cbind, lapply(unique(short_names), function(y) { # bind the list of columns returned by lapply
          dups <- grep(paste0("^(",y,")$"), short_names)
          if (length(dups) > 1) {
            feature <- apply(subdf[ , grep(paste0("^(",y,")$"), short_names)], MARGIN=1, any) # 'grep' selects columns with the name, apply checks if any of the row are true
          } else {
            feature <- subdf[ , grep(paste0("^(",y,")$"), short_names)]
          }
          feature <- data.frame(feature)
          names(feature) <- y
          feature
        }))
      }
      
      # number of records that have one of our enumerations
      # n <- sum(rowSums(subdf_for_n, na.rm=TRUE) > 0, na.rm=TRUE) combined value
      ns <- unlist(lapply(sources, function(source) { # n's
        nrow(subdf[subdf[[source.col]] == source, grep(source.col, names(subdf), invert = TRUE)])
      }))
      names(ns) <- sources
      ns <- ns[ns >= sample.size] # remove samples lower than sample size.
      if (length(ns) < sample.size) {
        warning(paste0("Only ", length(ns), " samples exist.  Sample mean may not represent the true mean."))
      }
      # count of each enumeration
      
      # generate matrix of source vs enumeration
      m <- do.call(
        rbind,
        lapply(names(ns), function(source) {
          colSums(subdf[subdf[[source.col]] == source, grep(source.col, names(subdf), invert = TRUE)], na.rm=TRUE)/ns[source]
        })
      )
      
      # the number of sources (number of rows in m) is the n value
      n <- nrow(m)
      
      # get the average souce percentage
      if (!is.null(m)) {
        v <- apply(m, MARGIN=2, mean, na.rm=T)
      } else {
        v <- NULL
      }
      
    } else if (enum_type == "single_column") {
      # get a logical vector representing unknowns or 'FALSE' if unknowns are to be included
      if (unk == FALSE) {
        unknown <- grepl("^(.+[.]|)(U|u)nknown$", subdf[[enum_enums]])
      } else {
        unknown <- rep(FALSE, length(subdf[[enum_enums]]))
      }
      
      # get a logical vector representing NAs or 'FALSE' if NAs are to be included
      if (is.null(na) & any(grep("[.]NA$", subdf[[enum_enums]]))) { stop("'na' must be specified if any column names end in .NA")}
      if (!is.null(na)) {
        if (na == FALSE) {
          nas <- grepl("^(.+[.]|)(U|u)nknown$", subdf[[enum_enums]])
        } else {
          nas <- rep(FALSE, length(subdf[[enum_enums]]))
        } 
      } else {
        nas <- rep(FALSE, length(subdf[[enum_enums]]))
      }
      
      # get a unique list of sources in this subdf
      sources <- unique(df[[source.col]])
      
      # generate matrix of source vs enumeration
      m.tot <- table(vz[, c(source.col, enum_enums)]) # totals table
      ns <- table(vz[[source.col]])
      m <- do.call( rbind,
                    lapply(unique(names(ns)), function(source) {
                      m.tot[source, ]/ns[[source]]
                    }))
      #       row.names(m) <- unique(names(ns))
      # m <- do.call(
      #     function(l) {
      #       Reduce(function(x, y) merge(x, y, by="enum", all=TRUE), l)
      #     },
      #     lapply(sources, function(source) {
      #       sample_subset <- subdf[[source.col]] == source # get a logical vector representing the source
      #       n <- nrow(subdf[sample_subset & !unknown & !nas, ])
      #       if (n >= sample.size) {
      #         t <- table(subdf[sample_subset & !unknown & !nas, ][[enum_enums]])/n
      #         data.frame(enum=names(t), x=t)
      #       }
      #     }))
      # m <- as.matrix(m[ , -enum])
      # is.na(m) <- 0
      # m <- t(m)
      
      # get n from matrix (really a df) of source vs enumeration
      n <- nrow(m)
      
      # get the average souce percentage
      if (!is.null(m)) {
        v <- apply(m, MARGIN=2, mean, na.rm=T)
      } else {
        v <- NULL
      }
      
    } else {
      stop("class of 'enum' column(s) was not identified, preventing summarization.")
    }
    
    if (!is.null(v)) {
      
      # create the chunk for this 'by'
      subchunk <- data.frame(enum=names(v), n=rep(n, length(v)), freq=v)
      
      # apply the confidence interval.  Apply to NA's and unk separately depending on if selected. (If you try and apply CI's cart blanc to the NA/Unknowns it can error out on binding the columns)
      if (length(ci.method) > 0) {
        # NOTE: I left a lot of settings in boot::boot to defaults and didn't expose them. Feel free to do so. - gdb 031417
        v.boot <- apply(m, MARGIN=2, boot::boot, function(u, i) {mean(u[i])}, R=boot.r)
        # NOTE: I left a lot of settings in boot::boot.ci to defaults and didn't expose them. Feel free to do so. - gdb 031417
        v.boot.ci <- do.call(rbind, lapply(v.boot, function(b) {
          bci <- boot::boot.ci(b, type=ci.method, conf=ci.level)
          bci[[ci.type[ci.method]]][1, (ncol(bci[[ci.type[ci.method]]])-1):ncol(bci[[ci.type[ci.method]]])] # last two columns
        }))
        subchunk <- cbind(subchunk, v.boot.ci)
        names(subchunk) <- c("enum", "n", "freq", "lower", "upper")
      }
      
      # If logical (rather than multinomial), remove all rows other than the one logical
      if (enum_type == "logical") {
        if (short.names) {
          logical_enum_end <- gsub('^(.*)[.]([^.]+$)', "\\2", logical_enum)
          subchunk <- subchunk[subchunk$enum == logical_enum_end, ]
        } else {
          subchunk <- subchunk[subchunk$enum == logical_enum, ]
        }
      }
      
      # add the 'by' column
      subchunk <- cbind(rep(x, nrow(subchunk)), subchunk)
      names(subchunk)[1] <- "by"
      
      subchunk # return
    }
  }))
  
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
  if ("by" %in% names(chunk)) {
    chunk <- chunk[order(chunk$by, -chunk$freq), ] 
    chunk$enum <- factor(chunk$enum, levels=rev(unique(chunk$enum)))
  } else {
    chunk <- chunk[order(-chunk$freq), ]
    chunk$enum <- factor(chunk$enum, levels=rev(unique(chunk$enum)))
  }
  
  # replace row numbers
  rownames(chunk) <- seq(length=nrow(chunk))
  # rownames(chunk) <- NULL
  
  # return
  chunk
}