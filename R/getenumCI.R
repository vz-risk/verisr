#' Wrapper for getenumCI()
#' 
#' @param ... getenumCI() parameters.  At a minimum, 'veris' (verisr object)
#'   and enum (string)
#' @export
getenum <- function(...) {
  getenumCI(...)
}

#' Summarizes veris enumerations from verisr objects
#' 
#' From dbirR 9047
#' 
#' @param veris A verisr object
#' @param enum A veris feature or enumeration to summarize
#' @param by A veris feature or enumeration to group by
#' @param na A boolean of whether to include not applicable in the sample set.
#'     This is REQUIRED if enum has a potential value of NA as there is no 
#'     'default' method for handling NAs.  Instead, it depends on the 
#'     hypothesis being tested.
#' @param unk A boolean referring whether to include 'unknown' in the sample.
#'     The default is 'FALSE' and should rarely be overwritten.
#' @param short.names A boolean identifying whether to use the full enumeration
#'     name or just the last section. (i.e. action.hacking.variety.SQLi vs
#'     just SQLi.)
#' @param ci.method A confidence interval method to use.  Current supported
#'     methods are any from binom.confint() or "multinomial".  If unsure
#'     which to use, use "wilson".
#' @param ci.level A number from 0 to 1 representing the width of the 
#'     confidence interval. (default = 0.95)
#' @param round.freq An integer indicating how many places to round
#'     the frequency value to. (default = 5)
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
#' getenumCI(vcdb, "asset.cloud")
#' getenumCI(vcdb, "action.social.variety.Phishing")
#' getenumCI(vcdb, "actor.*.motive", ci.method="wilson", na=FALSE)
getenumCI <- function(veris, 
                      enum, 
                      by=NULL,
                      na = NULL, 
                      unk=FALSE, 
                      short.names=TRUE, 
                      ci.method=c(), 
                      ci.level=0.95, 
                      round.freq=5, 
                      ...) {
  
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
        } else {
          enum_type <- "single_column"
        }
      } else {
        stop(paste0("Enum ", enum, " did not resolve to any columns."))
      }
    }
    
    # This allows us to handle numerical/factor/character and logical enumerations
    if (enum_type == "multinomial") {
      subdf <- subdf[, enum_enums]
      
      if (ncol(subdf) <= 0) { stop(paste(c("No columns matched feature(s) ", enum, " using regex ", paste0("^",enum,"[.][A-Z0-9][^.]*$"), collapse=" ")))}
      
      # we remove unknowns because they should normally not be counted
      if (unk == FALSE) {
        subdf_for_n <- subdf[, !grepl(".[U|u]nknown$", names(subdf))]
      } else {
        subdf_for_n <- subdf
      }
      
      # Whether to use NAs or not depends on the hypothesis being tested so we require an answer (no default)
      if (is.null(na) & any(grep("[.]NA$", names(subdf)))) { stop("'na' must be specified if any column names end in .NA")}
      if (!is.null(na)) {
        if (na == FALSE) {
          subdf_for_n <- subdf_for_n[, !grepl(".NA$", names(subdf_for_n)), ]
        }
      }
      
      # number of records that have one of our enumerations
      n <- sum(rowSums(subdf_for_n, na.rm=TRUE) > 0, na.rm=TRUE)
      # count of each enumeration
      
      #  if short.names, combine columns with short names. (Rather than summing same short name after calculating column sums, which double-counts in 'x'.)
      if (short.names) {
        short_names <- gsub('^.*[.]([^.]+$)', "\\1", names(subdf))
        subdf <- do.call(cbind, lapply(unique(short_names), function(y) { # bind the list of columns returned by lapply
          dups <- grep(y, short_names)
          if (length(dups) > 1) {
            feature <- apply(subdf[ , grep(y, short_names)], MARGIN=1, any) # 'grep' selects columns with the name, apply checks if any of the row are true
          } else {
            feature <- subdf[ , grep(y, short_names)]
          }
          feature <- data.frame(feature)
          names(feature) <- y
          feature
        }))
      }
      
      v <- colSums(subdf, na.rm=TRUE)  # used instead of a loop or plyr::count to compute x
      
    } else if (enum_type == "single_column") {
      table_v <- table(subdf[[enum_enums]])
      v <- as.integer(table_v)
      names(v) <- names(table_v)
      
      n <- sum(v, na.rm=TRUE)
      # remove unknowns
      if (unk == FALSE) {
        n <- n - sum(v[grepl("^(.+[.]|)Unknown$", names(v))], na.rm=TRUE)
      }

      # remove NAs
      if (!is.null(na)) {
        if (unk == FALSE) {
          n <- n - sum(v[grepl("^(.+[.]|)NA$", names(v))], na.rm=TRUE)
        }
      }
      
    } else if (enum_type == "logical") {
      v <- c(sum(subdf[[enum_enums]], na.rm=TRUE))
      names(v) <- enum_enums

      # enum_feature <- stringr::str_match(enum_enums, "^(.+)\\.")[1]
      # enum_feature <- unlist(regmatches(enum_enums, regexec("^(.+)\\.", enum_enums)))[1]
      enum_feature <- gsub('^.*[.]([^.]+$)', "\\1", enum_enums)[1]
      # need to remove where the enum is not true but the 'unknown' enumeration of the feature is.
      if (unk == FALSE & paste0(enum_feature, "Unknown") %in% names(subdf)) {
        subdf <- subdf[!(!subdf[[enum_enums]] & subdf[[paste0(enum_feature, "Unknown")]]), ]
      }
      # need to remove where the enum is not true but the 'na' enumeration of the feature is.
      if (!is.null(na)) { 
        if(na == FALSE & paste0(enum_feature, "NA") %in% names(subdf)) {
          subdf <- subdf[!(!subdf[[enum_enums]] & subdf[[paste0(enum_feature, "NA")]]), ]
        }
      }
      n <- nrow(subdf)
      
    } else {
      stop("class of 'enum' column(s) was not identified, preventing summarization.")
    }
    
    # create the chunk for this 'by'
    subchunk <- data.frame(enum=names(v), x=v, n=rep(n, length(v)), freq=v/n)
    enum_subchunk <- subchunk[!grepl("^(.+[.]|)Unknown$", subchunk$enum) & !grepl("^(.+[.]|)NA$", subchunk$enum), ]
    unk_subchunk <- subchunk[grepl("^(.+[.]|)Unknown$", subchunk$enum), ]
    na_subchunk <- subchunk[grepl("^(.+[.]|)NA$", subchunk$enum), ]
    
    # n is not applicable for Unknown (and potentially na) rows so zero it out
    if (unk == FALSE & nrow(unk_subchunk) > 0) {
      unk_subchunk[ , c("n", "freq")] <- NA
    }
    if (!is.null(na)) {
      if (na == FALSE & nrow(na_subchunk) > 0) {
        na_subchunk[ , c("n", "freq")] <- NA
      }
    }
      
    # apply the confidence interval.  Apply to NA's and unk separately depending on if selected. (If you try and apply CI's cart blanc to the NA/Unknowns it can error out on binding the columns)
    if (length(ci.method) > 0) {
      if (nrow(enum_subchunk) > 0) {
        # subchunk <- dplyr::bind_cols(subchunk, binom::binom.confint(subchunk$x, subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
        enum_subchunk <- cbind(enum_subchunk, binom::binom.confint(enum_subchunk$x, enum_subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
      } else {
        enum_subchunk <- cbind(enum_subchunk, data.frame(method=character(), lower=numeric(), upper=numeric()))
      }
      if (unk == FALSE) {
        # unk_subchunk <- dplyr::bind_cols(unk_subchunk, data.frame(method=rep(NA, nrow(unk_subchunk)), lower=rep(NA, nrow(unk_subchunk)), upper=rep(NA, nrow(unk_subchunk))))
        unk_subchunk <- cbind(unk_subchunk, data.frame(method=rep(NA, nrow(unk_subchunk)), lower=rep(NA, nrow(unk_subchunk)), upper=rep(NA, nrow(unk_subchunk))))
      } else if (nrow(unk_subchunk) >0) {
        # unk_subchunk <- dplyr::bind_cols(unk_subchunk, binom::binom.confint(unk_subchunk$x, unk_subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
        unk_subchunk <- cbind(unk_subchunk, binom::binom.confint(unk_subchunk$x, unk_subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
      } else {
        unk_subchunk <- data.frame()
      }
      if (!is.null(na)) {
        if (na == FALSE) {
          # na_subchunk <- dplyr::bind_cols(na_subchunk, data.frame(method=rep(NA, nrow(na_subchunk)), lower=rep(NA, nrow(na_subchunk)), upper=rep(NA, nrow(na_subchunk))))
          na_subchunk <- cbind(na_subchunk, data.frame(method=rep(NA, nrow(na_subchunk)), lower=rep(NA, nrow(na_subchunk)), upper=rep(NA, nrow(na_subchunk))))
        } else if (nrow(na_subchunk) > 0) {
          # na_subchunk <- dplyr::bind_cols(na_subchunk, binom::binom.confint(na_subchunk$x, na_subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
          na_subchunk <- cbind(na_subchunk, binom::binom.confint(na_subchunk$x, na_subchunk$n, conf.level=ci.level, methods=ci.method)[ , c(1, 5, 6)])
        } else {
          na_subchunk <- data.frame()
        }
      }
    }
    
    # recombine the portions of the subchunk
    subchunk <- rbind(enum_subchunk, na_subchunk, unk_subchunk)
    
    # add the 'by' column
    subchunk <- cbind(rep(x, nrow(subchunk)), subchunk)
    names(subchunk)[1] <- "by"

    subchunk # return
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