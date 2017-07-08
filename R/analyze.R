#' Convert a hierarchical verisr object to a flattened verisr object
#' 
#' (A flattened verisr object has no list columns)
#' 
#' @param veris The verisr object to flatten
#' @param level Used during recursion
#' @param row.name Column to indicate unique rows in parent dataframe
#' @return flattened verisr object
#' @export
flatten <- function(veris, level=NULL, row.name="extra.rowname") {
  df <- do.call(cbind.data.frame, 
    c(
      veris[ , lapply(veris, class) != 'list'], # columns that don't need parsing
      lapply(names(veris)[lapply(veris, class) == 'list'], function(n) { # each of the list columns
        # turn the column vector into a dataframe of the colums plus a 'row.name' column with row numbers to be used to join with the original DF
        df.l <- dplyr::bind_rows(veris[[n]], .id=row.name)
        df.l[[row.name]] <- as.integer(df.l[[row.name]])
        if (nrow(df.l) == 0) { # if there's nothing in the list, just return a column of NAs.
          setNames(data.frame(rep(NA, nrow(veris))), ifelse(is.null(level), n, paste(level, n, sep=".")))
        } else {
          # recurse
          df.l <- flatten(df.l, level=n, row.name=row.name)
          rowname_child <- paste(n, row.name, sep=".")
          ret <- as.data.frame(
            lapply(df.l, function(c) {
              if (class(c)=="logical") rep(FALSE, nrow(veris)) # The 'FALSE' will be counted even if a row is not imported while an NA will not.  Unfortunately 'FALSE' is not filled in at any point and unfilled rows should end up filtered anyway so leaving 'FALSE' rather than 'NA'. - gdb 170621
              else if (class(c)=="character") rep(NA_character_, nrow(veris))
              else if (class(c)=="factor") rep(NA_character_, nrow(veris))
              else if (class(c)=="integer") rep(NA_real_, nrow(veris))
              else if (class(c)=="double") rep(NA_real_, nrow(veris))
              else if (class(c)=="numeric") rep(NA_real_, nrow(veris))
              else warning(paste0("Column ", names(c), " of class ", class(c), " not included!") )
            }), stringsAsFactors = FALSE)
          names(ret) <- names(df.l)
          # WARNING: There could be multiple 'row.namee' in this so need to be specific about it for this level.
          ret[ , rowname_child] <- 1:nrow(veris)
          # join on rowname.
          # TODO: Probably just join.
          # ret <- dplyr::left_join(ret, df.l, by=rowname_child)
          ret[df.l[[rowname_child]] , ] <- df.l
          
          # remove lower level rowname
          ret <- ret[, !grepl(rowname_child, names(ret))]
          
          # return to be bound with other columns
          ret
        }
      })
    )
  )
  
  # Need to do the stupid trick with asset.assets and data.variety (amount -> variety.amount).
  if (('amount' %in% names(df)) & (any(grepl("variety.", names(df))))) {
    df.names <- c(names(df), paste0("amount.", gsub("^variety[.](.*)$", "\\1", names(df[, grep("^variety", names(df), value=T)]))))
    df <- cbind(df, as.data.frame(matrix(data=NA_real_, nrow = nrow(df), ncol=length(df.names)-ncol(df))))
    names(df) <- df.names
    varieties <- grepl("^variety", names(df))
    #varieties <- names(df[, grep("^variety", names(df), value=T)])[apply(df[, grep("^variety",names(df), value=T)], MARGIN=1, which)]
    #varieties <- paste0("amount.", gsub("^variety[.](.*)$", "\\1", variety))
    for (i in which(!is.na(df[["amount"]]))) {
      variety <- names(df[varieties])[which(as.logical(df[i, varieties]))[1]]
      variety <- paste0("amount.", gsub("^variety[.](.*)$", "\\1", variety))
      df[i, variety] <- df[i, "amount"]
    }
  }

  if (!is.null(level)) {  # not the top and more than 1 so flatten
    ret <- df %>%
      dplyr::group_by_(row.name) %>%
      # find rows in the parent level, that have multiple rows at this level
      dplyr::filter(n() > 1) %>%
      # compress down each column based on it's class
      dplyr::summarize_all(function(c) {
        if (class(c) == "logical") {
          # any true == true for logical
          any(c)
        } else if (class(c) %in% c("character", "factor")) {
          # characters are comma-separated
          paste(as.character(c), collapse=",")
        } else if (class(c) %in% c("integer", "numeric", "double")) { # all numbers
          # numbers are added
          ifelse(all(is.na(c)), NA_real_, sum(c, na.rm=TRUE)) # if all values are NA, we should return NA, not 
        } else {
          warning(paste("Column", names(c), "of class", class(c), "collapsed to character as the class is not recognized."))
          paste(as.character(c), collapse=",")
        }
      }) %>%
      # ungroup to be safe
      dplyr::ungroup(row.name)
    # join the columns with more than 1 row with those with 1 or less rows
    single_rows <- as.integer(names(table(df[[row.name]]))[table(df[[row.name]]) <= 1])
    df <- rbind(
      df[df[[row.name]] %in% single_rows, ],
      ret
    )
    
    # set the names
    names(df) <- paste(level, names(df), sep=".")
  }
  
  df
}