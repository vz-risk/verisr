#' Convert a hierarchical verisr object to a flattened verisr object
#' 
#' (A flattened verisr object has no list columns)
#' 
#' @param veris The verisr object to flatten
# #' @param columns Columns to keep
#' @param level Used during recursion
#' @param sequence logical.  If FALSE, sequence columns within sequence are
#'     flattened.  (e.g. sequence[[1]][1:2, "action.hacking.variety.MitM"]
#'     becomes sequence.action.hacking.variety.MitM) If TRUE, each row-column 
#'     combination within sequence receives a column.  (e.g. 
#'     sequence[[1]][1:2, "action.hacking.variety.MitM"] becomes 
#'     sequence.1.action.hacking.variety.MitM and 
#'     sequence.2.action.hacking.variety.MitM.)  FALSE is most the most common
#'     value as it results in a data frame similar to the classic verisr data
#'     frame.  TRUE is only useful if the goal is to compare different steps
#'     of the sequence.
#' @param row.name Column to indicate unique rows in parent dataframe
#' @return flattened verisr object
#' @export
flatten <- function(veris, 
                    # columns=NULL, 
                    level=NULL, 
                    sequence=FALSE, 
                    row.name="extra.rowname") {
  
  
  
  df.unjoined <- c(
    veris[ , lapply(veris, class) != 'list'], # columns that don't need parsing
    lapply(names(veris)[lapply(veris, class) == 'list'], function(n) { # each of the list columns
      # if (!is.null(columns) && n != "sequence" && n %in% columns) stop(paste0("The only list column which may be used to filter is 'sequence'. Remove ", n, " from column list."))
      
      # turn the column vector into a dataframe of the colums plus a 'row.name' column with row numbers to be used to join with the original DF
      if (n == "sequence" & sequence==TRUE) {  # this adds a 'sequence' column within the 'sequence' list column dataframes equating to the row number
        veris[[n]] <- lapply(veris[[n]], function(df) { 
          ### Next two lines would convert sequence to a column.  Unnecessary when converting each sequence df to a single row as below
          # if (nrow(df) > 0) df[["sequence"]] <- as.factor(1:nrow(df))
          # df
          ### The below lines convert the dataframe into a single row.
          do.call(cbind, lapply(seq_len(nrow(df)), function(i) {setNames(df[i, ], paste(i, names(df), sep="."))}))
        })
      }
      df.l <- dplyr::bind_rows(veris[[n]], .id=row.name)
      df.l[[row.name]] <- as.integer(df.l[[row.name]])
      # because the joine process for 'sequence' can introduce NAs in logical columns, we need to find them and change them to 'FALSE'
      df.l[lapply(df.l, class)=="logical"][is.na(df.l[lapply(df.l, class)=="logical"])] <- FALSE
      if (nrow(df.l) == 0) { # if there's nothing in the list, just return a column of NAs.
        setNames(data.frame(rep(NA, nrow(veris))), ifelse(is.null(level), n, paste(level, n, sep=".")))
      } else {
        # recurse
        df.l <- flatten(df.l, 
                        # columns=ifelse(is.null(n), columns, gsub(paste0("^",n, "[.](.*)"), "\\1", columns)), # strips leading column name 
                        level=n, 
                        row.name=row.name)
        if (nrow(df.l) != 0) { # if no rows exist, don't go through the trouble of binding them
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
          # WARNING: There could be multiple 'row.name' in this so need to be specific about it for this level.
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
      }
    })
  )
  df.unjoined[sapply(df.unjoined, is.null)] <- NULL
  df <- do.call(cbind.data.frame, df.unjoined)
  
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

  # # if we are downselecting to columns, do that here.  Lots of edge cases to handle.
  # if (!is.null(columns)) {
  #   # filter columns
  #   # df <- df[, intersect(names(df), c(columns, row.name))]
  #   # filter out empty rows
  #   ## This could potentially improve performance, but I'm commenting out as I"m concerned about it's effect on sample size. - gdb 170801
  #   # df <- df[apply(df, MARGIN=1, function(r) {all(is.na(r) | r == FALSE)}), ]
  #   columns.matching <- grep(paste0("^",paste(columns, collapse="|"),"[.][A-Z0-9][^.]*$"), names(df), value=TRUE)
  #   if (length(columns.matching) <= 0) {
  #     columns.matching <- grep(paste0("^",paste(columns, collapse="|"),"$"), names(df), value=TRUE)
  #   }
  #   # message(paste(c(level, columns.matching, row.name), collapse=", ")) # DEBUG
  #   if (!is.null(level)) columns.matching <- c(columns.matching, row.name)
  #   df <- df[, columns.matching]
  # }
  
  message(level)
  
  
  if (length(names(df)) <= 1) { # if none of the selected columns are in the dataframe, no reason to do all the rest of that work.
   if (is.null(level)) warning("No columns matched")
   return(data.frame())
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
  
  # fix sequence step number column name if it exists
  names(df) <- gsub("^sequence[.]sequence$", "sequence", names(df))
  
  # return
  # NOTE: additional return above in the case that no columns were selected
  df
}

#' Find the locations of vectors in a hierarchical veris object
#' 
#' @param veris The verisr object to flatten
#' @param columns to locate
#' @param level Used during recursion
#' @return flattened, subsetted verisr object
#' @export
recurse_columns <- function(veris, columns, level=NULL) {
  
  # 1. find columns at the different levels of the dataframe
  columns_to_keep <- c()
  # get character or numeric columns addressed by full name
  columns_to_keep <- c(columns_to_keep, intersect(columns, names(veris)))
  # remove columns from future analysis
  columns <- setdiff(columns, columns_to_keep)
  # get non-exclusively multinomial features spread across multiple logical columns
  cols.super <- intersect(columns, gsub('(^.*)[.][^.]+$', "\\1", names(veris))) # TODO: This breaks wild cards in the column name
  if (length(cols.super) >= 1) {
    columns_to_keep <- c(columns_to_keep, grep(paste0("(", paste(cols.super, collapse="|"), ')[.][^.]+$'), names(veris), value=T))
  }
  ## message(paste(columns_to_keep, collapse=", ")) # debug
  # remove those columns from future use
  columns <- setdiff(columns, cols.super)
  # split the remaining columns into all combinations to look to see if they begin with a list column
  cols.bifercated <- do.call(rbind, lapply(stringr::str_split(columns, pattern=stringr::fixed(".")), function(l) {
    do.call(rbind, lapply(2:(length(l)-1), function(i) {
      data.frame(super=paste(l[1:i-1], collapse="."), sub=paste(l[i:length(l)], collapse="."), stringsAsFactors = FALSE)
    }))
  }))
  # recurse on list columns
  cols.from.lists <- unlist(lapply(unique(as.character(cols.bifercated[, "super"])), function(s) {
    if (s %in% names(veris) & class(veris[[s]]) == "list") {
      recurse_columns(veris[[s]][[1]], columns = as.character(cols.bifercated[as.character(cols.bifercated[, "super"])==s, "sub"]), level=s)
    }
  }))
  columns_to_keep <- c(columns_to_keep, cols.from.lists)

  # if we are in a child, prepend the parent to child separated with a "|"
  if (!is.null(level)) {
    columns_to_keep <- paste(level, columns_to_keep, sep="|")
  }
  
  # return
  columns_to_keep
}
