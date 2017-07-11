#' Read in all the VERIS incidents (JSON files) in a given directory.
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as an encoded VERIS record.  This function
#' requires that a JSON schema be available for the VERIS data.  If the variable is 
#' not specified, it will attempt to grab the "verisc-merged.json" schema from
#' https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json.
#' 
#' This will return a verisr object, which is a data.table object and can be 
#' directly accesses as such.
#' 
#' Couple of unique things...  The returned object will have additional fields 
#' for convenience: 
#'   * *actor* will return top level actor categories
#'   * *action* will return top level action categories
#'   * *asset.variety* will return top level asset categories
#'   * *attribute* will return top level asset categories
#'   * *victim.industry2* will return the first 2 digits of the NAICS code
#'   * *victim.industry3* same, first 3 digits
#'   * *victim.orgsize* returns "Large" and "Small" enumerations
#' 
#' The victim.secondary.victim_id, external.actor.region, and any other free
#' text field that can be repeated is being collapsed into a single string 
#' seperated by a comma at the moment.  If that poses a challnge, open an issue
#' on it.
#'
#' @param dir the directory to list through.  This may be a vector of 
#' directorites, in which case each all the matching files in each 
#' directory will be laoded.
#' @param schema a full veris schema with enumerations included.
#' @param progressbar a logical value to show (or not show) a progress bar
#' @keywords json
#' @export
#' @examples
#' \dontrun{
#' # load up all the veris files in the "vcdb" directory
#' # grab the schema off of github.
#' veris <- json2veris(dir="~/vcdb")
#' 
#' # load up files from multiple directories
#' veris <- json2veris(dir=c("~/vcdb", "private_data"))
#' 
#' # specify a local schema with localized plus section.
#' veris <- json2veris(dir="~/vcdb", 
#'                     schema="~/veris/verisc-local.json")
#' }
json2veris <- function(dir=".", schema=NULL, progressbar=F) {
  debug <- FALSE
  if (debug) {
    warning("DEBUG ENABLED.")
  }
  savetime <- proc.time()
  # if no schema, try to load it from github
  if (missing(schema)) {
    x <- RCurl::getURL("https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json")
    lschema <- rjson::fromJSON(json_str=x)
  } else {
    lschema <- rjson::fromJSON(file=schema)
  }  
  # create listing of files
  jfiles <- unlist(sapply(dir, list.files, pattern = "json$", full.names=T))
  numfil <- length(jfiles)
  # need to pull these before we loop, used over and over in loop
  a4 <- geta4names() # just returns a (named) character vector of the 4A's and their next level values.  i.e. actor.External
  vtype <- parseProps(lschema) # recursively parse the schema. returns a named character vector. names=column, value=class. (no enumerations)
  # get a named vector of field and types
  ### OLD
  # vft <- getverisdf(lschema, a4) # we now have all the columns we need
  ### NEW
  vft <- veriscol(lschema, a4) # getverisdf removed and all functionality moved to veriscol. - gdb 170619
  ###
  # now create a data table with the specific blank types
  # we just pulled from getverisdf()
  
  #### OLD
  # veris <- as.data.table(lapply(seq_along(vft), function(i) {
  # #veris <- as.data.frame(lapply(seq_along(vft), function(i) {
  #   if (vft[i]=="character") rep(NA_character_, numfil)
  #   else if (vft[i]=="logical") rep(FALSE, numfil)
  #   else if (vft[i]=="integer") rep(NA_real_, numfil)
  #   else if (vft[i]=="double") rep(NA_real_, numfil)
  # }))
  # setnames(veris, names(vft))
  ### NEW
  verisBuilt <- buildVeris(vft, numfil) # build recursive list to populate with data.
  # verisFilled <- joinVeris(verisBuilt) # recursive join
  # one-level join:
  verisFilled <- as.data.frame(verisBuilt[[1]])
  if (length(verisBuilt) > 1) { # if you try and iterate over a zero-length, vector, it contain's 'NA' rather than not iterating
    for(n in names(verisBuilt)[2:length(verisBuilt)]) {
      if (is.null(nrow(verisBuilt[[n]])) || nrow(verisBuilt[[n]]) <= 1) {
        df <- do.call(data.frame, list(verisBuilt[[n]], "check.names"=FALSE, "stringsAsFactors"=FALSE)) # check.names required. Otherwise spaces are replaced with periods.
        verisFilled[[n]] <- replicate(nrow(verisFilled), df, simplify=FALSE)
      } else {
        verisFilled[[n]] <- replicate(nrow(verisFilled), verisBuilt[[n]], simplify=FALSE)
      }
    }
  }
  veris <- verisFilled
  rm(verisFilled)
  
  ###
  
  # get a text progress bar going
  pb <- NULL
  if (progressbar) pb <- txtProgressBar(min = 0, max = length(jfiles), style = 3)
  # in each file, pull out the values and fill in the data table
  if (debug) {
    jfiles <- jfiles[7000:length(jfiles)] # DEBUG
  }
  failures <- rep(TRUE, length(numfil))
  for(i in seq_along(jfiles)) {
    json <- rjson::fromJSON(file=jfiles[i], method='C')
    nfield <- nameveris(json, a4, vtype)
    
    if (length(nfield)==0) warning(paste("empty json file parsed from", jfiles[i]))
    
    ### OLD
    # nomatch <- !(names(nfield) %in% colnames(veris))
    # if (any(nomatch)) {
    #   warning(paste0("Column[s]: \n", paste0("  \"", names(nfield)[nomatch], "\"", collpase=", "), 
    #                  "\nNot found in schema, source file:", jfiles[i]))
    # }
    # for(x in names(nfield)) {
    #   if(length(nfield[[x]]) > 1) {
    #     tt <- tryCatch(set(veris, i=as.integer(i), j=x, value=paste(nfield[[x]], collapse=",")),
    #                    error=function(e) e, warning=function(w) w)
    #   } else {
    #     tt <- tryCatch(set(veris, i=as.integer(i), j=x, value=nfield[[x]]),
    #                    error=function(e) e, warning=function(w) w)
    #   }
    #   if(is(tt,"warning")) {
    #     cat(paste0("Warning found trying to set ", i, ", \"", x, "\" for \"", nfield[[x]], "\"\n"))
    #     cat("  length of assignment:", length(nfield[[x]]), "\n")
    #     cat("  in", i, jfiles[i], "\n")
    #     print(tt)
    #     cat("\n")
    #   }
    #   
    # }
    ### NEW
    if (debug) {
      veris[i, ] <- fillVeris(nfield, verisBuilt, jfiles[i]) # DEBUG
    } else {
      tt <- tryCatch(veris[i, ] <- fillVeris(nfield, verisBuilt, jfiles[i]), # MAIN PATH
                     error=function(e) e, warning=function(w) w)
      if(is(tt,"error")) {
        warning(paste0("Error importing file ", jfiles[i], ".  It will be skipped."))
        failures[i] <- FALSE
      } else if(is(tt,"warning")) {
        # cat(paste0("Warning found trying to set ", i, ", \"", x, "\" for \"", nfield[[x]], "\"\n"))
        # cat("  length of assignment:", length(nfield[[x]]), "\n")
        # cat("  in", i, jfiles[i], "\n")
        print(tt)
        cat("\n")
      }
    }
    ###
    if (!is.null(pb)) setTxtProgressBar(pb, i)
  }
  if (!is.null(pb)) close(pb)
  veris <- veris[failures, ] # if there were any errors, remove those records.
  
  # Not Applicable is replaced with NA for consistency
  # WARNING: the below line causes duplicates and overwrites legitimate 'Not Applicable' enumerations (such as plus.attack_difficulty_initial) so removing
  #  Instead, need to standardize NAs in veris.
  # colnames(veris) <- gsub('Not Applicable', 'NA', colnames(veris), ignore.case=TRUE) # this causes more problems than it solves. 17-01-17 GDB

  if (progressbar) {
    print(proc.time() - savetime)
    message("Beginning post processing.  This may take a while as well.")
  }
  veris <- post.proc(veris) # TODO: ADD BACK IN AND UPDATE APPROPRIATELY

  # veris <- as.data.frame(veris) # convert data.table to data.frame because data tables are evil. - 17-01-17. It's now outputting a dataframe. 17-06-23
  class(veris) <- c("verisr", class(veris))
  if(progressbar) {
    print(proc.time() - savetime)
  }
  veris
}

#' Post process the veris object to add convenience fields.
#' 
#' Given a veris object this will populate several convenience fields
#' like the victim.industry2 and industry3, 
#' 
#' Change in 1.1.3: now adds dummar vars for each pattern as "pattern.*"
#' 
#' @param veris the verisr object
#' @export
post.proc <- function(veris) {

  ### OLD
  # orgsize
  # small <- c("victim.employee_count.1 to 10", "victim.employee_count.11 to 100", 
  #            "victim.employee_count.101 to 1000", "victim.employee_count.Small")
  # large <- c("victim.employee_count.1001 to 10000", "victim.employee_count.10001 to 25000", 
  #            "victim.employee_count.25001 to 50000", "victim.employee_count.50001 to 100000", 
  #            "victim.employee_count.Over 100000", "victim.employee_count.Large")
  # veris[ , victim.orgsize.Small := rowSums(veris[ ,small, with=F]) > 0]
  # veris[ , victim.orgsize.Large := rowSums(veris[ ,large, with=F]) > 0]

  # # victim.industry
  # ind2 <- substring(unlist(veris[ ,"victim.industry", with=F], use.names=F), 1L, 2L)
  # # want an enumeration now, instead of a single list.
  # # But Kevin wants both so he is uncommenting the line that Jay commented
  # veris[ , victim.industry2 := ind2]
  # for(x in unique(ind2)) {
  #   iname <- paste0('victim.industry2.', x)
  #   veris[ ,iname:=(ind2==x), with=F]
  # }
  ## industry3 may require more prep work since dashes are allowed.
  # veris[ , victim.industry3 := substring(unlist(veris[ ,"victim.industry", with=F], 
  #                                               use.names=F), 1L, 3L)]

  # # victim.industry.name
  # data(industry2, envir = environment())
  # veris$victim.industry.name <- sapply(veris$victim.industry2, function(x) {
  #   ifelse(x %in% industry2$code, industry2$shorter[which(industry2$code==x)], "Unknown")
  # })
  
  # # actor.partner.industry
  # veris[ , actor.partner.industry2 := substring(unlist(veris[ ,"actor.partner.industry", with=F], 
  #                                               use.names=F), 1L, 2L)]
  # veris[ , victim.industry3 := substring(unlist(veris[ ,"victim.industry", with=F], 
  #                                               use.names=F), 1L, 3L)]
  #
  # veris <- cbind(veris, getpattern(veris))
  #
  ### NEW
  data(industry2, envir = environment())
  
  # unnest sequence dataframes to make modification easier
  df <- veris %>%
    dplyr::select(sequence) %>%
    dplyr::mutate(rowname = 1:n()) %>%
    tidyr::unnest()
  
  # orgsize
  small <- c("victim.employee_count.1 to 10", "victim.employee_count.11 to 100", 
             "victim.employee_count.101 to 1000", "victim.employee_count.Small")
  large <- c("victim.employee_count.1001 to 10000", "victim.employee_count.10001 to 25000", 
             "victim.employee_count.25001 to 50000", "victim.employee_count.50001 to 100000", 
             "victim.employee_count.Over 100000", "victim.employee_count.Large")
  df[["victim.orgsize.Small"]] <- rowSums(df[ , small]) > 0
  df[["victim.orgsize.Large"]] <- rowSums(df[ , large]) > 0
  
  # victim.industry
  sequence.names <- c(names(df), paste("victim.industry2", c(industry2$code, "00"), sep="."))
  df <- cbind(df, data.frame(matrix(data=FALSE, nrow=nrow(df), ncol=length(industry2$code)+1)))
  names(df) <- sequence.names
  for (ind2 in c(setdiff(industry2$code, c("31_33", "44_45", "48_49")), "00")) {
    df[grepl(paste0("^", ind2), df$victim.industry), paste0("victim.industry2.", ind2)] <- TRUE
  }
  df[grepl("^3[1-3]", df$victim.industry), "victim.industry2.31_33"] <- TRUE
  df[grepl("^4[4-5]", df$victim.industry), "victim.industry2.44_45"] <- TRUE
  df[grepl("^4[8-9]", df$victim.industry), "victim.industry2.48_49"] <- TRUE
  
  ## industry3 may require more prep work since dashes are allowed.
  df[["victim.industry3"]] <- substring(df[["victim.industry"]], 1, 3)
  
  # victim.industry.name
  ind2.tmp <- industry2[, c("code", "shorter")]
  names(ind2.tmp) <- c("extra.code", "victim.industry.name")
  df[["extra.code"]] <- substr(df[["victim.industry"]], 1, 2)
  df <- merge(df, ind2.tmp, by="extra.code", all.x=TRUE)
  df <- df[, "extra.code" != names(df)]
  
  # actor.partner.industry
  df[["actor.partner.industry2"]] <- substring(df[["actor.partner.industry"]], 1, 2)
  
  veris <- cbind(veris, getpattern(df, rowname="rowname"))
  
  # renest sequence and store back in veris
  df <- df %>%
    dplyr::group_by(rowname) %>%
    tidyr::nest(.key = "sequence") %>%
    dplyr::arrange(rowname)
  veris[["sequence"]] <- df[["sequence"]]
  ###
  
  print("veris dimensions")
  print(dim(veris))
  
  
  fails <- sapply(colnames(veris), function(x) is.logical(veris[[x]]) & any(is.na(veris[[x]])))
  print(which(fails))
  if (any(fails)) {
    for (i in which(fails)) {
      set(veris, i=which(is.na(veris[[i]])), j=i, value=FALSE)
    }
  } 
  fails <- sapply(colnames(veris), function(x) is.logical(veris[[x]]) & any(is.na(veris[[x]])))
  print(which(fails))
  veris
}

#' Determine the patterns from the 2014 Verizon DBIR
#' 
#' given a verisr object, this will determine which pattern the 
#' incident is in (or "Other" if no pattern is matched). Note the 
#' returned vector will be a factor with ordered levels for 
#' arranging the patterns in an order.
#' 
#' @param sequence the unnested sequence column from a verisr object
#' @param rowname string identifying column uniquely identifying rows
#'     Defaults to 'extra.rowname'
#' @export
#' @examples
#' data(veris.sample, package="verisr")
#' 
#' # produces a vector with 1-to-1 mapping to verisr object
#' pat <- getpattern(veris.sample)
#' 
#' # can summarize the results
#' table(pat)
getpattern <- function(sequence, rowname="extra.rowname") {
  sequence.flat <- verisr::flatten(sequence, level = "sequence", row.name=rowname)
  
  skimmer <- sequence.flat[['sequence.action.physical.variety.Skimmer']] |
    (sequence.flat[['sequence.action.physical.variety.Tampering']] & sequence.flat[['sequence.attribute.confidentiality.data.variety.Payment']])
  espionage <- (sequence.flat[['sequence.actor.external.motive.Espionage']] | 
                  sequence.flat[['sequence.actor.external.variety.State-affiliated']])
  
  pos <- (sequence.flat[['sequence.asset.assets.server.variety.POS controller']] |
            sequence.flat[['sequence.asset.assets.user device.variety.POS terminal']])
  dos <- sequence.flat[['sequence.action.hacking.variety.DoS']]
  webapp <- sequence.flat[['sequence.action.hacking.vector.Web application']]
  webapp <- webapp & !(webapp & dos)
  misuse <- sequence.flat[['sequence.action.Misuse']]
  
  vfilter <- skimmer | espionage | pos | dos | webapp | misuse  
  mal.tmp <- sequence.flat[['sequence.action.Malware']] & 
    !sequence.flat[['sequence.action.malware.vector.Direct install']]
  malware <- (mal.tmp & !vfilter)
  theftloss <- sequence.flat[['sequence.action.error.variety.Loss']] | 
    sequence.flat[['sequence.action.physical.variety.Theft']]
  vfilter <- vfilter | malware | theftloss
  errors <- sequence.flat[['sequence.action.Error']] & !vfilter
  vfilter <- vfilter | errors
  other <- !vfilter
  pats <- data.frame(pos, webapp, misuse, theftloss, errors, malware,
                     skimmer, dos, espionage, other)
  
  patcols  <- c("Point of Sale",
                "Web Applications",
                "Privilege Misuse",
                "Lost and Stolen Assets",
                "Miscellaneous Errors",
                "Crimeware",
                "Payment Card Skimmers",
                "Denial of Service",
                "Cyber-Espionage",
                "Everything Else")
  colnames(pats) <- patcols  
  # Commenting out named.df/retval stuff as single-pattern column is misleadering since pattern is non-exclusively multinomial. - gdb 17-07-07
  # # convert T/F to colname if True
  # named.df <- do.call(cbind, lapply(colnames(pats), function(x) {
  #   ifelse(pats[ ,x], x, NA)
  # }))
  # # now reduce each row to a single label, return the vector
  # retval <- apply(named.df, 1, function(x) {
  #   x[!is.na(x)][1]
  # })
  colnames(pats) <- paste0("pattern.", patcols)
  # retval <- factor(retval, levels=patcols, ordered=T)  
  # cbind(data.table(pattern=retval), pats)
  pats
}

#' Map VERIS fields to data type.
#'
#' Given a json schema for VERIS, this function will return a named vector
#' where the name is the field and the value is the R mode string or "enum".
#'
#' @param schema the merged veris schema in json
#' @param cur the current name (internal)
#' @param outvec the current output vector (internal)
#' @keywords json
parseProps <- function(schema, cur="", outvec=list()) {
  if ('items' %in% names(schema)) {
    if (schema[['items']][['type']] == 'object') {
      # handle lists of objects, which results in a recursive schema
      outvec[[cur]] <- parseProps(schema[['items']][['properties']])
    } else if ('enum' %in% names(schema[['items']])) {
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, "enum")
      outvec <- setNames(outvec, vnames)
    } else {
      outvec <- parseProps(schema[['items']], cur, outvec)
    }
  } else if ('type' %in% names(schema)) {
    if(schema[['type']]=='object') {
      outvec <- parseProps(schema[['properties']], cur, outvec)
    } else if ('enum' %in% names(schema)) {
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, "enum")
      outvec <- setNames(outvec, vnames)
    } else {
      setto <- "character"
      if(schema[['type']] == 'number') {
        setto <- "double"
      } else if (schema[['type']] == 'integer') {
        setto <- "integer"
      # Below three lines added to deal with new logical features in veris 1.3.1. - 17-01-17
      } else if (schema[['type']] == 'boolean') {
        setto <- "logical"
      }
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, setto)
      outvec <- setNames(outvec, vnames)
    }
  } else {
    if (length(names(schema)) > 0) {  # this is the normal case
      for(x in names(schema)) {
        newcur <- ifelse(nchar(cur), paste(cur, x, sep='.'), x)
        outvec <- parseProps(schema[[x]], newcur, outvec)
      }
    } else { # in some cases, a feature may be an object with no properties. without this catch, the column won't be created.  E.g. sequence$discovery_method.unknown - GDB 170626
      outvec[[cur]] <- "logical"
    }
  }
  outvec
}

#' Create a list of all column names expected from JSON schema.
#'
#' Given a json schema for VERIS, this function will return a vector
#' of columns names to be set in the verisr object.  This is a wrapper 
#' around mkenums() which is a recursive function.  This will clean up
#' some of the one-off things we do after reading the schema.
#'
#' @param schema the merged veris schema in json
#' @keywords json
veriscol <- function(schema, a4) {
  rawfields <- mkenums(schema)
  ### Because asset.assets and attribute.confidentiality.data are now hierarchical, no need to
  ###    handle the wonky fields.  - gdb 170616
  # gfields <- c("^ioc" # # "^impact",   # 2.0.6 change, putting impact back in
  #              "attribute.confidentiality.data.amount", 
  #              "asset.assets.amount"
  # )
  # # remove the asset.assets.amount and attribute.confidentiality.data.amount columns that are not associated with their variety from rawfields
  # clean <- rawfields[grep(paste(gfields, collapse="|"), rawfields, invert=T)]
  # # Normally, mkenums returns asset.assets.amount & asset.assets.variety w/o acknowledging the relationship.  The below fixes that.
  # #  Same for attribute.confidentiality.data.variety.  without it it'd just be attribute.confidentiality.data.amount
  # wonkyvariety <- clean[grep('asset.assets.variety|attribute.confidentiality.data.variety', clean)]
  # wonkyamount <- sapply(strsplit(wonkyvariety, "[.]"), function(x) {
  #   x[x=="variety"] <- "amount"
  #   paste(x, collapse=".")
  # })
  # sort(c(wonkyamount, clean))
  ### Replacement for above code.
  ###   NOTE: This is NOT recursive so only checks first level. To make recursive, move
  ###         this functionality into mkenums. - gdb 170616
  ###   NOTE: Because this only filters "^ioc" current
  gfields <- c("^ioc") # fields to remove
  clean <- unlist(lapply(1:length(rawfields), function(x) {
    if ( !any(grepl(paste(gfields, collapse="|"), rawfields[[x]])) ) {
      x
    }
  })) # returns row numbers not including gfields
  rawfields <- rawfields[clean]
  
  # Moved from getverisdf to obviate it
  # Fill in convenience columns
  sequence <- rawfields[['sequence']]
  vnames <- c(names(sequence), names(a4))
  sequence <- c(sequence, rep("logical", length(a4)))
  sequence <- setNames(sequence, vnames)
  rawfields[['sequence']] <- sequence
  
  # return
  # list(rawfields, vnames, sequence) # DEBUG
  rawfields
}

#' Create a raw list of all column names expected from JSON schema.
#'
#' Given a json schema for VERIS, this function will return a vector
#' of columns names to be set in the verisr object. 
#'
#' @param schema the merged veris schema in json
#' @param cur the current named value
#' @param outvec the vector passed around building the output
#' @keywords json
mkenums <- function(schema, cur="", outvec=list()) {
  # HANDLES LISTS/ARRAYS
  if ('items' %in% names(schema)) {
    # HANDLES STRINGS IN LISTS/ARRAYS
    if (schema[['items']][['type']] == 'object') {
      # handle lists of objects, which results in a recursive schema
      outvec[[cur]] <- mkenums(schema[['items']][['properties']])
    } else if ('enum' %in% names(schema[['items']])) {
      vnames <- c(names(outvec), paste(cur, schema[['items']][['enum']], sep='.'))
      outvec <- c(outvec, rep("logical", length(schema[['items']][['enum']])))
      outvec <- setNames(outvec, vnames)
    } else {
      # HANDLES ALL ELSE, PROBABLY SHOULDN'T BE REACHED
      # warning(paste0("mkenums reached a list of items which had neither an enumeration, nor were an object with enumeration ", cur, " ."))
      type <- list("number"="double", "integer"="integer", "boolean"="logical", "string"="character")[[schema[['items']][['type']]]]
      if (is.null(type)) { error(paste0("In mkenums <list, no enum>, schema[['items']][['type']] ", schema[['items']][['type']], " does not correspond to a column type.")) }
      # add the enums to the output vector as name:type items in the output vector.
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, type)
      outvec <- setNames(outvec, vnames)
    }
  # if there's no 'items', it's not an array (list) and so we need to check type
  } else if ('type' %in% names(schema)) {
    # HANDLES OBJECTS
    if(schema[['type']]=='object') {
      # if it's an object, we need to recurse on the properties portion of the object
      outvec <- mkenums(schema[['properties']], cur, outvec)
    # HANDLES STRINGS
    } else if ('enum' %in% names(schema)) {
      vnames <- c(names(outvec), paste(cur, schema[['enum']], sep='.'))
      outvec <- c(outvec, rep("logical", length(schema[['enum']])))
      outvec <- setNames(outvec, vnames)
    # HANDLES INTEGERS, NUMERICS, and BOOLEANS
    } else {
      type <- list("number"="double", "integer"="integer", "boolean"="logical", "string"="character")[[schema[['type']]]]
      if (is.null(type)) { error(paste0("In mkenums <not list, no enum>, schema[['type']] ", schema[['type']], " does not correspond to a column type.")) }
      # add the enums to the output vector as name:type items in the output vector.
      vnames <- c(names(outvec), cur)
      outvec <- c(outvec, type)
      outvec <- setNames(outvec, vnames)
    }
  } else {
    for(x in names(schema)) {
      # HANDLES ANYTHING WITHOUT A TYPE
      # (does so by just recursing on all properties of the thing)
      newcur <- ifelse(nchar(cur), paste(cur, x, sep='.'), x)
      outvec <- mkenums(schema[[x]], newcur, outvec)
    }
  }
  outvec
}

#' Merges veriscol and parseProps in a single object.
#'
#' Given the schema object, will identify each column type to be 
#' created into a data.table.
#' 
#' WARNING: getverisdf is dead code.  All functionality has been 
#'   moved to veriscol. It only added the typing of enums and a4 
#'   columns.  mkenums has been rewritten to just output the needed 
#'   typing since it's only used here through veriscol.  a4 was 
#'   moved to veriscol since mkenums is recursive.  getverisdf added 
#'   an unnecessary level of abstraction (and a second recursive 
#'   parse through the schema in the call to 'parseProps' and so has 
#'   been removed from usage.) - gdb 170619
#' 
#' TODO: need to add the convenience fields in here.
#'
#' @param lschema the json schema object for VERIS
#' @param a4 the a4 named vector for convenience fields
getverisdf <- function(lschema, a4) {
  # get a named vector of VERIS objects
  # e.g. c("action.hacking.variety" = "enum")
  vtype <- parseProps(lschema) # named
  # get a vector of veris columns
  # e.g. c("action.hacking.variety.Brute force", "action.hacking.variety.SQLi"...)
  vfield <- veriscol(lschema) # not named
  out <- sapply(vfield, function(x) {
    ret <- vtype[x]
    if (is.na(ret)) {
      temp <- unlist(strsplit(x, '[.]'))
      base <- paste(temp[1:(length(temp)-1)], collapse='.')
      ret <- vtype[base]
    }
    ifelse(ret=="enum", "logical", ret)
  }, USE.NAMES=F)
  out <- c(out, rep("logical", length(a4))) # add the convenience columns
  setNames(out, c(vfield, names(a4)))
}

#' Return a dense list of only the VERIS variables and values in the JSON.
#' 
#' Given an incident in json format, it will process the file and 
#' return a simple list object where each element is named with 
#' the field name and the value is the value read in, or True if the
#' field is an enumeration.
#' 
#' @param json the json object from reading in a file.
#' @param vtype the vtype ojbect from parseProps()
#' @param cur the current field name as it is being built
#' @param outlist the return value being passed internally
nameveris.recurs <- function(json, vtype, cur=NULL, outlist=list()) {
  # Three options:
  #   named values (fields to loop through)
  #   looped variety object (sequence, asset.assets, data variety, etc)
  #   a value itself
  
  # if named values, loop through each of the children and recurse to myself  
  if (length(names(json))>0) { # names >0: json is an object, not a list, so recurse
    
    for(x in names(json)) {
      
      curname <- ifelse(is.null(cur), x, paste(cur, x, sep='.'))
      if ((length(names(json[[x]])) == 0) & (class(json[[x]]) == "list")) { # list object
        outlist[[curname]] <- lapply(json[[x]], nameveris.recurs, vtype=vtype[[curname]])
      } else { # object object
        outlist <- nameveris.recurs(json[[x]], vtype, cur=curname, outlist)
      }
    }
    
  } else { # no names and length > 0 or 
    if (cur %in% names(vtype)) {
      if (vtype[cur] == "enum") {
        for(x in json) {
          curname = paste(cur, x, sep='.')
          outlist[[curname]] = TRUE
        }
      } else {
        if (!mode(json) %in% c("character", "numeric")) {
          cat('mode of', cur, "is", mode(json), "\n")
        }
        outlist[[cur]] = json
      }
    } else if (cur %in% c("actor.unknown", "action.unknown")) {
      outlist[[sub('u', 'U', cur)]] <- TRUE
      # } else {
      # warning(paste("Invalid data in JSON, dropping value of", cur))
    }
  } # names > 0 and json == 0 not handled.  not possible
  outlist
}

#' Return a dense and complete list of VERIS variables and values
#' 
#' Given an incident in json format, it will process the file and 
#' return a simple list object where each element is named with 
#' the field name and the value is the value read in, or True if the
#' field is an enumeration.
#' 
#' @param json the json object from reading in a file.
#' @param a4 the a4 object from geta4names()
#' @param vtype the vtype object from parseProps()
nameveris <- function(json, a4, vtype) {
  olist <- nameveris.recurs(json, vtype)
  # start simple, with the actor, action, asset and attribute fields
  for(a4name in names(a4)) {
    olist[["sequence"]] <- lapply(olist[['sequence']], function(step) {
      if (any(grepl(paste0('^', a4[a4name]), names(step)))) {
        step[[a4name]] = TRUE
      }
      step
    })
  }
  olist
}

#' Convenience function for the a4 names and values
#' 
#' This returns a named vector where the names are the column names 
#' in the final verisr data table and the valus are suitable for using
#' in a regex in the existing column names.  
geta4names <- function() {
  convenience <- function(nm) {
    out <- tolower(nm)
    setNames(tolower(nm), nm)
  }
  actor <- convenience(paste('actor', 
                             c('External', 'Internal', 'Partner', 'Unknown'), 
                             sep="."))
  action <- convenience(paste('action', 
                              c('Malware', 'Hacking', 'Social', 'Physical', 
                                'Misuse', 'Error', 'Environmental', 'Unknown'), 
                              sep="."))
  attribute <- convenience(paste('attribute', 
                                 c('Confidentiality', 'Integrity', 'Availability'), 
                                 sep="."))
  # asset.assets.<type> is now value (granted a list of DF's) within the schema and verisr
  # as such, making this consistant with the other 3A's.  - gdb 170616
  # assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
  #              "P "="Person", "T "="Kiosk/Term", "Un"="Unknown", "E"="Embedded")
  #asset <- setNames(paste('asset.assets.variety', names(assetmap), sep='.'),
  #                  paste('asset.variety', assetmap, sep='.'))
  asset <- convenience(paste('asset.assets', 
                              c('Server', 'Network', 'User Dev', 'Media', 
                                'Person', 'Public Terminal', 'Embedded', 'Unknown', 'Other'), 
                              sep="."))
  discovery <- convenience(paste('discovery_method', 
                                 c('Internal', 'External', 'Partner', 'Other', 'Unknown'), 
                                 sep="."))
  c(actor, action, asset, attribute, discovery)  
}


#' recursively build a data structure to store for the veris record
#' 
#' This recursively builds a list of the form 
#' list(DT, name1=list(), name2=list(), etc) where name is the name
#' of a column which will be a list of data frames and list() is
#' the same recursive structure, starting with a DT.
#' 
#' @param columns list. The output of veriscol listing columns & 
#' types.  Likely 'vft'
#' @param nrow integer. The number of rows to create (likely 'numfil'
#' or 'length(jfiles)')
#' @return a list of data tables and lists to fill
buildVeris <- function(columns, nrow) {
  veris <- list()
  
  # for non-list columns, create a data table with columns of the correct type and store it in the first item of the output list
  non_list_columns <- columns[lapply(columns, class) != "list"] # added to speed up next block by preventing repeated subsetting
  veris[[1]] <- data.table::as.data.table(lapply(seq_along(non_list_columns), function(i) {
    if (non_list_columns[i]=="character") rep(NA_character_, nrow)
    else if (non_list_columns[i]=="logical") rep(FALSE, nrow) # The 'FALSE' will be counted even if a row is not imported while an NA will not.  Unfortunately 'FALSE' is not filled in at any point and unfilled rows should end up filtered anyway so leaving 'FALSE' rather than 'NA'. - gdb 170621
    else if (non_list_columns[i]=="integer") rep(NA_real_, nrow)
    else if (non_list_columns[i]=="double") rep(NA_real_, nrow)
    else warning(paste0("Column ", names(columns[i]), " of class ", columns[i], " not included!") )
  }))
  data.table::setnames(veris[[1]], names(non_list_columns))
  
  # recurse on list columns in columns
  veris <- c(veris, lapply(columns[lapply(columns, class) == "list"], buildVeris, nrow=1))
  
  # return
  veris
}

#' Join the list of data tables from buildVeris into a data frame
#' 
#' This effectively produces the recursive veris data structure
#' 
#' @param veris list of data tables in buildVeris structure
#' @return data frame. Data frame with list columns
joinVeris <- function(veris) {
  out <- as.data.frame(veris[[1]])
  if (length(veris) > 1) { # if you try and iterate over a zero-length, vector, it contain's 'NA' rather than not iterating
    for(n in names(veris)[2:length(veris)]) {
      if (is.null(nrow(veris[[n]])) || nrow(veris[[n]]) <= 1) {
        df <- do.call(data.frame, list(joinVeris(veris[[n]]), "check.names"=FALSE, "stringsAsFactors"=FALSE)) # check.names required. Otherwise spaces are replaced with periods.
        out[[n]] <- replicate(nrow(out), df, simplify=FALSE)
      } else {
        df2 <- joinVeris(veris[[n]])
        out[[n]] <- replicate(nrow(out), df2, simplify=FALSE)
      }
    }
  }
  
  # return
  out
}

#' Recursively fill the data table list structure from buildVeris()
#' 
#' NOTE: list columns in empty list columns are not created as it
#'     would require that the intermediate list column have 
#'     data frames which would then have at least one row to contain
#'     the second empty list column.  This would potentially create
#'     extra samples.
#'     
#'     That said, the sample is the record itself which exists and
#'     so it may not matter for sample and in fact be better to have
#'     the all list columns contain data frames with a ddefault row
#'     down to the depth of the schema.  This would ensure that all
#'     endpoints of the schema exist in the record, potentially 
#'     making parsing easier. (Without it, the enumeration function
#'     will need to specifically catch lists that do not recurse to
#'     the depth of the schema.) (This may not actually be a 
#'     practical immediate problem as the only embedded lists of 
#'     lists are under 'sequence' which must exist.)
#'     
#'     Ultimately, how lists of objects are enumerated will decide
#'     the best approach.  (I.e. accurately counting assets of a 
#'     given type, for example 'People', under multiple sequence 
#'     steps per record across multiple records.)
#' 
#' @param records list. The incident as interpreted by nameveris()
#' @param veris.built list outputted by buildVeris.  This is used as
#'     a template for the record.
#' @param filename character. record file name. Used for debug text.
#' @param level character. The column currently being filled.
#'     defaults to 'root'.
#' @return list of data tables of the structure from buidlVeris()
fillVeris <- function(records, veris.built, filename = "not supplied", level="root") {
  # we're going to iterate over the records (primarily during recursion), so if records is a single record, we need to put it in a list.
  if ("incident_id" %in% names(records)) records <- list(records)
  
  ret <- lapply(records, function(record) {
    row <- as.data.frame(veris.built[[1]][1, ])
    if (length(veris.built) > 1) {
      for (name in names(veris.built[2:length(veris.built)])) {
        # row[[name]] <- replicate(nrow(row), data.frame(), simplify=FALSE) # if row can be more than 1 long
        row[[name]] <- list(data.frame())
      }
    }
    
    ### Extra convoluted no match checking to handle things like matching action.unknown to action.Unknown. 
    ###    The backward approach to lowering names is to avoid messing upper-casing of a single character internal to the string.
    # We need caps names first so they are the value. lower case will be the names since we need them as a reference
    # schema_names <- c(colnames(veris.built[[1]]), names(veris.built)[2:length(veris.built)]) # old
    schema_names <- colnames(veris.built[[1]])
    if (length(veris.built) > 1) {
      schema_names <- c(schema_names, names(veris.built)[2:length(veris.built)])
    }
    # check for names in record not in the schema
    nomatch <- !(names(record) %in% schema_names)
    # if everything matches, we can skip the shinanigans
    if (any(nomatch)) { 
      # fill vector with lower case names.  We'll need the cross-match later
      names(schema_names) <- lapply(schema_names, tolower)
      # Are any of the non-matching columns convenience columns?
      convenience_match <- (names(record) %in% names(schema_names)) & nomatch
      nomatch <- !convenience_match & nomatch
      # If any where convenience columns, fill them in with the convenience column name (so action.unknown -> action.Unknown) and set TRUE
      if (any(convenience_match)) {
        record <- record[!convenience_match] # remove the lower case convenience columns with no corresponding content.
      }
      # if any no matches still exist, alert on them.
      if (any(nomatch)) {
        warning(paste0("Column[s]: \n", paste0("  \"", names(record)[nomatch], "\"", collpase=", "), 
                       "\nNot found in schema column ", level, ".  Source file:", filename, "\n"))
      }
    }
    
    lColsNames <- names(record[lapply(record, class) == "list"])
    lCols <- lapply(lColsNames, function(name) {
      list(fillVeris(record[[name]], veris.built=veris.built[[name]], filename=filename, level=name))
    })
    if(length(lCols) >= 1) row[1, lColsNames] <- lCols # add list columns if there are any
    
    nonlCols <- names(record[lapply(record, class) != "list"])
    
    if (!is.null(nonlCols)) {
      # some strings can have multiple values 
      if (any(unlist(lapply(record[nonlCols], length))>1)) {
        record[nonlCols] <- lapply(record[nonlCols], function(feature) {
          ifelse(length(feature) > 1, paste(feature, sep=","), feature)
        })
      }
      # the setdiff... is to prevent trying to write data to non-matching columns.
      row[1, nonlCols] <- data.frame(record[nonlCols], check.names=FALSE, stringsAsFactors=FALSE)[1, ]
    }
    
    #return
    # if (ncol(row) == 1) {
    #   row
    # } else {
    #   row[1, ]
    # }
    if (nrow(row) > 1) {
      message(paste0(level, " has more than one row out per row which should not be possible."))
    }
    row
  })
  
  # join rows
  ret <- do.call(rbind.data.frame, ret)
  
  #return  
  ret
}


#' Displays a useful description of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @method summary verisr
#' @export
summary.verisr <- function(object, ...) {
  veris <- object
  cat(paste(nrow(veris), "incidents in this object.\n"))
  actor <- getenum(veris, "actor", add.freq=T)
  action <- getenum(veris, "action", add.freq=T)
  asset <- getenum(veris, "asset.variety", add.freq=T)
  attribute <- getenum(veris, "attribute", add.freq=T)
  outlist <- list(actor=actor, action=action, asset=asset, attribute=attribute)
  full <- lapply(names(outlist), function(n) {
    thisdf <- outlist[[n]]
    ret <- unlist(lapply(seq(nrow(thisdf)), function(i) {
      rep(thisdf$enum[i], thisdf$x[i])
    }))
    ret
  })
  maxline <- max(sapply(full, length))
  temp.out <- as.data.frame(lapply(full, function(x) {
    c(as.character(x), rep(NA, maxline-length(x)))
  }))
  names(temp.out) <- names(outlist)
  outs <- summary(temp.out, maxsum=100)
  outs[grep("^NA\'s", outs)] <- ""
  outs
}

#' Displays a four panel barplot of a verisr object
#' 
#' @param object veris object to summarise
#' @param ... other arguments ignored (for compatibility with generic)
#' @keywords internal
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @method plot verisr
#' @export
plot.verisr <- function(x, y, ...) {
  # @importFrom ggplot2 ggplotGrob
  veris <- x
  actor <- getenum(veris, "actor", add.freq=T)
  action <- getenum(veris, "action", add.freq=T)
  asset <- getenum(veris, "asset.variety", add.freq=T)
  attribute <- getenum(veris, "attribute", add.freq=T)
  a4 <- list(actor=actor, action=action,
             asset=asset,
             attribute=attribute)
  ht_mult <- c(0.22)  # multiplier for each row
  highest <- (max(sapply(a4, nrow))*ht_mult)
  
  plots <- lapply(names(a4), function(x) {
    this.ht <- (nrow(a4[[x]])*ht_mult)
    ht.diff <- (highest - this.ht) * 0.5
    simplebar(a4[[x]], x, plot.margin=unit(c(0,0,ht.diff,0), "npc"))
  })
#   foo <- arrangeGrob(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrow=2)
#   print(foo)

#   ggplotGrob <- ggplot2::ggplotGrob
   print(do.call(arrangeGrob, c(plots, nrow=2)))
#   print(do.call(grid.arrange, c(plots, nrow=2)))
}