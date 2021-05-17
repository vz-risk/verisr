#' Read in all the VERIS incidents (JSON files) in a given directory.
#'
#' This function will iterate through all the JSON files (regex pattern of "json$") in
#' the given directory and parse it as an encoded VERIS record.  
#' 
#' It also provides the option of passing a vector of individual files.  R's
#' directory and file parsing functions (list.dirs and list.files) are relatively
#' slow and files are basically enumerated twice if you specify directories, (once
#' while you enumerated the directories and once while you enumerated the files).  
#' Specifying individual files should substantially decrease this time and allows 
#' the user to use OS-specific file enumeration processes that may be faster than R's.
#' 
#' This function requires that a JSON schema be available for the VERIS data.  
#' If the variable is not specified, it will attempt to grab the 
#' "verisc-merged.json" schema from
#' https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json.
#' 
#
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
#'   * *discovery_method* will return top level discovery_method categories
#'   * *value_chain* will return top level value_chain categories
#' 
#' The victim.secondary.victim_id, external.actor.region, and any other free
#' text field that can be repeated is being collapsed into a single string 
#' seperated by a comma at the moment.  If that poses a challnge, open an issue
#' on it.
#'
#' @param dir The directory to list through.  This may be a vector of 
#' directorites, in which case each all the matching files in each 
#' directory will be loaded.
#' @param files a chatacter vector of individual json files to be parsed.  
#' These will be added to any files found in directories specified 
#' with 'dir'.  Any duplicates between files found in 'dir' and 'files'
#' will be removed.
#' @param schema a full veris schema with enumerations included.
#' @param progressbar a logical value to show (or not show) a progress bar
#' @keywords json
#' @import data.table
#' @import RCurl
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
json2veris <- function(dir=c(), files=c(), schema=NULL, progressbar=F) {

  savetime <- proc.time()
  # if no schema, try to load it from github
  if (missing(schema)) {
    x <- getURL("https://raw.githubusercontent.com/vz-risk/veris/master/verisc-merged.json")
    lschema <- rjson::fromJSON(json_str=x)
  } else {
    lschema <- rjson::fromJSON(file=schema)
  }  
  # create listing of files
  jfiles <- data.frame(jfile = unique(c(files, unlist(sapply(dir, list.files, pattern = "zip$|json$", full.names=T))))) #files and dirs
  if (nrow(jfiles) == 0) {stop("No json or zip files were found in the directories or files provided. Please double check your input.")}
  jfiles[['ftype']] <- "file"
  
  ### Now we're going to try and parse out the zip files and turn them into text strings
  zfiles <- jfiles[grepl("\\.zip$", jfiles$jfile), ]
  jfiles <- jfiles[!grepl("\\.zip$", jfiles$jfile), ]

  for (zfile in zfiles$jfile) { # for each zfile we need to parse it's content and 
    # get the internal files we need to operate over
    content <- unzip(zfile, list=TRUE)
    content <- as.character(content[grepl("[.]json$", content$Name), "Name"])
    # We'll parse each file to a string
    zipped_strings <- unlist(lapply(content, function(filename) {
      con <- unz(zfile, filename, open="") # open set to "" so it isn't opened initially so we can open it and set blocking=TRUE
      open(con, open="rt", blocking=TRUE) # open it read/text with blocking=TRUE as readLines won't read the last line if blocking is not true on a read text connection
      s <- paste(readLines(con, warn=FALSE), sep=" ", collapse=" ") # parse all the files to strings
      close(con)
      s
    }))
    # While we shouldn't zip unjoined files, we'll test for it
    # This test is flimsy as it just looks for a "[" as the starting character, but the alternative of checking types with jqr is complex.
    joined <- grepl("^\\s*\\[", zipped_strings) # if it's an array the first character needs to be an open bracket
    
    # We'll warn if a zipped file has normal (unjoined) json in it
    if (any(!joined)) {
      warning(paste0("The zip file ", zfile, " contains files that aren't joined VERIS json.  ", 
                     "This is very slow to process though it will work.  ", 
                     "To improve speed, only included joined json (a list of VERIS objects) in zipped json."))
    }
    
    # we then bind the json strings from both individual files and joine dfiles
    jfiles <- rbind(jfiles, data.frame(
      jfile=c(zipped_strings[!joined], # individual files
              unlist(do.call(c, lapply(zipped_strings[joined], function(s) {jqr::jq(s, ".[]")}))) #joined files separated with qjr ".[]" query
      ),
      ftype="string" # we need to identify these as strings as they need to be fed to a different argument of rjson::fromJSON
    ))
  }
    
  numfil <- nrow(jfiles) # was length, but now jfiles is a data.frame rather than a 
  # need to pull these before we loop, used over and over in loop
  a4 <- geta4names() # just returns a (named) character vector of the 4A's and their next level values.  i.e. actor.External
  vtype <- parseProps(lschema) # recursively parse the schema. returns a named character vector. names=column, value=class. (no enumerations)
  # get a named vector of field and types
  vft <- getverisdf(lschema, a4) # we now have all the columns we need
  # now create a data table with the specific blank types
  # we just pulled from getverisdf()
  veris <- as.data.table(lapply(seq_along(vft), function(i) {
  #veris <- as.data.frame(lapply(seq_along(vft), function(i) {
    if (vft[i]=="character") rep(NA_character_, numfil)
    else if (vft[i]=="logical") rep(FALSE, numfil)
    else if (vft[i]=="integer") rep(NA_real_, numfil)
    else if (vft[i]=="double") rep(NA_real_, numfil)
    else if (vft[i]=="list") vector("list", numfil)
  }))
  setnames(veris, names(vft))
  # get a text progress bar going
  pb <- NULL
  if (progressbar) pb <- txtProgressBar(min = 0, max = nrow(jfiles), style = 3)
  # in each file, pull out the values and fill in the data table
  event_chain <- vector("list", numfil) # event_chain will be the only list column, so create teh column separately to add back in. - GDB 180118
  for (i in 1:nrow(jfiles)) {
  #furrr::future_map(1:nrow(jfiles), function(i) { # Tried parallelization, however since the `veris` object is not shared writable across futures, it doesn't get written correctly. - GDB 201123
      #message(i) # DEBUG
    # we have to use rjson here because other parsers fail
    if (as.character(jfiles[i, "ftype"] == "file")) {
      json <- rjson::fromJSON(file=as.character(jfiles[i, "jfile"]), method='C') 
    } else if (as.character(jfiles[i, "ftype"] == "string")) {
      json <- rjson::fromJSON(json_str=as.character(jfiles[i, "jfile"]), method='C') 
    } else {
      # should not be reachable.
      stop(paste0("Internal error.  jfile ", jfiles[i, "jfile"], "has type ", jfiles[i, "ftype"], " which is unprocessable."))
    }
    nfield <- nameveris(json, a4, vtype)
    names
    if (length(nfield)==0) warning(paste("empty json file parsed from", jfiles[i, "jfile"]))
    nomatch <- !(names(nfield) %in% colnames(veris))
    if (any(nomatch)) {
      warning(paste0("Column[s]: \n", paste0("  \"", names(nfield)[nomatch], "\"", collpase=", "), 
                     "\nNot found in schema, source file:", jfiles[i, "jfile"]))
    }
    for(x in names(nfield)) {
      if (x != "plus.event_chain") { # we will handle plus.event_chain as a 1-off below. - GDB 180118
        if(length(nfield[[x]]) > 1) {
          tt <- tryCatch(set(veris, i=as.integer(i), j=x, value=paste(nfield[[x]], collapse=",")),
                         error=function(e) e, warning=function(w) w)
        } else { 
          tt <- tryCatch(set(veris, i=as.integer(i), j=x, value=nfield[[x]]),
                         error=function(e) e, warning=function(w) w)
        }
      }
      if(is(tt,"warning")) {
        cat(paste0("Warning found trying to set ", i, ", \"", x, "\" for \"", nfield[[x]], "\"\n"))
        cat("  length of assignment:", length(nfield[[x]]), "\n")
        cat("  in", i, jfiles[i, "jfile"], "\n")
        print(tt)
        cat("\n")
      }
      
    }
    # fill in the event_chain list. - GDB 180118
    if ("plus.event_chain" %in% names(nfield)) {
      event_chain[[i]] <- nfield[['plus.event_chain']]
    }
    if (!is.null(pb)) setTxtProgressBar(pb, i)
  }
#  }) # pairs with furrr::map( above.
  if (!is.null(pb)) close(pb)
  # veris <- veris[, !grepl("event_chain", names(veris))]
  veris[, (grep("event_chain[.]", names(veris), value=TRUE)) := NULL]
  set(veris, j='plus.event_chain', value=event_chain) # store the event_chain list into the data.table as a column - GDB 180118
  # Not Applicable is replaced with NA for consistency
  # WARNING: the below line causes duplicates and overwrites legitimate 'Not Applicable' enumerations (such as plus.attack_difficulty_initial) so removing
  #  Instead, need to standardize NAs in veris.
  # colnames(veris) <- gsub('Not Applicable', 'NA', colnames(veris), ignore.case=TRUE) # this causes more problems than it solves. 17-01-17 GDB
  veris <- post.proc(veris)
  gc(verbose=FALSE)
  #veris <- as.data.frame(veris) # convert data.table to data.frame because data tables are evil. - 17-01-17
  data.table::setDF(veris)
  # veris[['plus.event_chain']] <- event_chain
  class(veris) <- c("verisr", class(veris))
  if(progressbar) {
    print(proc.time() - savetime)
  }
  gc(verbose=FALSE)
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
post.proc <- function(veris) {
  # orgsize
  small <- c("victim.employee_count.1 to 10", "victim.employee_count.11 to 100", 
             "victim.employee_count.101 to 1000", "victim.employee_count.Small")
  large <- c("victim.employee_count.1001 to 10000", "victim.employee_count.10001 to 25000", 
             "victim.employee_count.25001 to 50000", "victim.employee_count.50001 to 100000", 
             "victim.employee_count.Over 100000", "victim.employee_count.Large")
  veris[ , victim.orgsize.Small := rowSums(veris[ ,small, with=FALSE]) > 0]
  veris[ , victim.orgsize.Large := rowSums(veris[ ,large, with=FALSE]) > 0]
  veris[ , victim.orgsize.Unknown := rowSums(veris[ ,c(small,large), with=FALSE]) > 0]
  # victim.industry
  ind2 <- substring(unlist(veris[ ,"victim.industry", with=F], use.names=F), 1L, 2L)
  # want an enumeration now, instead of a single list.
  # But Kevin wants both so he is uncommenting the line that Jay commented
  veris[ , victim.industry2 := ind2]
  ## Replacing `unique(ind2)` with `naics2s` as not only does it add bad NAICS,
  ##  if it's a small dataset, it misses legit ones. - GDB 181208
  ## Current as of 181208
  naics2s <- c("00", "11", "21", "22", "23", "31", "32",
               "33", "42", "44", "45", "48", "49", "51",
               "52", "53", "54", "55", "56", "61", "62",
               "71", "72", "81", "92")
  #for(x in unique(ind2)) {
  for(x in naics2s) {
    iname <- paste0('victim.industry2.', x)
    veris[ ,(iname):=(ind2==x)] #, with=F
  }
  ## industry3 may require more prep work since dashes are allowed.
  veris[ , victim.industry3 := substring(unlist(veris[ ,"victim.industry", with=FALSE], 
                                                use.names=F), 1L, 3L)]

  ### victim.industry.name
  data(industry2, envir = environment(), package='verisr')
  #veris$victim.industry.name <- sapply(veris$victim.industry2, function(x) {
  #  ifelse(x %in% industry2$code, industry2$shorter[which(industry2$code==x)], "Unknown")
  #})
  ind_updated <- industry2[, c("code", "shorter")]
  names(ind_updated) <- c("victim.industry2", "victim.industry.name")
  ind_updated <- data.table::as.data.table(ind_updated)
  veris[ind_updated, on="victim.industry2", victim.industry.name := i.victim.industry.name]
  data.table::set(veris,which(is.na(veris[["victim.industry.name"]])),"victim.industry.name","Unknown")
  
  # actor.partner.industry
  veris[ , actor.partner.industry2 := substring(unlist(veris[ ,"actor.partner.industry", with=FALSE], 
                                                use.names=F), 1L, 2L)]
  veris[ , actor.partner.industry3 := substring(unlist(veris[ ,"actor.partner.industry", with=FALSE], 
                                                use.names=F), 1L, 3L)]
  
  # patterns
  #veris <- cbind(veris, getpattern(veris))
  #patterns <- data.table::as.data.table(getpattern(veris)) # 2014 patterns
  #veris <- data.table::setDT(unlist(list(veris, patterns), recursive = FALSE), check.names=FALSE)
  veris <- verisr::add_patterns(veris, # all arguements below are the defaults.
                                centroids = verisr:::models_to_centroids(), # 2021 patterns
                                prefix = "pattern",
                                replace = TRUE, 
                                clusters = FALSE, 
                                threshold = 0.1,
                                veris_update_f = NULL) # NOTE: this will need to change for verisr past 1.3.5
  gc(verbose=FALSE)
  
  print("veris dimensions")
  print(dim(veris))
  fails <- sapply(colnames(veris), function(x) is.logical(veris[[x]]) & any(is.na(veris[[x]]))) # takes 6gb ram but gc() cleans up
  gc(verbose=FALSE)
  print(which(fails))
  if (any(fails)) {
    for (i in which(fails)) {
      set(veris, i=which(is.na(veris[[i]])), j=i, value=FALSE)
    }
  } 
  fails <- sapply(colnames(veris), function(x) is.logical(veris[[x]]) & any(is.na(veris[[x]]))) # takes 6gb ram but gc() cleans up
  gc(verbose=FALSE)
  print(which(fails))
  
  # Add this to ensure that if the 'confidentiality' section is empty, data_disclosure is marked 'no' (i.e. not a breach) - GDB 200428
  # NOTE: This may not be the best choice.  We may want to mark these 'unknown', but if we had any reason to believe it was a breach, it should have been marked at least 'potentially'.
  veris$attribute.confidentiality.data_disclosure.No <- veris$attribute.confidentiality.data_disclosure.No |
    unlist(apply(veris[, grepl("^attribute.confidentiality", names(veris)), with=FALSE], MARGIN=1, function(v) {
      # apply() casts to character
      v <- gsub("FALSE", "", v) # make "FALSE" into ""
      v <- gsub("0", "", v) # makes numeric zero columns 0
      all(na.omit(v) == "") # are any not NA and not FALSE?
  })) # takes 6GB of ram
  gc(verbose=FALSE)
  
  veris
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
parseProps <- function(schema, cur="", outvec=NULL) {
  if ('items' %in% names(schema)) {
    if ('enum' %in% names(schema[['items']])) {
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
    for(x in names(schema)) {
      newcur <- ifelse(nchar(cur), paste(cur, x, sep='.'), x)
      outvec <- parseProps(schema[[x]], newcur, outvec)
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
veriscol <- function(schema) {
  rawfields <- mkenums(schema)
  gfields <- c("^ioc", # "^impact",   # 2.0.6 change, putting impact back in
               "attribute.confidentiality.data.amount", 
               "asset.assets.amount")
  # remove the asset.assets.amount and attribute.confidentiality.data.amount columns that are not associated with their variety from rawfields
  clean <- rawfields[grep(paste(gfields, collapse="|"), rawfields, invert=T)]
  # Normally, mkenums returns asset.assets.amount & asset.assets.variety w/o acknowledging the relationship.  The below fixes that.
  #  Same for attribute.confidentiality.data.variety.  without it it'd just be attribute.confidentiality.data.amount
  wonkyvariety <- clean[grep('asset.assets.variety|attribute.confidentiality.data.variety', clean)]
  wonkyamount <- sapply(strsplit(wonkyvariety, "[.]"), function(x) {
    x[x=="variety"] <- "amount"
    paste(x, collapse=".")
  })
  wonkytype <- sapply(strsplit(wonkyvariety, "[.]"), function(x) {
    x[x=="variety"] <- "type"
    paste(x, collapse=".")
  })
  sort(c(wonkyamount, clean))
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
mkenums <- function(schema, cur="", outvec=NULL) {
  # HANDLES LISTS/ARRAYS
  if ('items' %in% names(schema)) {
    # HANDLES STRINGS IN LISTS/ARRAYS
    if ('enum' %in% names(schema[['items']])) { 
      outvec <- c(outvec, paste(cur, schema[['items']][['enum']], sep='.')) 
    } else {
      # HANDLES ALL ELSE, MOST LIKELY OBJECTS IN LISTS/ARRAYS
      outvec <- mkenums(schema[['items']], cur, outvec) # otherwise, recurse on each item
    }
  # if there's no 'items', it's not an array (list) and so we need to check type
  } else if ('type' %in% names(schema)) {
    # HANDLES OBJECTS
    if(schema[['type']]=='object') {
      # if it's an object, we need to recurse on the properties portion of the object
      outvec <- mkenums(schema[['properties']], cur, outvec)
    # HANDLES STRINGS
    } else if ('enum' %in% names(schema)) {
      outvec <- c(outvec, paste(cur, schema[['enum']], sep='.'))
    # HANDLES INTEGERS, NUMERICS, and BOOLEANS
    } else {
      outvec <- c(outvec, cur)
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
  # add 'plus.event_chain' to names - GDB 180118
  out <- c(out, rep("logical", length(a4)), "list") # add the convenience columns
  setNames(out, c(vfield, names(a4), "plus.event_chain"))
  # out <- c(out, rep("logical", length(a4))) # add the convenience columns
  # setNames(out, c(vfield, names(a4)))
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
  # Four options:
  #   named values (fields to loop through)
  #   looped variety object (asset.assets or data variety)
  #   event chain
  #   a value itself
  
  # if named values, loop through each of the children and recurse to myself
  if (length(names(json))>0) {
    for(x in names(json)) {
      curname <- ifelse(is.null(cur), x, paste(cur, x, sep='.'))
      outlist <- nameveris.recurs(json[[x]], vtype, cur=curname, outlist)
    }
  # if one of the repeated values, handle uniquely
  } else if (cur %in% c('asset.assets', 'attribute.confidentiality.data')) {
    for(x in json) {
      if ('variety' %in% names(x)) {
        curname = paste(cur, 'variety', x[['variety']], sep='.')
        outlist[curname] <- TRUE
        if ('amount' %in% names(x)) {
          curname = paste(cur, 'amount', x[['variety']], sep='.')
          outlist[curname] <- x[['amount']]
        }
      }
    }
  # if this is an event chain, handle uniquely
  } else if (cur == "plus.event_chain") {
    outlist[['plus.event_chain']] = do.call(dplyr::bind_rows, json) # stores DFs vs list-of-list - GDB 180118
    # outlist[['plus.event_chain']] = json # stores list-of-list vs DF - GDB 180118
  # else this is field to assign, assign it
  } else {
    if (cur %in% names(vtype)) {
      if (vtype[cur] == "enum") {
        for(x in json) {
          curname = paste(cur, x, sep='.')
          outlist[[curname]] = TRUE
        }
      } else {
        if (!mode(json) %in% c("character", "numeric", "logical")) {
          cat('mode of', cur, "is", mode(json), "\n")
        }
        outlist[[cur]] = json
      }
    } else if (cur %in% c("actor.unknown", "action.unknown")) {
      outlist[[sub('u', 'U', cur)]] <- TRUE
    # } else {
      # warning(paste("Invalid data in JSON, dropping value of", cur))
    }
  }
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
    if (any(grepl(paste0('^', a4[a4name]), names(olist)))) {
      olist[[a4name]] = TRUE
    }
  }  
  olist
}

#' Convenience function for the a4 names and values + discovery_method
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
  discovery_method <- convenience(paste('discovery_method',
                                 c('External', 'Internal', 'Partner', 'Other', 'Unknown'),
                                 sep="."))
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown", "E"="Embedded")
  asset <- setNames(paste('asset.assets.variety', names(assetmap), sep='.'),
                    paste('asset.variety', assetmap, sep='.'))
  value_chain <- convenience(paste('value_chain',
                                   c('Development', 'Non-distribution services', 'Targeting', 
                                     'Distribution', 'Cash-out', 'Money laundering'),
                                   sep="."))
  c(actor, action, asset, attribute, discovery_method, value_chain)  
}


counto <- function(olist) {
  cnm <- names(olist)
  found.enum <- unlist(lapply(c('action', 'actor', 'attribute'), function(x) {
    paste(x, unique(getnth(cnm[grep(paste0("^", x), cnm)])), sep=".")
  }))
  for(x in found.enum) {
    olist[[x]] <- TRUE
  }
  assetmap <- c("S "="Server", "N "="Network", "U "="User Dev", "M "="Media", 
                "P "="Person", "T "="Kiosk/Term", "Un"="Unknown")
  outs <- cnm[grep('^asset.assets.variety', cnm)]
  if (length(outs) > 0) {
    found.enum <- paste('asset.assets', assetmap[substr(getlast(outs), 1,2)], sep=".")
    for(x in found.enum) {
      olist[[x]] <- TRUE
    }
  }
  # TODO : need to add more fields for convenience things like:
  # victim.industry2
  # victim.industry3
  # victim.orgsize, (Small/Large)
  # victim.region
  # victim.subregion
  olist
}

#' Get the last element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
getlast <- function(nm) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[length(temp)]
  })
}

#' Get the last element from a column name, include action
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the last string in the name, as it is seperated by [.].  It will
#' also assume this is the action and attempt to map nice names to the name.
#' 
#' @param nm the vector of column names
getlastaction <- function(nm) {
  fixlabel <- c("malware"="[Mal]", "hacking"="[Hack]", "social"="[Soc]",
                "error"="[Err]", "physical"="[Phys]", "misuse"="[Mis]",
                "environmental"="[Env]", "unknown"="[Unk]")
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    paste(temp[length(temp)], fixlabel[temp[2]])
  })
}

#' Get the nth element from a column name
#' 
#' Givn a vector with one or more column names (veris fields), this will
#' return the nth string in the name, as it is seperated by [.].
#' 
#' @param nm the vector of column names
#' @param which the nth vlue to return
getnth <- function(nm, which=2) {
  sapply(nm, function(x) {
    temp <- unlist(strsplit(x, '[.]'))
    temp[2]
  })
}

#' Get a data.frame of counts from an enumeration
#'
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.
#' 
#' Note there are some special values that can be set as the enumeration, 
#' that may not be obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
#' * asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#' * pattern: will return the pattern the incidents are in.
#'
#' Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' 
#' @param veris a verisr object
#' @param enum the field to count
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @examples
#' \dontrun{
#' hacking <- getenum(veris, "action.hacking.variety")
#' external <- getenum(veris, "actor.external.motive")
#' }
getenum.single <- function(veris, enum, filter=NULL, add.n=T, add.freq=T) {
  if (is.null(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  
  # get names from the veris object
  cnames <- colnames(veris)
  # extract by the enumeration
  # if field name exists as is, return it, else search.
  if(any(grepl(paste0('^', enum, "$"), cnames))) {
    # yes it exists
    if(is.logical(veris[[enum]])) {
      warning(paste0("single logical field requested: ", enum, ", skipping..."))  
    } else { # not a logical field, assuming factor
      out.table <- table(veris[[enum]])
      outdf <- data.frame(enum=names(out.table), x=as.vector(out.table))
      if (add.n) outdf$n <- sum(!is.na(veris[[enum]]))
      if (add.freq) outdf$freq <- outdf$x/outdf$n
      if (is.ordered(veris[[enum]])) {
        outdf$enum <- factor(outdf$enum, levels=levels(veris[[enum]]), ordered=T)
      } else {
        outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
      }
    }
  } else {
    # only match where there are one level of enumerations 
    # after the requested enum
    if(enum=="action.variety") {
      gkey <- paste0("^action.*.variety")
      thisn <- cnames[grep(gkey, cnames)]
      ret <- setNames(colSums(veris[filter ,thisn, with=F]), getlastaction(thisn))
      # TODO add "Action.variety" into getenumby()
    } else {
      gkey <- paste0("^", enum, ".[^.]+$")
      thisn <- cnames[grep(gkey, cnames)]
      colmodes <- sapply(thisn, function(x) is.logical(veris[[x]]) | is.numeric(veris[[x]]))
      if(!all(colmodes)) {
        warning(paste0("non-numeric/non-logical values requested: ", enum, ". Perhaps the enum is incorrect?"))
        return(data.frame())
      }
      ret <- setNames(colSums(veris[filter ,thisn, with=F]), getlast(thisn))
    }
    ret <- ret[ret>0]
    outdf <- data.table(enum=names(ret), x=ret)
    #outdf <- data.frame(enum=names(ret), x=ret)
    n <- sum(rowSums(veris[filter ,thisn, with=F]) > 0)
    if (n==0) return(data.frame())
    if (add.n) outdf$n <- n
    if (add.freq) outdf$freq <- outdf$x/n
    outdf <- outdf[order(rank(x), enum)]
    if (enum %in% c('actor', 'action', 'asset.variety', 'attribute')) {
      a4names <- names(geta4names())
      n.order <- getlast(a4names[grep(paste0('^', enum), a4names)])
      outdf$enum <- factor(outdf$enum, levels=rev(n.order), ordered=T)
    } else {
      outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
    }
  }
  outdf
}

#' Extract counts from one or more enumerations
#'
#' When exploring VERIS data, you may want to get a simple count of the values within a value or enumeration.  
#' Given one or more enumerations, this will return the subsequent underlying logical values in an ordered data frame.  
#' The data frame should be formatted for use in \code{ggplot2} graphics.
#' 
#' As of version 1.1: the \code{enum} variable may be a vector of one or more enumerations.  
#' This enables any number of dimensions to be specified.  This makes the \code{primary} and \code{secondary}
#' obsolete but are still supported for the time being.
#' 
#' Note there are some special values that can be set as the enumeration, 
#' that may not be obvious. :
#' * actor, action, attribute: will all return the next level down.  For example, just querying for "action" will return "malware", "hacking", and so on.
#' * action.variety: will return the variety enumerations across all actions (e.g. top N actions) (not in getenumby() yet)
#' * asset.variety: will return the type of assets, "Server", "Network, "User Dev" and so on
#' * victim.industry2: will return a short label of industries based on 2 values of NAICS code.
#' * victim.industry3: will return a short label of industries based on 3 values of NAICS code.
#' * pattern: will return the pattern the incidents are in.
#'
#' Change in 1.1: the "add.n" and "add.freq" options are now TRUE by default.
#' #' @aliases getenumby
#' @param veris a verisr object
#' @param enum the main enumeration field 
#' @param primary the primary enumeration to filter on
#' @param secondary the (optional) secondary enumeration to filter on
#' @param filter limit what records are searched (optional)
#' @param add.n include a total count of variables found (denominator)
#' @param add.freq include a percentage (x/n)
#' @param fillzero fill in missing matches with zeros
#' @param exclusive logical value, If true, will count the unknown value only if it exclusive and it will not count the Unknown if it is selected with other attributes in the enumeration.
#' @export
#' @import data.table
#' @examples
#' \dontrun{
#' # old method:
#' a2 <- getenum(veris, "action", primary="asset.variety")
#' # new method:
#' a4 <- getenum(veris, c("action", "asset.variety", "actor", "attribute"))
#' }
getenum2 <- function(veris, enum, primary=NULL, secondary=NULL, filter=NULL, 
                      add.n=T, add.freq=T, fillzero=T, exclusive=F) {
    if (missing(filter)) {
    filter <- rep(T, nrow(veris))
  } else if (length(filter) != nrow(veris)) {
    warning(paste0("filter is not same length (", length(filter),
                   ") as object (", nrow(veris), ")."))
    return(NULL)
  }
  cnames <- colnames(veris)
  enum <- c(enum, primary, secondary)
  if (length(enum)>1 & exclusive) {
    warning("Cannot retrieve multiple enumerations and have exclusive set to TRUE, ignoring exclusive argument.")
    exclusive <- FALSE
  }
  if(any(enum %in% c("asset.assets"))) {
    # message("getenumby(): as of version 1.1, asset.assets should be replaced by asset.variety")
    enum[which(enum %in% c("asset.assets"))] <- "asset.variety"
  }
  fullkey <- paste0('^', enum, "$")
  fulln <- sapply(fullkey, function(x) any(grepl(x, cnames)))
  if (length(enum)==1 & all(fulln)) {
    if(is.logical(veris[[enum]])) {
      warning(paste0("single logical field requested: ", enum, ", skipping..."))
      return(data.frame())
    } else { # not a logical field, assuming factor
      out.table <- table(veris[[enum]])
      if(!length(out.table)) {
        return(data.frame())
      }
      outdf <- data.frame(enum=names(out.table), x=as.vector(out.table))
      if (add.n) outdf$n <- sum(!is.na(veris[[enum]]))
      if (add.freq) outdf$freq <- outdf$x/outdf$n
      if (is.ordered(veris[[enum]])) {
        outdf$enum <- factor(outdf$enum, levels=levels(veris[[enum]]), ordered=T)
      } else {
        outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
      }
    }
  } else {
    gkey <- paste0("^", enum, ".[^.]+$")
    savethisn <- thisn <- lapply(gkey, function(x) cnames[grep(x, cnames)])
    allfound <- sapply(thisn, function(x) length(x)>0)
    if(!all(allfound)) {
      warning(paste0("getenumby(): No columns matched \"", enum[!allfound], "\"", collapse="\", \""))
      return(data.frame())
    }
    
    thisn$x <- 0
    outdf <- as.data.table(expand.grid(thisn))
    #outdf <- as.data.frame(expand.grid(thisn))
    cnm <- colnames(outdf)[1:(ncol(outdf)-1)]
    # just look in first enum (exclusive) for unknowns
    myunks <- unique(unlist(sapply(c("Unknown", " - Other", "unknown"), function(p) grep(p, thisn[[1]])), use.names=F))
    for(i in seq(nrow(outdf))) {
      this.comp <- as.character(unlist(outdf[i, cnm, with = F]))
      count <- rowSums(veris[filter, this.comp, with=F]) == length(enum)
      if (exclusive && i %in% myunks) {
        count <- sum(count & rowSums(veris[filter, thisn[[1]], with=F])==1)
      } else {
        count <- sum(count)
      }
      outdf[i, (x):=count]
    }
    for(column in cnm) {
      tempcol <- getlast(as.character(unlist(outdf[ , column, with=F])))
      outdf[ , (column):=tempcol, with=F]
    }
    extra.names <- NULL
    if (length(enum)>1) extra.names <- paste0('enum', seq((length(enum)-1)))
    setnames(outdf, c('enum', extra.names, 'x'))
    n <- sum(rowSums(veris[filter ,unlist(savethisn), with=F], na.rm=T) > 0, na.rm=T)
    if (n==0) return(data.frame())
    if (!fillzero) {
      outdf <- outdf[outdf$x>0,]
    }
    if (add.n) outdf$n <- n
    if (add.freq) outdf$freq <- outdf$x/n
    # how about we put some order to the chaos
    a4names <- names(geta4names())
    for(i in seq_along(enum)) {
      if (enum[i] %in% c('actor', 'action', 'asset.variety', 'attribute')) {
        n.order <- getlast(a4names[grep(paste0('^', enum[i]), a4names)])
        this.col <- colnames(outdf)[i]
        outdf[[this.col]] <- factor(outdf[[this.col]], levels=rev(n.order), ordered=T)
      }    
    }
  }
  # name the columns... enum enum1 enum2 (?)
  # print(outdf)
  #outdf <- outdf[order(-rank(x), enum)]
  #outdf$enum <- factor(outdf$enum, levels=outdf$enum, ordered=T)
  outdf
}

#' @describeIn getenumCI getenumby A wrapper for getenumCI().  Maintained for backwards compatibility.
#' @export
getenumby <- function(...) {
  getenum(...)
}

#' @describeIn getenumCI getenum A wrapper for getenumCI().  Maintained for backwards compatibility.
#' @export
getenum <- function(...) {
  getenumCI(...)
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

#' Convenience function to pull the logical columns from verisr object
#' 
#' Given a verisr object this will return a vector of column names which are logical values.
#' 
#' @param veris the verisr object
#' @export
getlogical <- function(veris) {
  our.cols <- sapply(veris, mode)
  names(our.cols[grep('^logical$', our.cols)])
}

#' Metadata for 2-digit NAICS industry classification
#'
#' This data allows a mapping between two digit NAICS code and the 
#' full definition provided by the NAICS specification and a shorter
#' version of the title for compact visuals.
#' @name industry2
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL

#' Metadata for 3-digit NAICS industry classification
#'
#' This data allows a mapping between three digit NAICS code and the 
#' full definition provided by the NAICS specification.
#' @name industry3
#' @docType data
#' @references \url{www.census.gov/naics/}
#' @keywords data
NULL


#' sample data file to test/explore verisr
#'
#' This is a collection of the 1000 most complete incidents within the VCDB database.
#' See the example below on how the data was generated.
#' 
#' @name veris.sample
#' @docType data
#' @keywords data
#' @examples
#' \dontrun{
#' # set vcdbdir and schema file
#' 
#' vcdb <- json2veris(vcdbdir, schemafile, progressbar = T)
#' mycols <- getlogical(vcdb)
#' testdata <- vcdb[order(-rowSums(vcdb[, mycols, with=F]))]
#' got.action <- as.vector(testdata[ ,'action.Unknown', with=F] == F)
#' got.actor <- as.vector(testdata[ ,'actor.Unknown', with=F] == F)
#' got.asset <- as.vector(testdata[ ,'asset.assets.variety.Unknown', with=F] == F)
#' veris.sample <- head(testdata[got.actor & got.action & got.asset, ], 1000)
#' # cast this into a verisr object for future use.
#' class(veris.sample) <- c("verisr", "data.table", "data.frame")
#' save(veris.sample, file="data/veris.sample.rda", compress="xz")
#' }
NULL

