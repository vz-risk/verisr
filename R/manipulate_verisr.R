#' flatten a verisr dataframe such that each row represents a single plus.master_id
#' @param veris A verisr object
#' @return a verisr object with each plus.master_id represented by a single row
#' @export
flatten_verisr <- function(veris) {
  # no duplicates so return input verisr object
  if (length(veris[["plus.master_id"]]) == length(unique(veris[["plus.master_id"]]))) {return(veris)}
  
  # first separate duplicate from  non-duplicate master_ids
  rows_per_master_id <- table(veris$plus.master_id) # this is slow
  
  ret <- veris[veris[["plus.master_id"]] %in% names(rows_per_master_id[rows_per_master_id > 1]), ]
  ret <- dplyr::group_by(ret, plus.master_id)
  ret <- dplyr::summarize(ret,
                          dplyr::across(where(is.logical), any, na.rm=TRUE),
                          dplyr::across(where(is.numeric), sum, na.rm=TRUE),
                          dplyr::across(where(is.character), function(c) {
                            unique_c <- unique(c)
                            if (length(unique_c) > 1) {warning(paste0("First of ", paste(unique_c, collapse=","), " kept during flattening."))}
                            return(unique_c[1])
                          }),
                          dplyr::across(where(is.factor), function(c) {
                            c <- as.character(c) # convert to character and treat as normal from there. - catch for patterns as factor.
                            unique_c <- unique(c)
                            if (length(unique_c) > 1) {warning(paste0("First of ", paste(unique_c, collapse=","), " kept during flattening."))}
                            return(unique_c[1])
                          })
  )
  
  
  ret <- dplyr::bind_rows( # using dplyr here to try and 
    veris[veris[["plus.master_id"]] %in% names(rows_per_master_id[rows_per_master_id == 1]), ], # rows that don't need to be changed
    ret
  )
  
  
  return(ret)
}