#' Helper function to retrieve prototypes from skmeans models for add_patterns
#' 
#' @param breach A skmeans model of breach centroids. (Should)
#' @return A matrix of of skmeans centroids with one row per centroid
models_to_centroids <- function() {
  #data("models", envir=environment())
  load(system.file("data", "2021_pattern_models.rda", package="verisr"), verbose=FALSE)
  
  non_breach_proto <- models$non_breach$prototypes
  breach_proto <- models$breach$prototypes
  rownames(non_breach_proto) <- paste0("cluster.I", rownames(non_breach_proto))
  rownames(breach_proto) <- paste0("cluster.B", rownames(breach_proto))
  
  rm(models)
  
  return(rbind(non_breach_proto, breach_proto))
}


#' Groom data for patterns from VERIS 1.3.4 to 1.3.5
#' 
#' Because VERIS changes, columns come and go.  This makes the clustering
#' function unhappy.  We need to make sure the data we're clustering matches
#' the matrix we're using to cluster first.  
#' 
#' @param veris veris object to cluster
#' @param centroids skmeans cluster prototypes
#' @return veris, centroids
#' @export
pattern_1.3.4_to_1.3.5 <- function(veris, centroids) {
  ### Because VERIS changes, the columns used to cluster may change.
  ### We can map renamed enumerations from VERIS to their new name using this mapping
  veris_name_map <- readr::read_csv("from,to
    action.malware.vector.Web download,action.malware.vector.Web application - download
    action.malware.vector.Web drive-by,action.malware.vector.Web application - drive-by")
  ### For columns that are combined during a VERIS update  (i.e. 2 columns in 
  ###  centroids are now 1 in veris) we will duplicate the column in VERIS under
  ###  the old name to allow it to be used during clustering.
  veris_col_dup <- readr::read_csv("from,to
    action.physical.vector.Victim work area,action.physical.vector.Visitor privileges
    action.physical.vector.Public facility,action.physical.vector.Uncontrolled location")
  ### It's possible for columns to be removed from VERIS.  Those must be removed
  ### from the the cluster
  veris_name_rem <- c("discovery_method.external.variety.Monitoring service")
  ### New columns added to veris cannot be added without re-clustering.
  
  ### Apply the rules to account for VERIS updates
  ### Handle joined columns
  for (i in 1:nrow(veris_col_dup)) {
    veris[[veris_col_dup[i, ][["to"]]]] <- veris[[veris_col_dup[i, ][["from"]]]]
  }
  ### Update columns
  colnames(centroids) <- plyr::mapvalues(colnames(centroids), veris_name_map$from, veris_name_map$to)
  ### Remove obsolete columns
  centroids <- centroids[, setdiff(colnames(centroids), veris_name_rem)]
  
  return(list("veris"=veris, "centroids"=centroids))
}


#' A function to add patterns to a verisr dataframe
#' 
#' This function works by scoring the incidents according to the skmeans clusters.  Not, it can be rather slow on large data sets.
#'
#' @param veris  A verisr data.table
#' @param centroids  A matrix of of skmeans centroids with one row per centroid. If null, (the default), the 2021 DBIR pattern centroids will be used.
#' @param prefix  The predicate of the column name to use for the patterns
#' @param replace  Whether to remove previously existing columns with the same predicate before adding the patterns
#' @param clusters  If TRUE, will add the clusters to the returned veris object as 'cluster.X' with a value of the cosign distance to the cluster
#' @param threshold The ratio of the difference of cluster-to-incident distances and the smallest cluster-to-incident distance. Defaults to 1/10th (i.e. the difference must be 1/10th the distance to the incident. This results in two percent of clusters kept in 2020 data)
#' @param veris_update_f A function to apply to centoids and veris to handle updates to veris after the clusters are defined.  It must take a veris object and a centroid and return a list of a veris object and centroid.  Because veris adds, removes, and changes enumerations each year, this function modifies the data and centroids, (currently based on veris 1.3.5) to be compatible with the current version of VERIS.
#' @return a data.table object with the columns added
#' @export
add_patterns <- function(veris, 
                         centroids=NULL, 
                         prefix="pattern", 
                         replace=TRUE, 
                         clusters=FALSE, 
                         threshold=0.1,
                         veris_update_f = NULL) {
  
  
  if (is.null(centroids)) {
    centroids <- verisr:::models_to_centroids()
  }
  
  ### 2021 V6
  cluster_map <- readr::read_csv(
    "B1,System Intrusion
    B2,Lost and Stolen Assets
    B3,Social Engineering
    B4,Basic Web Application Attacks
    B5,Privilege Misuse
    B6,Miscellaneous Errors
    B7,Miscellaneous Errors
    B8,System Intrusion
    I1,Denial of Service
    I2,Miscellaneous Errors
    I3,Lost and Stolen Assets
    I4,System Intrusion
    I5,Privilege Misuse
    I6,Basic Web Application Attacks
    I7,System Intrusion
    I8,Privilege Misuse
    I9,System Intrusion
    I10,Social Engineering", 
    comment = "#",
    quote = "'",
    col_names = c("cluster", "pattern")
  )
  cluster_map$cluster <- paste0("cluster.", cluster_map$cluster)
  cluster_map$pattern <- paste0(prefix, ".", cluster_map$pattern)
  
  #glimpse(cutoffs)
  #glimpse(cluster_map)
  
  ### This applies any veris updates necessary to match the veris version of the
  ###  data to the veris version of the clusters.
  if (!is.null(veris_update_f)) {
    veris_update <- veris_update_f(veris, centroids)
    veris <- veris_update[[1]]
    centroids <- veris_update[[2]]
  }
  ### As a last resort, we remove columns not in the veris dataframe, but
  ###  we generate a warning first
  if (length(setdiff(colnames(centroids), names(veris))) > 0) {
    warning(paste0("There are columns in the clusters not in the data: (", 
            paste(setdiff(colnames(centroids), names(veris)), collapse = ", "),
            "). Removing them from clusters so that clustering can proceed, ",
            "but you should check your data and if these are significant ",
            "columns, you shoudl apply a veris update function with ",
            "veris_update_f to properly handle them."))
    centroids <- centroids[ , intersect(names(veris), colnames(centroids))]
  }
  
  
  ## get columns to use for clustering
  #breach_cols_to_cluster <- colnames(models$breach$prototypes)
  #non_breach_cols_to_cluster <- colnames(models$non_breach$prototypes)
  cols_to_cluster <- colnames(centroids)
  
  
  ###  Predict clusters
  
  chunk <- suppressWarnings(verisr::flatten_verisr(veris)) # unfortunately slow
  
  ## save master_ids
  master_ids <- chunk$plus.master_id
  
  ## convert verisr to matrix
  chunk <- chunk[, ..cols_to_cluster]
  col_names <- names(chunk)
  chunk <- as.matrix(chunk)
  colnames(chunk) <- col_names
  rownames(chunk) <- master_ids
  ## It's possible for columns that are not in the schema to be in both chunk and the centroids
  ## In this case, the column will contain 'NAs' where the value wasn't specified
  ## If we leave them, they will cause clustering issues.
  ## Implicitly those should all be FALSE so we fill that in here. - GDB 210723
  chunk[is.na(chunk)] <- FALSE
  
  ## calculate cosign distance. (Small is better. Took me a while to figure that out)
  all_pred <- skmeans::skmeans_xdist(
    x=chunk[rowSums(chunk, na.rm=TRUE) > 0, ], 
    y=centroids)
  
  ### create a single dataframe of cluster distances
  all_pred <- tibble::as_tibble(all_pred, rownames="plus.master_id")
  all_pred <- tidyr::pivot_longer(all_pred, -plus.master_id, names_to="cluster", values_to="value")
  
  
  ### Join patterns with dataset
    # map the clusters to patterns
  patterns <- dplyr::left_join(all_pred, cluster_map, by=c("cluster" = "cluster")) 
    # remove unmapped clusters (before scoring so unmapped clusters don't disadvantage other clusters)
    # the other clusters already have a max distance cutoff
  patterns <- dplyr::filter(patterns, pattern != paste0(prefix, ".NA")) 
  # DDoS is the only incident that can be included below the quality threshold. Because of that, it's picking up low-quality 
  #   Incidents that should be in other clusters.  Here we remove DDoS w/o DDoS so and let them go into the next likely pattern
  patterns <- dplyr::left_join(patterns, 
                               # Because 'veris' isn't flattened, we use chunk for the DoS columns.  However chunk is a matrix and
                               #    lacks 'plus.master_id', so we convert to a tibble and add plus.master_id.
                               # Little worried about performance, but chunk should be far smaller due to fewer columns and all but 1 are logical.
                               # GDB 210723
                               dplyr::mutate(
                                 tibble::as_tibble(chunk[ , c("action.hacking.variety.DoS", "action.malware.variety.DoS")]),
                                 `plus.master_id` = rownames(chunk)
                               ),
                               by=c("plus.master_id"="plus.master_id")
              )
  patterns <- dplyr::mutate(patterns, value = ifelse(
    pattern == paste0(prefix, ".", "Denial of Service") & !(action.hacking.variety.DoS | action.malware.variety.DoS), # if it's DoS w/o DOS
    1, # Make DoS the longest possible distance
    value)) # otherwise leave it alone
  patterns <- dplyr::select(patterns, -action.hacking.variety.DoS, -action.malware.variety.DoS)
  # 'cut' ends up being the TRUE/FALSE for the pattern. The closest always is in as distance is 0, but others below the threshold can also make it
  # first check that the value is above the minimum distance from the centroid for the given centroid
  # this is determined by looking at the distribution of distances where the centroid is the top assignment
  # if we don't do this, everything will get a pattern since it'll have a closest centroid no matter how far away
  #        dplyr::left_join(cutoffs, by=c("cluster" = "cluster")) %>%
  #        dplyr::mutate(cut = value <= cutoff) %>%
  # We look for centroids similar distance to the top centroid for each incident
  # This ensures that if an incident is equally close to two centroids it gets counted in both clusters
  # We compare the difference in distances between the clusters and incident and minimum-distance cluster to the incident.
  # We keep the incident assignment if the distance is below the reshold. (10% results in 2% of clusters kept in addition to the closest cluser in 2020 data)
  patterns <- dplyr::group_by(patterns, plus.master_id) 
  patterns <- dplyr::mutate(patterns, cut = (value - min(value))/min(value) <= threshold) 
  patterns <- dplyr::ungroup(patterns)
    #dplyr::glimpse() %>% # DEBUG
  patterns <- dplyr::select(patterns, plus.master_id, pattern, cut) 
  patterns <- dplyr::arrange(patterns, dplyr::desc(cut)) # arrange true first so it comes before false in distinct
  patterns <- dplyr::distinct(patterns, plus.master_id, pattern, .keep_all = TRUE)
  patterns <- tidyr::pivot_wider(patterns, names_from=pattern, values_from=cut, values_fill=FALSE)
  
  
  ### If replace, remove the existing prefix columns
  if (replace) {
    veris <- veris[, !grepl(paste0("^", prefix), names(veris)), with = FALSE]
  }
  
  
  ### Join the patterns to the veris dataframe
  ## (Where the magic happens)
  ## Done by plus.master_id to support future veris where one line is not necessarily one incident
  veris <- dplyr::left_join(
    veris,
    patterns,
    by=c("plus.master_id"="plus.master_id")
  )
  
  
  ### Need to remove "NA's" before the sanity checks
  na_replace = function(v,value=FALSE) { v[is.na(v)] = value; v }
  veris <- veris[, 
        c(grep(paste0("^", prefix), names(veris), value=TRUE)) := lapply(.SD, na_replace), 
        .SDcols= grep(paste0("^", prefix), names(veris), value=TRUE)]
  
  ### Sanity checks
  ## Ensure the core tenant of the pattern is met
  veris <- veris[
    veris[[paste0(prefix, ".Privilege Misuse")]]  & !veris[["action.Misuse"]], # misuse without misuse
    c(paste0(prefix, ".Privilege Misuse")) := FALSE]
  veris <- veris[
    veris[[paste0(prefix, ".Miscellaneous Errors")]]  & !veris[["action.Error"]], # error without error
    paste0(prefix, ".Miscellaneous Errors") := FALSE]
  veris <- veris[
    veris[[paste0(prefix, ".Social Engineering")]]  & !veris[["action.Social"]], # phishing without Social action
    paste0(prefix, ".Social Engineering") := FALSE]
  veris <- veris[
    veris[[paste0(prefix, ".System Intrusion")]]  & !(veris[["action.Hacking"]] | veris[["action.Malware"]]), # Intrusion without hacking/malware
    paste0(prefix, ".System Intrusion") := FALSE]
  #veris[
  #  veris[[paste0(prefix, ".Crimeware")]]  & !veris$action.Malware, # Crimeware without malware
  #  paste0(prefix, ".Crimeware")] <- FALSE
  veris <- veris[
    veris[[paste0(prefix, ".Basic Web Application Attacks")]]  & !(veris[["`asset.assets.variety.S - Web application"]] | 
                                                       veris[["action.hacking.vector.Web application"]] |
                                                       veris[["action.malware.vector.Web download"]] |   
                                                       veris[["action.malware.vector.Web drive-by"]]  |
                                                       veris[["action.social.vector.Website"]]), # webapp without web application
    paste0(prefix, ".Basic Web Application Attacks") := FALSE]
  veris <- veris[
    veris[[paste0(prefix, ".Lost and Stolen Assets")]]  & !(veris[["action.Physical"]] | veris[["action.error.variety.Loss"]]), # loss w/o physical or loss
    paste0(prefix, ".Lost and Stolen Assets") := FALSE]
### NOTE:   Commenting out the  Denial of service sanity check:
###         This is forced above, but in such a way that the incident can be reclassified rather than being classified 'EE'
###         The reason is the training data used Quality incidents where DoS can bypass the quality filter, drawing non-DoS low quality incidents
###         to it.  To remedy that, we allow re-assignment of Non-DOS DoS-cendroid incidents to the next closest cluster.
#  veris[
#    veris[[paste0(prefix, ".Denial of Service")]]  & !(veris$action.hacking.variety.DoS | veris$action.malware.variety.DoS), # DoS w/o DoS
#    paste0(prefix, ".Denial of Service")] <- FALSE
  
  
  # If including clusters, join the cluster distances
  if (clusters) {
    all_clust <- dplyr::select(all_pred, plus.master_id, cluster, value)
    all_clust <- tidyr::pivot_wider(all_clust, names_from=cluster, values_from=value, values_fill=NA)
    veris <- dplyr::left_join(
      veris,
      all_clust,
      by=c("plus.master_id"="plus.master_id")
    )
  }
  
  # code anything uncoded as everything else
  veris[[paste0(prefix, ".Everything Else")]] <- !unlist(apply(veris[, grep(paste0("^", prefix), names(veris)), with = FALSE], MARGIN=1, any, na.rm=TRUE))
  
  return(veris)
}