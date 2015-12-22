#' Internal function to grab the average of given fields.
#' 
#' grabs an average from a matrix of enumerations
#' 
#' @param mat verisr matrix object
#' @param fields character vector of colnames in mat
enum.avg <- function(mat, fields) {
  present <- mapply(fields, FUN=function(x) {
    smat <- mat[ ,grep(x, colnames(mat))]
    if (is.matrix(smat)) {
      if(ncol(smat)>0) {
        smat <- apply(smat, 1, max)
      } else {
        smat <- rep(0, nrow(mat))
      }
    }
    smat
  })
  if (! is.vector(present)) {
    present <- rowMeans(present)
  }
  present
}

#' Creates a matrix of measurements about various fields within a veris matrix object.  
#' will return a matrix of the same number of rows, but the columns are various measurements 
#' around the completeness, context and complexity (CCC) of the incidents.
#'
#'
#' @param mat a verisr oject, should have removed unknown/other fields
#' @export
getccc <- function(mat) {
  fields <- list(ccc.actor.external=c("actor.external.variety", "actor.external.motive"),
                 ccc.actor.internal=c("actor.internal.variety", "actor.internal.motive"),
                 ccc.actor.partner=c("actor.partner.motive", "actor.partner.industry",
                                     "actor.partner.country"))
  actors <- mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=mat))
  ccc <- cbind(actors, "ccc.actor"=apply(actors, 1, max))
  fields <- list(ccc.action.malware=c("action.malware.variety", "action.malware.vector"),
                 ccc.action.hacking=c("action.hacking.variety", "action.hacking.vector"),
                 ccc.action.social=c("action.social.variety", "action.social.vector", "action.social.target"),
                 ccc.action.misuse=c("action.misuse.variety", "action.misuse.vector"),
                 ccc.action.physical=c("action.physical.variety", "action.physical.vector", "action.physical.location"),
                 ccc.action.error=c("action.error.variety", "action.error.vector"),
                 ccc.action.environmental=c("action.environmental.variety", "action.environmental.variety"))
  actions <- mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=mat))
  ccc <- cbind(ccc, actions, "ccc.action"=apply(actions, 1, max))
  ccc <- cbind(ccc, "ccc.asset"=enum.avg(mat, "asset.assets.variety"))
  ccc <- cbind(ccc, ccc.complete=rowMeans(ccc[ ,c('ccc.actor', 'ccc.action', 'ccc.asset')]))
  # this isn't used in a calc, I just wanted it available in the matrix.
  fields <- list(ccc.victim.employee_count = "victim.employee_count", 
                 ccc.victm.victim.industry = "victim.industry")
  ccc <- cbind(ccc, mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=mat)))
  
  fields <- list(ccc.victim=c("victim.employee_count", "victim.industry",
                       "victim.country"),
                 ccc.timeline=c("timeline.compromise.unit",
                            "timeline.exfiltration.unit",
                            "timeline.containment.unit",
                            "timeline.discovery.unit"),
                 ccc.dicovery_method=c("discovery_method"),
                 ccc.targeted=c("targeted"))
  context <- mapply(fields, FUN=enum.avg, MoreArgs = list("mat"=mat))
  ccc <- cbind(ccc, context, ccc.context=rowMeans(context))
  ccc <- cbind(ccc, ccc.complexity=rowSums(mat))
  ccc
}

