#' Create a table of "wasAssociatedWith" relationships from PROV-JSON input.
#'
#' \code{getAssociatedWith} returns a data frame containing the PROV-O "associatedWith" relationships extracted from a DDMoRe PROV-JSON document. An activity association is an assignment of responsibility to an agent for an activity, indicating that the agent had a role in the activity. It further allows for a plan to be specified, which is the plan intended by the agent to achieve some goals in the context of this activity.
#'
#' @param z A PROV-JSON data frame.
#' 
#' @return A \code{data.frame} containing \code{associatedWith}, \code{activity}, and \code{plan} fields. 
#'
#' @seealso \url{https://www.w3.org/TR/prov-o/#wasAssociatedWith}
#'
#' @examples
#' ### Reads in a sequence of JSON files from dir
#' loadOutput <- function(files) {
#' jList <- list()
#' for(f in files) {
#'   jList[[basename(f)]] <- fromJSON(txt = f)
#' }
#' jList
#' }
#' 
#' dir   <- "/data/jsonDocs"
#' jlist <- loadOutput(file.path(dir, dir(dir)))
#' 
#' j1 <- lapply(jlist, function(x) {
#'   z <- getAssociatedWith(x)
#'   z
#' })
#' jAss       <- do.call(rbind, j1)
#' jAss$index <- row.names(jAss)
#'
#' @export

getAssociatedWith <- function(z) {
  a <- length(names(z$bundle))     # how many repos?
  assoc    <- c()
  activity <- c()
  plan     <- c()
  for(i in 1:a) {
    b <- length(names(z$bundle[[i]]$wasAssociatedWith))
    if(b > 0) {
      for (j in 1:b) {
        assoc <- c(assoc, names(z$bundle[[i]]$wasAssociatedWith)[j])
        if(length(z$bundle[[i]]$wasAssociatedWith[[j]]$`prov:activity`) > 0) {
          activity <- c(activity, z$bundle[[i]]$wasAssociatedWith[[j]]$`prov:activity`)
        } else {
          activity <- c(activity, NA)
        }
        if(length(z$bundle[[i]]$wasAssociatedWith[[j]]$`prov:plan`) > 0) {
          plan <- c(plan, z$bundle[[i]]$wasAssociatedWith[[j]]$`prov:plan`)
        } else {
          plan <- c(plan, NA)
        }
      }
    }
  }
  data.frame(associatedWith = assoc,
             activity = as.character(activity),
             plan = as.character(plan))
}

