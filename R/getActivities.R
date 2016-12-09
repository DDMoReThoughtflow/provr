#' Create a table of activities from PROV-JSON input.
#'
#' \code{getActivities} returns a data frame containing a list of activities and pertinent
#' provenance information, extracted from a DDMoRe PROV-JSON document.
#'
#' @param z A PROV-JSON data frame.
#' 
#' @return A \code{data.frame} containing \code{activity}, \code{startTime}, \code{description}, 
#' and \code{displayName} fields. 
#'
#' @seealso \url{https://www.w3.org/TR/prov-o/#Activity}
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
#'   z <- getActivities(x)
#'   z
#' })
#' jAct       <- do.call(rbind, j1)
#' jAct$index <- row.names(jAct)
#' @export


getActivities <- function(z) {
  a <- length(names(z$bundle))     # how many bundles?
  names <- c()
  time  <- c()
  desc  <- c()
  display <- c()
  note <- c()
  for(i in 1:a) {
    b <- length(names(z$bundle[[i]]$activity))
    if(b > 0) {
      for (j in 1:b) {
        names <- c(names, names(z$bundle[[i]]$activity)[j])
        if(length(z$bundle[[i]]$activity[[j]]$`prov:startTime`) > 0) {
          time <- c(time, z$bundle[[i]]$activity[[j]]$`prov:startTime`)
        } else {
          time <- c(time, NA)
        }
        if(length(z$bundle[[i]]$activity[[j]]$`note`) > 0) {
          note <- c(note, z$bundle[[i]]$activity[[j]]$`note`)
        } else {
          note <- c(note, NA)
        }
        if(length(z$bundle[[i]]$activity[[j]]$`ddmore:description`) > 0) {
          desc <- c(desc, z$bundle[[i]]$activity[[j]]$`ddmore:description`)
        } else {
          if(length(z$bundle[[i]]$activity[[j]]$`description`) > 0) {
            desc <- c(desc, z$bundle[[i]]$activity[[j]]$`description`)
          } else {
            desc <- c(desc, NA)
          }
        }
        if(length(z$bundle[[i]]$activity[[j]]$`ddmore:displayName`) > 0) {
          display <- c(display, z$bundle[[i]]$activity[[j]]$`ddmore:displayName`)
        } else {
          if(length(z$bundle[[i]]$activity[[j]]$`prov:label`) > 0) {
            display <- c(display, z$bundle[[i]]$activity[[j]]$`prov:label`)
          } else {
            if(length(z$bundle[[i]]$activity[[j]]$`displayName`) > 0) {
              display <- c(display, z$bundle[[i]]$activity[[j]]$`displayName`)
            } else {
              display <- c(display, NA)
            }
          }
        }
      }
    }
  }
  data.frame(activity = as.character(names),
             startTime = as.character(time),
             description = as.character(desc),
             displayName = as.character(display),
             note = as.character(note))
}

