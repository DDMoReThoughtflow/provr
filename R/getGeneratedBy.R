#' Create a table of "wasGeneratedBy" relationships from PROV-JSON input.
#'
#' \code{getGeneratedBy} returns a data frame containing the PROV-O "wasGeneratedBy" relationships extracted from a DDMoRe PROV-JSON document. Generation is the completion of production of a new entity by an activity. This entity did not exist before generation and becomes available for usage after this generation.
#'
#' @param z A PROV-JSON data frame.
#' 
#' @return A \code{data.frame} containing \code{gen}, \code{activity}, and \code{entity} fields. 
#'
#' @seealso \url{https://www.w3.org/TR/prov-o/#wasGeneratedBy}
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
#'   z <- getGeneratedBy(x)
#'   z
#' })
#' jGen       <- do.call(rbind, j1)
#' jGen$index <- row.names(jGen)
#'
#' @export

getGeneratedBy <- function(z) {
  a <- length(names(z$bundle))     # how many repos?
  gen      <- c()
  activity <- c()
  entity   <- c()
  for(i in 1:a) {
    b <- length(names(z$bundle[[i]]$wasGeneratedBy))
    if(b > 0) {
      for (j in 1:b) {
        gen <- c(gen, names(z$bundle[[i]]$wasGeneratedBy)[j])
        if(length(z$bundle[[i]]$wasGeneratedBy[[j]]$`prov:activity`) > 0) {
          activity <- c(activity, z$bundle[[i]]$wasGeneratedBy[[j]]$`prov:activity`)
        } else {
          activity <- c(activity, NA)
        }
        if(length(z$bundle[[i]]$wasGeneratedBy[[j]]$`prov:entity`) > 0) {
          entity <- c(entity, z$bundle[[i]]$wasGeneratedBy[[j]]$`prov:entity`)
        } else {
          entity <- c(entity, NA)
        }
      }
    }
  }
  data.frame(gen = gen,
             activity = as.character(activity),
             entity = as.character(entity))
}

