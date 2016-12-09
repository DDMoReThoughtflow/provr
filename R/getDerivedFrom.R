#' Create a table of "wasDerivedFrom" relationships from PROV-JSON input.
#'
#' \code{getDerivedFrom} returns a data frame containing the PROV-O "wasDerivedFrom" relationships extracted from a DDMoRe PROV-JSON document. A derivation is a transformation of an entity into another, an update of an entity resulting in a new one, or the construction of a new entity based on a pre-existing entity.
#'
#' @param z A PROV-JSON data frame.
#' 
#' @return A \code{data.frame} containing \code{derivedFrom}, \code{generatedEntity}, and \code{usedEntity} fields. 
#'
#' @seealso \url{https://www.w3.org/TR/prov-o/#wasDerivedFrom}
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
#' dir   <- getwd()
#' jlist <- loadOutput(file.path(dir, dir(dir)))
#' 
#' j1 <- lapply(jlist, function(x) {
#'   z <- getAssociatedWith(x)
#'   z
#' })
#' jDeriv       <- do.call(rbind, j1)
#' jDeriv$index <- row.names(jDeriv)
#' 
#' @export

getDerivedFrom <- function(z) {
  a <- length(names(z$bundle))     # how many repos?
  deriv     <- c()
  genEntity <- c()
  useEntity <- c()
  for(i in 1:a) {
    b <- length(names(z$bundle[[i]]$wasDerivedFrom))
    if(b > 0) {
      for (j in 1:b) {
        deriv <- c(deriv, names(z$bundle[[i]]$wasDerivedFrom)[j])
        if(length(z$bundle[[i]]$wasDerivedFrom[[j]]$`prov:generatedEntity`) > 0) {
          genEntity <- c(genEntity, z$bundle[[i]]$wasDerivedFrom[[j]]$`prov:generatedEntity`)
        } else {
          genEntity <- c(genEntity, NA)
        }
        if(length(z$bundle[[i]]$wasDerivedFrom[[j]]$`prov:usedEntity`) > 0) {
          useEntity <- c(useEntity, z$bundle[[i]]$wasDerivedFrom[[j]]$`prov:usedEntity`)
        } else {
          useEntity <- c(useEntity, NA)
        }
      }
    }
  }
  data.frame(derivedFrom     = deriv,
             generatedEntity = as.character(genEntity),
             usedEntity      = as.character(useEntity))
}

