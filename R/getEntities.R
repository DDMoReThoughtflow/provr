#' Create a table of entities from PROV-JSON input.
#'
#' \code{getEntities} returns a data frame containing a list of entities and pertinent
#' provenance information, extracted from a DDMoRe PROV-JSON document.
#'
#' @param z A PROV-JSON data frame.
#' @param filter A string defining a filter for the entity dataset.
#' @param type The type of entity to return. Valid types are \code{all}, \code{dataset}, \code{document}, \code{plan}, \code{model} and \code{output}. 
#' 
#' @return A \code{data.frame} containing \code{entity}, \code{location}, \code{time},
#' \code{owningBundle}, and \code{type} fields. 
#'
#' @seealso \url{https://www.w3.org/TR/prov-o/#Entity}
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
#'   z <- getEntities(x)
#'   z
#' })
#' jEnt       <- do.call(rbind, j1)
#' jEnt$index <- row.names(jEnt)
#' @export

getEntities <- function(z, entityFilter="", entityType="all") {
  
  if(!(entityType %in% c("all","document","dataset","plan","model","output"))) {
    stop("Please specify a valid type of entity (all, dataset, document, plan, model, or output).")
    
  }
  a <- length(names(z$bundle))     # how many repos?
  entities <- c()
  times    <- c()
  type     <- c()
  location <- c()
  bundle   <- c()
  label    <- c()
  for(i in 1:a) {
    b <- length(names(z$bundle[[i]]$entity))
    if(b > 0) {
      for (j in 1:b) {
        entities <- c(entities, names(z$bundle[[i]]$entity)[j])
        
        if(length(z$bundle[[i]]$entity[[j]]$`prov:time`) == 1) {
          times <- c(times, z$bundle[[i]]$entity[[j]]$`prov:time`)
        } 
        if(length(z$bundle[[i]]$entity[[j]]$`prov:time`) == 2) {
          times <- c(times, z$bundle[[i]]$entity[[j]]$`prov:time`$`$`)
        } 
        if(!length(z$bundle[[i]]$entity[[j]]$`prov:time`) %in% c(1,2)) {
          times <- c(times, NA)
        }
        if(length(z$bundle[[i]]$entity[[j]]$`prov:type`) > 0) {
          type <- c(type, z$bundle[[i]]$entity[[j]]$`prov:type`)
        } else {
          type <- c(type, NA)
        }
        if(length(z$bundle[[i]]$entity[[j]]$owningBundle) > 0) {
          bundle <- c(bundle, z$bundle[[i]]$entity[[j]]$owningBundle)
        } else {
          bundle <- c(bundle, NA)
        }
        if(length(z$bundle[[i]]$entity[[j]]$`prov:location`) > 0) {
          location <- c(location, z$bundle[[i]]$entity[[j]]$`prov:location`)
        } else {
          location <- c(location, NA)
        }
        if(length(z$bundle[[i]]$entity[[j]]$`prov:label`) > 0) {
          label <- c(label, z$bundle[[i]]$entity[[j]]$`prov:label`)
        } else {
          label <- c(label, NA)
        }
      }
    }
  }
  a <- data.frame(entity = as.character(entities),
             location = as.character(location),
             label = as.character(label),
             time = as.character(times),
             owningBundle = as.character(bundle),
             type = as.character(type))
  
  if(entityFilter!="") {
    a <- a[grep(entityFilter, a$location),]
  }
  a <- subset(a, !is.na(type))
  if(entityType == "dataset") {
    a <- subset(a, type=="ddmore:Dataset")
  }
  if(entityType == "document") {
    a <- subset(a, type=="document")
  }  
  if(entityType == "plan") {
    a <- subset(a, type=="prov:Plan")
  }
  if(entityType == "model") {
    a <- subset(a, type=="ddmore:Model")
  }
  if(entityType == "output") {
    a <- subset(a, type=="ddmore:Output")
  }
  a
}

