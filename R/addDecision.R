#' Creates a "decision" object in XML.
#'
#' \code{addDecision} creates an "decision" object in XML.
#'
#' @param decision A description of the decision, given as a string.
#' @param entity A list of entities upon which the decision was based.
#' @param file The filename to write the decision object to. If blank, the XML will be written to the console.
#'        
#' @return A structured XML object containing the decision 
#'
#' @examples
#'
#' addDecision(decision="Final model",
#'   entity=list("/models/Step7/GOF_MLX_1.png",
#'      "/models/Step7/GOF_MLX_2.png",
#'      "/models/Step7/GOF_MLX_3.png",
#'      "/models/Step7/Step7.SO.xml"),
#'   file="dec1.xml")
#'
#' @import XML 
#' @export
#' 
addDecision <- function(decision, entity=c(), file="") {
  
  if(decision=="") {
    stop("Description of the decision cannot be empty!\n")
  }
  
  xmlOut        <- xmlOutputDOM(tag = "Decision")
  xmlOut$addTag("DecisionBody", decision)  
  
  for(i in 1:length(entity)) {
    xmlOut$addTag("Entity", entity[i])
  }
  
  if(file=="") { 
    xmlOut$value()
  } else {
    saveXML(xmlOut$value(), file=file)
  }
}

