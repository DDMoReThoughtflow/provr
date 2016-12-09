#' Creates an "assumption" object in XML.
#'
#' \code{addAssumption} creates an "assumption" object in XML. Marshall and colleagues define a range of so-called MID3 assumptions used in modeling \& simulation activities, which has been used as the basis for our approach.
#' 
#' Marshall SF, Burghaus R, Cosson V, Cheung S, Chenel M, DellaPasqua O, et al (2016). Good Practices in Model‐Informed Drug Discovery and Development: Practice, Application, and Documentation. \emph{CPT Pharmacometrics Syst Pharmacol} \strong{5}(3):93–122. 
#'
#' @param type The type of assumption. Options are \code{pharm} (pharmacological), \code{phys} (physiological),
#'             \code{dis} (disease), \code{data} (data), and \code{math} (mathematical/statistical).
#' @param body A description of the assumption, given as a string.
#' @param justif A motivation explaining why the assumption was made, given as a string.
#' @param estab Has the assumption been established outside of the current analysis? Can be \code{TRUE} ("established") or \code{FALSE} ("new"). 
#' @param testable Is the assumption testable? Can be \code{TRUE} or \code{FALSE}. 
#' @param testtext An evaluation of whether or not the assumption can be tested, with or without explanatory detail, given as a string. If \code{testable} is \code{FALSE}, defaults to "Not testable", but is required if \code{testable} is \code{TRUE}.
#' @param testapp A description of the approach for testing the assumption, if \code{testable} is \code{TRUE}, given as a string. Can be empty if \code{testable} is \code{FALSE}.
#' @param testout A description of the outcome of assumption testing, given as a string. 
#' @param file The filename to write the assumption object to. If blank, the XML will be written to the console.
#'        
#' @return A structured XML object containing the assumption. 
#'
#' @seealso \url{http://onlinelibrary.wiley.com/doi/10.1002/psp4.12049/abstract}
#'
#' @examples
#'
#' ## Examples from Table 4 in Marshall et al.
#' 
#' addAssumption(type = "pharm", 
#'   body = "Emax model fixed to 100% is a more physiological description of the data compared to a linear model.",
#'   justif = "Emax model is not better than linear model; however, for this drug class, Emax of 100% is more realistic.",
#'   estab = F,
#'   testable = T,
#'   testtext = "Testable with a wider range ofconcentrations (external/future study).",
#'   testapp = "Comparison of simulated metrics of interest between the two competing models.",
#'   testout = "To achieve a 90% response (assumed to be clinically meaningful) requires a two-fold higher dose using the Emax model compared to the linear model -> Test doses suggested by Emax model in Phase 2.",
#'   file="ass1.xml")
#'
#' addAssumption(type = "phys", 
#'   body = "No difference in clearance between healthy subjects and patients.",
#'   justif = "Patients with major depression disorders are considered as healthy subjects (in regard of ADME/PK features) once age and weight are taken into account.",
#'   estab = T,
#'   testable = T,
#'   testtext = "Testable by pooling healthy subjects and patient data, assuming that all other qualities across the pooled trials are exchangeable.",
#'   testapp = "Combined analysis with healthy subjects and patients.",
#'   testout = "Combined analysis found only a 10% lower clearance in patients. -> No dose adjustment necessary for PK reasons.")
#'   
#' addAssumption(type = "dis", 
#'   body = "Linear progression of disease with a slope of X/year.",
#'   justif = "Cannot be estimated directly from the dataset, but supported by literature review.",
#'   estab = T,
#'   testable = F,
#'   testtext = "Not testable with the current dataset.",
#'   testapp = "Sensitivity analysis changing the value of the slope for disease progression from X to Y.",
#'   testout = "Varying the slope by between X and Y will not change the selected dose for Phase 3 -> Selected dose for Phase 3 can be implemented.")
#'   
#' addAssumption(type = "data", 
#'   body = "Data BLQ have no impact on analysis results.",
#'   justif = "There are less than 20% of concentrations after treatment BLQ.",
#'   estab = F,
#'   testable = T,
#'   testtext = "Testable.",
#'   testapp = "Run final model with BLQ using M3 method (Beal) and compare to model without BLQ.",
#'   testout = "Negligible changes in parameter estimates -> Final model excluding BLQ observations selected.")
#'   
#' addAssumption(type = "math", 
#'   body = "Similar variability in clearance between adults and children.",
#'   justif = "Physiological and PK knowledge.",
#'   estab = F,
#'   testable = F,
#'   testtext = "Not testable at the stage of predictions but can be evaluated with data from children.",
#'   testapp = "Sensitivity analysis on the variance value of clearance.",
#'   testout = "If variance is twofold, children would still be within the safety range established for adults -> Suggested dosing can be used in children.")  
#'   
#' @export

addAssumption <- function(type="pharm", body, justif, estab=TRUE, testable=TRUE, testtext,
                          testapp, testout, file="") {
  
  if(!(type %in% c("pharm","phys","dis","data","math"))) {
    stop("Assumptions must be of type pharmacological (pharm), physiological (phys), disease (dis), data (data), or mathematical/statistical (math).\n")
  }
  
  if(body=="") {
    stop("Description of the assumption cannot be empty!\n")
  }
  
  estabText <- "established"
  if(estab==F) estabText <- "new"
  
  if(testable & testtext=="") {
    stop("Please provide detail on the testability of this assumption.\n")
  } 
  
  if(!testable & testtext=="") {
    testtext <- "Not testable"
  } 
  
  if(testable & testapp=="") {
    stop("Please provide a description of how to test this assumption.\n")
  } 
  
  xmlOut        <- xmlOutputDOM(tag = "Assumption")
  xmlOut$addTag("Type", type)  
  xmlOut$addTag("AssumptionBody", body) 
  xmlOut$addTag("Justification", justif) 
  xmlOut$addTag("Established", estabText) 
  xmlOut$addTag("Testable", testtext) 
  xmlOut$addTag("TestApproach", testapp) 
  xmlOut$addTag("TestOutcome", testout) 
  
  if(file=="") { 
    xmlOut$value()
  } else {
    saveXML(xmlOut$value(), file=file)
  }
}

