#' get all variables in a repo in both data and metadata
#'
#' @param expes vector of names of experiments, default value all to scan all experiments in the repo
#'
#' @return a named list by experiment with metadata and variables
#' @export
#' @importFrom openxlsx getNamedRegions
#' @examples
getAllVariables <- function(expes="all")
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("No working environment defined, use the setRepo function")}
  results <- list()
  xpPaths<-repo$MetadataFilePaths
  if(!"all" %in% expes) {xpPaths<-repo$MetadataFilePaths[names(repo$MetadataFilePaths) %in% expes]}
  for (i in 1:length(xpPaths))
  {
    xp_metadata <- openxlsx::getNamedRegions(xpPaths[i])
    xp_metadata <- as.character(gsub(".*\\.","",xp_metadata)) ## change colnames of data.frame according to names in standard (without the name of sheet before)
    xp_metadata <- unique(xp_metadata)
    xp_variables <- vitisdatacropR::variables(expes=names(xpPaths[i]))$variables
    list_temp <- list(list(metadata = xp_metadata, variables= xp_variables))
    names(list_temp) <- make.names(xpPaths[i])
    results <- append(results,list_temp)
  } # end of for
  return(results)
}
