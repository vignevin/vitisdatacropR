#' Variables
#'
#' @param expes vector of names of experiments, default value all to scan all experiments in the repo
#' @return a dataframe with all variables for each experiment
#' @export
#'
#' @examples
variables <- function(expes="all")
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("No working environment defined, please use the setRepo() function")}
  results <- data.frame()
  xpPaths<-repo$MetadataFilePaths
  if(!"all" %in% expes) {xpPaths<-repo$MetadataFilePaths[names(repo$MetadataFilePaths) %in% expes]}
  for (i in 1:length(xpPaths))
  {
    xp <- extractSheet(xpPaths[i],sheet="data_dictionary")
    xp <- data.frame(name_of_experiment=names(xpPaths[i]),
                     variables=xp$var_ref_name[!is.na(xp$var_ref_name)])
    results <- rbind(results,xp)
  } # end of for
  return(results)
}
