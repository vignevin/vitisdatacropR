#' extract information of data dictionary in the repo
#'
#' @return a dataframe with dictionaries found in the repo
#' @param noreturn if true the function do not return anything, but dictionary are stored in the working environnement
#' @export
#' @importFrom dplyr mutate bind_rows across everything group_by
#' @importFrom magrittr %>%
#' @examples
dictionaries <- function(noreturn=FALSE)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("Pas d'environnement de travail défini, utilisez la fonction setRepo")}
  list_xlsx_metadata <- the$entrepot$MetadataFilePaths
  # ### extract information about variables
  dic_temp <- data.frame()
  for (i in 1:length(list_xlsx_metadata))
  {
    xp <- extractSheet(wb=list_xlsx_metadata[i],sheet="data_dictionary") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    xp <- data.frame(name_of_experiment=names(list_xlsx_metadata[i]),xp)
    dic_temp <- dplyr::bind_rows(dic_temp,xp)
  } # end of for
  if(nrow(dic_temp)>0) {
  the$entrepot$Dictionaries <- dic_temp
  results <- the$entrepot$Dictionaries
  if(noreturn==FALSE) {return(results)}
  } else {
    print("Aucun dictionnaire de données trouvé")
  }
}
