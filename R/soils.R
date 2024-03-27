#' extract information of data dictionary in the repo
#'
#' @return a dataframe with soils metadat found in the repo
#' @param noreturn if true the function do not return anything, but soils are stored in the working environnement
#' @export
#' @importFrom dplyr mutate bind_rows across everything group_by
#' @importFrom magrittr %>%
#' @examples
soils <- function(noreturn=FALSE)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("Pas d'environnement de travail défini, utilisez la fonction setRepo")}
  list_xlsx_metadata <- the$entrepot$MetadataFilePaths
  # ### extract information about variables
  soil_temp <- data.frame()
  for (i in 1:length(list_xlsx_metadata))
  {
    xp <- extractSheet(wb=list_xlsx_metadata[i],sheet="soil") %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    if(nrow(xp)>0) {
    xp <- data.frame(name_of_experiment=names(list_xlsx_metadata[i]),xp)
    soil_temp <- dplyr::bind_rows(soil_temp,xp)
    }
  } # end of for
  if(nrow(soil_temp)>0) {
  the$entrepot$Soils <- soil_temp
  results <- the$entrepot$Soils
  if(noreturn==FALSE) {return(results)}
  } else {
    print("Aucune métadonnée de sol trouvée")
  }
}
