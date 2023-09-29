#' extract information of experiments in the repo
#'
#' @return a dataframe with experiments found in the repo
#' @param noreturn if true the function do not return anything, but experiments are stored in the working environnement
#' @export
#' @importFrom dplyr mutate bind_rows across everything
#' @importFrom magrittr %>%
#' @examples
experiments <- function(noreturn=FALSE)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("Pas d'environnement de travail définit, utilisez la fonction setRepo")}
  if(is.null(the$entrepot$Experiments)) {
    list_xlsx_metadata <- the$entrepot$MetadataFilePaths
    ### extract information about experiments
    expes_temp <- data.frame()
    for (i in 1:length(list_xlsx_metadata))
    {
      xp <- extractSheet(wb=list_xlsx_metadata[i],sheet="experimentation") %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      xp <- data.frame(metadataFilePath=list_xlsx_metadata[i],xp)
      expes_temp <- dplyr::bind_rows(expes_temp,xp)
    } # end of for
    the$entrepot$Experiments <- expes_temp
  }
  results <- the$entrepot$Experiments
  if(noreturn==FALSE) {return(results)}
}
