#' extract information of fields in the repo
#'
#' @return a dataframe with fields found in the repo
#' @param noreturn if true the function do not return anything, but fields are stored in the working environnement
#' @export
#' @importFrom dplyr mutate bind_rows across everything
#' @importFrom magrittr %>%
#' @examples
fields <- function(noreturn=FALSE,unique_id=FALSE)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("Pas d'environnement de travail définit, utilisez la fonction setRepo")}
  if(is.null(the$entrepot$Fields)) {
    list_xlsx_metadata <- the$entrepot$MetadataFilePaths
    # ### extract information about fields
    field_temp <- data.frame()
    for (i in 1:length(list_xlsx_metadata))
    {
      xp <- extractSheet(wb=list_xlsx_metadata[i],sheet="field") %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      xp <- data.frame(name_of_experiment=names(list_xlsx_metadata[i]),xp)
      field_temp <- dplyr::bind_rows(field_temp,xp)
    } # end of for
    if(sum(is.na(field_temp$field_name))>1) {
    warning("Les parcelles sans nom ont été supprimées")
    field_temp <- field_temp[!is.na(field_temp$field_name),] ## to remove all rows without field_name, required
    }
    ### to add an id for each field ( name_of_experiment::field_name if field_id missing)
    field_temp$field_id[is.na(field_temp$field_id)] <- paste(field_temp$name_of_experiment[is.na(field_temp$field_id)],field_temp$field_name[is.na(field_temp$field_id)],sep=":")

    if(unique_id)
    {
      field_temp <- field_temp %>%
        group_by(field_id) %>%
        #summarise(across(everything(), ~paste0(unique(.,na.), collapse = "|"))) ### collapse with | to keep all data
        summarise(across(everything(), ~{
        unique_values <- unique(.)
        # Supprimer les valeurs NA de la liste des valeurs uniques
        unique_values <- unique_values[!is.na(unique_values)]
        # Concaténer les valeurs uniques restantes
        paste0(unique_values, collapse = "|")
      }))
    }
    the$entrepot$Fields <- field_temp
  }
  results <- the$entrepot$Fields
  if(noreturn==FALSE) {return(results)}
}
