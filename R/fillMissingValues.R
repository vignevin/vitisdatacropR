#' Function to fill missing values in a data frame from other values according to an id
#'
#' @param df dataframe to complete
#' @param id_field id field in the data frmae
#'
#' @return a filled data frame
#' @export
#'
#' @examples
fillMissingValues <- function(df,id_field="field_id") {
  if(!id_field %in% colnames(df)) {stop("Invalid field identifier")}
  ### code pour propager les metadonnes si elles manques
  col_id <- which(colnames(df)==id_field) ## numero de la colonne identifiante
  duplicates <- names(table(df[,col_id])[table(df[,col_id])>1]) ## id of duplicate fields
  #variable <- value <- NULL ## binding the variable locally to the function
  df_unique <- df %>%
    dplyr::filter(get(id_field) %in% duplicates) %>% ## get necessary to use the parameter
    dplyr::group_by(get(id_field)) %>% ##
    #summarise(across(everything(), ~paste0(unique(.,na.), collapse = "|"))) ### collapse with | to keep all data
    dplyr::summarise(dplyr::across(dplyr::everything(), ~{
      unique_values <- unique(.)
      # Supprimer les valeurs NA de la liste des valeurs uniques
      unique_values <- unique_values[!is.na(unique_values)]
      unique_values[length(unique_values)>1] <- "" ## pour supprimer si plus d'1 possibilite
      # Concatener les valeurs uniques restantes
      #paste0(unique_values, collapse = "|")
      paste0(unique_values,collapse = "")
    }))

  df_unique[df_unique=="NA"] <- NA
  col_uid <- which(colnames(df_unique)==id_field) ## numero de la colonne identifiante

  ### boucle de remplissage
  df2 <- df
  for (i in which(df2[,col_id] %in% duplicates)) ## boucle sur les lignes dupliquées
  {
    for (c in colnames(df2))
    {
      if(is.na(df2[i,c])) { ## si la valeur est manquante
        df2[i,c] <- df_unique[df_unique[,col_uid]==df2[i,col_id],c]
      }
    }
  }
  return(df2)
}
