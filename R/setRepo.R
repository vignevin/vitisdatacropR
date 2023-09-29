### definition of an R6 class to store the information in the internal env
the <- new.env(parent = emptyenv())
the$repoClass <- R6::R6Class("Repo",
                   # parent_env = emptyenv(),
                    public = list(
                      Path = getwd(),
                      MetadataFilePaths = NULL,
                      Experiments = NULL,
                      Fields = NULL,
                      initialize = function(Path,MetadataFilePaths,Experiments = NULL,Fields = NULL){
                        self$Path <- Path
                        self$MetadataFilePaths <- MetadataFilePaths
                        self$Experiments <- Experiments
                        self$Fields <- Fields
                      }
                      )
                    )


#' setRepo
#'
#' @param folder le chemin du dossier (repo) a explorer
#' @return no return
#'
#' @importFrom openxlsx readWorkbook loadWorkbook getCreators
#' @importFrom R6 R6Class
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples

setRepo <- function(folder)
{
  list_xlsx <- list.files(folder,pattern="*.xlsx",recursive=T,full.names = T) ##" list of all xlsx
  if (length(list_xlsx)==0) {stop("Aucun fichier de métadonnées standard trouvé")}
  # to identify all standard metadata files
  metadataF<- rep(FALSE,length(list_xlsx))
  nomsXp <- rep(NA,length(list_xlsx))
  for (i in 1:length(list_xlsx))
  {
    wb <- openxlsx::loadWorkbook(list_xlsx[i])
    creator <- openxlsx::getCreators(wb)
    metadataF[i] <- any(grepl("Standard",creator,fixed=T)) ## if we find "standard" in the creator field, this is a standardized metadata file
    if(metadataF[i]) {
      nomsXp[i] <- openxlsx::read.xlsx(wb, namedRegion = "name_of_experiment",colNames = FALSE)[1,1]
    }
  }
  list_xlsx_metadata <- stats::setNames(list_xlsx[metadataF],nomsXp[metadataF])
  if (length(list_xlsx_metadata)==0) {stop("Aucun fichier de métadonnées standard trouvé")}
  print(paste("Entrepot définit:",folder))
  print(paste(length(list_xlsx_metadata),"fichiers de métadonnées standard trouvés:"))
  print(basename(list_xlsx_metadata))
  print("Pour obtenir le descriptif des expérimentations : experiments()")
  print("Pour obtenir le descriptif des parcelles : fields()")
  the$entrepot <- the$repoClass$new(Path = folder,
                                    MetadataFilePaths = list_xlsx_metadata)
}
