### definition of an R6 class to store the information in the internal env
the <- new.env(parent = emptyenv())
the$repoClass <- R6::R6Class("Repo",
                   # parent_env = emptyenv(),
                    public = list(
                      Path = getwd(),
                      MetadataFilePaths = NULL,
                      Experiments = NULL,
                      Fields = NULL,
                      Dictionaries = NULL,
                      Soils = NULL,
                      DataTables = NULL,
                      Hierarchy = NULL,
                      initialize = function(Path,MetadataFilePaths,Experiments = NULL,Fields = NULL,
                                            Dictionaries = NULL, Soils = NULL, DataTables = NULL,
                                            Hierarchy = c("field_id","field_name","treatment_name","replicate")){
                        self$Path <- Path
                        self$MetadataFilePaths <- MetadataFilePaths
                        self$Experiments <- Experiments
                        self$Fields <- Fields
                        self$Dictionaries <- Dictionaries
                        self$Soils <- Soils
                        self$DataTables <- DataTables
                        self$Hierarchy <- Hierarchy
                      }
                      )
                    )


#' setRepo
#'
#' @param folder repository path to explore
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
  if (length(list_xlsx)==0) {stop("No standard metadata files found")}
  # to identify all standard metadata files
  metadataF<- rep(FALSE,length(list_xlsx))
  nomsXp <- rep(NA,length(list_xlsx))
  for (i in 1:length(list_xlsx))
  {
    wb <- openxlsx::loadWorkbook(list_xlsx[i])
    creator <- openxlsx::getCreators(wb)
    metadataF[i] <- any(grepl("Standard",creator,fixed=T)) ## if we find "standard" in the creator field, this is a standardized metadata file
    if(metadataF[i]) {
      nomsXp[i] <- openxlsx::read.xlsx(wb, namedRegion = "experimentation.name_of_experiment",colNames = FALSE)[1,1]
    }
  }
  list_xlsx_metadata <- stats::setNames(list_xlsx[metadataF],nomsXp[metadataF])
  if (length(list_xlsx_metadata)==0) {stop("No standard metadata files found")}
  print(paste("Repository:",folder))
  print(paste(length(list_xlsx_metadata),"Standard metadata files found:"))
  print(basename(list_xlsx_metadata))
  print("To get the description of the experiments : experiments()")
  print("To get the description of the fields : fields()")
  print("To get the data dictionary : dictionaries()")
  print("To get the description of the soils : soils()")
  print("To explore data tables : dataTables()")
  the$entrepot <- the$repoClass$new(Path = folder,
                                    MetadataFilePaths = list_xlsx_metadata)
}
