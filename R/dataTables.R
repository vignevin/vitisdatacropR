
#' get the list of all data table in a repo
#' @param expes vector of names of the experiment
#' @param noreturn if true the function do not return anything, but experiments are stored in the working environnement
#' @return
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
dataTables <- function(expes="all", noreturn = FALSE)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("No working environment defined, please use the setRepo() function")}

  xpPaths<-repo$MetadataFilePaths
  expes <- ifelse(expes=="all",names(repo$MetadataFilePaths),expes)
  xpPaths<-repo$MetadataFilePaths[names(repo$MetadataFilePaths) %in% expes]
  if(length(xpPaths)==0) {stop("No experimentations found")}

  ### looking if data already in environnement, then do not do again
  if(!is.null(repo$DataTables)) {
    xpPaths <- xpPaths[!names(xpPaths) %in% unique(repo$DataTables$expe)]
  }

  if(length(xpPaths)>0) {
  for (i in 1:length(xpPaths))
  {
    path <- xpPaths[i]
    if(length(path)==0) {stop("No experimentation found with this name")}
    expe <- names(path)

    ### looking in metadata file
    # metadata_sheets <- openxlsx::getSheetNames(path) ## to identify all sheets names
    # metadata_sheets <- metadata_sheets[metadata_sheets!="listes"&metadata_sheets!="HELP"]
    # meta_results <- data.frame()
    # for (sheet in metadata_sheets)
    # {
    #   temp_data <- extractSheet(path,sheet=sheet)
    #   meta_results <- rbind(meta_results,c(expe,path,basename(path),sheet,paste(colnames(temp_data),collapse = ";"),nrow(temp_data)))
    # }
    # colnames(meta_results) <- c("expe","path","filename","sheet","ref_vars","nrow")
    xp_metadata <- openxlsx::getNamedRegions(path)
    xp_metadata_split <- strsplit(xp_metadata, split = "\\.")
    meta_results <- do.call(rbind, lapply(xp_metadata_split, function(x) data.frame(sheet = x[1], ref_var = x[2], stringsAsFactors = FALSE)))
    #meta_results <- aggregate(ref_vars ~ sheet, data = meta_results, FUN = function(x) paste(x, collapse = ";"))
    meta_results$expe = expe
    meta_results$path = path
    meta_results$metadata = TRUE
    meta_results$filename = basename(path)

    ### chargement du contexte
    context <- openxlsx::readWorkbook("https://github.com/vignevin/vitisdatacrop/raw/main/experimental_context_description.xlsx")
    colnames(context)[colnames(context)=="property"]<-"ref_var"
    meta_results <- merge(meta_results,context[,c("ref_var","type")],all.x=T)
    colnames(meta_results)[colnames(meta_results)=="type"]<-"data_type"


    ## screening files and find variables in colnames
    data_dictionary <- extractSheet(path,sheet="data_dictionary")
    wd_xp_data <- paste0(dirname(path),"//data//") ## path to access to the data repo of the xp

    ## to check if the "data" directory exists
    if (!dir.exists(wd_xp_data)) {
      stop("No data folder found")
    }
    list_files <- list.files(wd_xp_data,pattern="*.xlsx",recursive = T) ## list of all excel files in data repo

    results <- data.frame()
    ## read all xlsx files
    for (theFile in list_files) #a boucle "for" to read all data files
    {
      myFile <- paste0(wd_xp_data,theFile)
      feuilles <- openxlsx::getSheetNames(myFile) ## to identify all sheets names
      #Create a Empty DataFrame with 0 rows and n columns

      for (theSheet in feuilles)
      {
        #temp_data <- openxlsx::readWorkbook(myFile,sheet=theSheet)
        #original_vars <- colnames(temp_data)
        original_vars <- as.character(openxlsx::readWorkbook(myFile,sheet=theSheet,rows=1,colNames = F)) ## read the first line (only names)
        ref_vars <- data_dictionary[match(original_vars,data_dictionary$variable_name),c("var_ref_name","data_type")]
        ref_var <- na.omit(ref_vars)
        colnames(ref_var)[colnames(ref_var)=="var_ref_name"]<-"ref_var"
        if(nrow(ref_var)>0){ ## to be sure that there is a least one variable found
          df <- data.frame(ref_var,expe=expe,path=myFile,filename=basename(myFile),sheet= theSheet,metadata = F)
          results <- rbind(results,df)
        }
        #results <- rbind(results,c(expe,myFile,basename(myFile),theSheet,paste(original_vars,collapse = ";"),paste(ref_vars,collapse=";")))
      }
    }
    results <- dplyr::bind_rows(results,meta_results)

    ## harmonize var_types
    results$data_type <- iconv(results$data_type, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    results$data_type[results$data_type == "numerique"] <- "number"
    results$data_type[results$data_type == "texte"] <- "string"
    results$data_type[results$data_type == "date"] <- "date"


    if(!is.null(the$entrepot$DataTables)) {
      the$entrepot$DataTables <- rbind(the$entrepot$DataTables,results) # complete the existing files
     } else {
       the$entrepot$DataTables <- results # first time
    }

  }
  }
  if(noreturn==FALSE) {
    if(expes !="all") {return(the$entrepot$DataTables[the$entrepot$DataTables$expe %in% expes,])} else
    {return(the$entrepot$DataTables)}
  }
}
