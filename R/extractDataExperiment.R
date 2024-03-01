#' extract data from experiment
#'
#' @param expe name of the experiment
#' @param variables vector of the standardized names of variables to extract
#' @param addFieldName true or false if you want to add field name to the dataframe
#' @param addExpeName true or false if you want to add expe name to the dataframe
#'
#' @return an unique dataframe that combine all datasets where the variables to extrat where found
#' @export
#' @importFrom openxlsx getSheetNames readWorkbook
#' @importFrom dplyr mutate bind_rows across everything
#' @importFrom stats na.omit
#' @examples

extractDataExperiment <- function(expe,variables,addFieldName=T,addExpeName=T)
{
  #variables <- "YIELD_PLANT"
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("Pas d'environnement de travail définit, utilisez la fonction setRepo")}
  xpPath <-repo$MetadataFilePaths[expe]
  if(length(xpPath)==0) {stop("Aucune expérimentation trouvée avec ce nom")}
  data_dictionnary <- extractSheet(xpPath,sheet="data_dictionnary")
  var_names <- data_dictionnary$variable_name[!is.na(data_dictionnary$var_ref_name)] ## select var_name with var_ref_name
  var_ref_names <- data_dictionnary$var_ref_name[!is.na(data_dictionnary$var_ref_name)] ## the same vector with ref name
  looked_var_names <- var_names[var_ref_names %in% variables] ## to identify the user data variable looked

  ############### EXTRACT DATA from xlsx files ##########
  ## screening files and find variables in colnames
  wd_xp_data <- paste0(dirname(xpPath),"//data//") ## path to access to the data repo of the xp
  ## to check if the "data" directory exists
  if (!dir.exists(wd_xp_data)) {
    stop("No data repository found")
  }
  list_files <- list.files(wd_xp_data,pattern="*.xlsx",recursive = T) ## list of all excel files in data repo

  ## read all xlsx files
  for (f in 1:length(list_files)) #a boucle "for" to read all data files
  {
    myFile <- paste0(wd_xp_data,list_files[f])
    feuilles <- openxlsx::getSheetNames(myFile) ## to identify all sheets names
    #Create a Empty DataFrame with 0 rows and n columns
    dataExtrated <- data.frame(matrix(data="",nrow = 0, ncol = length(var_names)))
    # Assign column names
    colnames(dataExtrated) = var_names
    for (i in 1:length(feuilles))
    {
      names_F <- as.character(openxlsx::readWorkbook(myFile,sheet=feuilles[i],rows=1,colNames = F)) ## read the first line (only names)
      selectCol <- names_F %in% gsub(" ",".",var_names)
      if(sum(names_F %in% looked_var_names)>0) { ## to add a condition if the looking variable is in selectCol ok extract, else no
        dataF <- openxlsx::readWorkbook(myFile,sheet=feuilles[i],cols=c(1:length(selectCol))[selectCol])
        if (!is.null(dataF))
        {
          dataF <-  dataF %>%
            dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
          #dataF %>% select(colnames(dataF)[selectCol])
          dataExtrated <- dplyr::bind_rows(dataExtrated,dataF)
        }
      } else {} ## end of if sum(names_F %in% looked_var_names)>0
    } ## end for feuilles
  } ## end for files

  #### mutate var type according to var_type and convert if necessary
  var_types <- data_dictionnary$data_type[data_dictionnary$variable_name %in% var_names]
  var_conv <- data_dictionnary$var_ref_conv[data_dictionnary$variable_name %in% var_names]
  for (i in 1:length(var_types))
  {
    if(var_types[i]=="numérique")
      dataExtrated[,i] <- as.numeric(dataExtrated[,i])
    if (!is.na(var_conv[i])) {
      dataExtrated[,i] <- dataExtrated[,i]*var_conv[i]
    }
  }

  #### convert name to standard names
  colnames(dataExtrated) <- data_dictionnary$var_ref_name[na.omit(match(colnames(dataExtrated),data_dictionnary$variable_name))]

  ### check if "parcelle" is missing and if missing add it to dataExtracted
  if (!"field_name" %in% colnames(dataExtrated) && addFieldName)
  {
    dataExtrated$field_name <- NA
    if(is.null(the$entrepot$Fields)) {fields(noreturn=TRUE)} ## to populate Fields if necessary
    #nDFprcl <- extractSheet(xpPath,sheet="field")
    myFieldName <-repo$Fields$field_name[repo$Fields$name_of_experiment == expe]
    if (length(myFieldName)>1) {warning("Plusieurs noms de parcelles pour le jeu de données")}
    else {
      dataExtrated$field_name <- myFieldName[1] ## in this case, it should have only one name
    }
  }

  ### to add also field_id
  if (!"field_id" %in% colnames(dataExtrated) && addFieldName) ##
  {
    if(is.null(the$entrepot$Fields)) {fields(noreturn=TRUE)} ## to populate Fields if necessary
    dataExtrated <- merge(dataExtrated,repo$Fields[repo$Fields$name_of_experiment == expe,c("field_id","field_name")],by = "field_name",all.x=T)
    dataExtrated$field_id[is.na(dataExtrated$field_id)] <- paste(expe,dataExtrated$field_name[is.na(dataExtrated$field_id)],sep=":")
  }


  ### check if "name_of_experiment" is missing and if missing add it to dataExtracted
  if (!"name_of_experiment" %in% colnames(dataExtrated) && addExpeName)
  {
    dataExtrated$name_of_experiment <- expe ##
    }
  return(dataExtrated)
} # end of function
