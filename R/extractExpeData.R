#' Extraction of data from an experimentation based on variables
#'
#' @param expe name of the experiment
#' @param variables vector of the standardized names of variables to extract
#' @param addExpeName true or false if you want to add expe name to the final dataframe
#'
#' @return n unique dataframe that combine all datasets where the variables to extrat where found
#' @export
#' @importFrom dplyr desc mutate group_by summarise filter left_join arrange full_join
#' @importFrom openxlsx readWorkbook getDateOrigin
#'
#' @examples
extractExpeData <- function(expe,variables,addExpeName=T)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("No working environment defined, please use the setRepo() function")}

  xpPath <-repo$MetadataFilePaths[expe]

  if(is.null(repo$DataTables) | !(expe %in% repo$DataTables$expe)) {
    dataTables(expes = expe,noreturn = T)
  }
  myDT <- repo$DataTables
  myDT <- myDT[myDT$expe == expe,]

  ## local binding
  ref_var <- filename <- sheet <- var_order <- maxorder <- NULL

  myDT_hierarchy <- myDT %>%
    dplyr::mutate(var_order = match(ref_var,repo$Hierarchy,nomatch=0)) %>%
    dplyr::group_by(filename,sheet) %>%
    dplyr::summarise(maxorder = max(var_order,na.rm=T))

  ## list of data tables that contains the looked variables
  # listDT <- myDT %>%
  #   dplyr::filter(ref_var %in% variables) %>%
  #   dplyr::left_join(myDT_hierarchy) %>%
  #   dplyr::arrange(dplyr::desc(maxorder)) %>%
  #   dplyr::select(path,sheet,maxorder,metadata) %>%
  #   unique()

  listDT <- myDT %>%
    dplyr::filter(ref_var %in% variables) %>%
    dplyr::left_join(myDT_hierarchy) %>%
    dplyr::group_by(path,sheet,metadata) %>%
    dplyr::summarise(maxorder=max(maxorder),nvar=dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(maxorder),dplyr::desc(nvar))


  prof_max <- max(listDT$maxorder)

  dico <- extractSheet(xpPath,sheet="data_dictionary")

  compteur =0
  repeat
  {
    compteur = compteur + 1
    if(compteur > nrow(listDT)) {break()}

    if(listDT$metadata[compteur]) {
      myDT_i <- extractSheet(listDT$path[compteur],sheet=listDT$sheet[compteur]) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
     } else {
      myDT_i <- openxlsx::readWorkbook(listDT$path[compteur],sheet=listDT$sheet[compteur])

      var_types <- dico$data_type[dico$variable_name %in% colnames(myDT_i)]
      var_types <- iconv(var_types, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
      var_conv <- dico$var_ref_conv[dico$variable_name %in% colnames(myDT_i)]
      for (i in 1:length(var_types))
      {
        if(var_types[i]=="numerique")
          myDT_i[,i] <- as.numeric(myDT_i[,i])
        if (!is.na(var_conv[i])) {
          myDT_i[,i] <- myDT_i[,i]*var_conv[i]
        }
        if(var_types[i]=="date") {
          date_origin <- openxlsx::getDateOrigin(listDT$path[compteur])
          myDT_i[,i] <- as.Date(as.numeric(myDT_i[,i]),origin = date_origin)
        }
      }
      myDT_i <- myDT_i %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      # #### convert name to standard names for data only
      cols_to_change <- which(colnames(myDT_i) %in% dico$variable_name[!is.na(dico$var_ref_name)])
      colnames(myDT_i)[cols_to_change] <- dico$var_ref_name[match(colnames(myDT_i)[cols_to_change],dico$variable_name)]
     }
    ### remove all emty cols
    myDT_i <- myDT_i[,colSums(!is.na(myDT_i))>0]
    #if(any(variables %in% colnames(myDT_i))) {next} ## if no interest to continue

    if(min(dim(myDT_i))>0) {
      if (compteur == 1) {
        results <- myDT_i }
      if( compteur > 1) {
        if(!(listDT$maxorder[compteur]<prof_max & all(variables %in% colnames(results)))) ## if order in hierarchy of DT i is above and all looked variables alreay in DT i : do nothing
        {
          if(length(intersect(colnames(results),colnames(myDT_i)))>0) {  # checking if there are common cols
            results <- dplyr::full_join(results,myDT_i) ## then full_join
          } else {
            results <- dplyr::cross_join(results,myDT_i)
          }
          # if(nrow(myDT_i)==1) { ## with only one row no risk of ambiguity
          #   results <- dplyr::full_join(results,myDT_i)} else {
          #     print(paste("no common variables between",basename(listDT$path[compteur]),listDT$sheet[compteur],
          #                 "and", basename(listDT$path[compteur-1]),listDT$sheet[compteur-1]))
          #     if(length(intersect(colnames(results),colnames(myDT_i)))>0) { ## to check if there is at least one common variable
          #       results <- dplyr::full_join(results,myDT_i)
          #     }
        }
      }
    }
  }


  #### keep only cols with standard names
  no_match_names <- setdiff(colnames(results), myDT$ref_var)
  if(length(no_match_names)>0){
  results <- results[,-which(colnames(results) %in% no_match_names)]
  }
  # convert character to data type
  results <- convertDataType(data=results)


  ### check if "name_of_experiment" is missing and if missing add it to dataExtracted
  if(nrow(results)>0) {
    if (!"name_of_experiment" %in% colnames(results) && addExpeName)
    {
      results$name_of_experiment <- expe ##
    }
  }
  return(results)
}
