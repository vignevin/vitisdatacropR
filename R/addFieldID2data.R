#' add field name and id to a dataframe
#'
#' @param data a dataframe of data
#' @param expe name of the experiment
#'
#' @return a dataframe with new cols name_of_experiment,field_name and field_id
#' @export
#'
#' @examples
addFieldID2data <- function(data,expe=NULL) {
  data_colnames <- colnames(data)
  if("field_id" %in% data_colnames)
  {
    data2return <- data
    if("field_name" %in% data_colnames & "name_of_experiment" %in% data_colnames )
    {
      data2return$field_id[is.na(data2return$field_id)] <- paste(data2return$name_of_experiment[is.na(data2return$field_id)],
                                                                 data2return$field_name[is.na(data2return$field_id)],sep=":")
    }
    return(data2return)
  }
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {print("No working environment defined, please use the setRepo() function")
    return(NA)}
  if(is.null(expe) & !"name_of_experiment" %in% data_colnames) {
    print("No experiment name found")
    return(NA)
  }
  if(is.null(the$entrepot$Fields)) {
    fields(noreturn=TRUE)
    repo <- the$entrepot ##  the repository where to work define by setRepo
  } ## to populate Fields if necessary


  if("name_of_experiment" %in% data_colnames & !is.null(expe)) {print("expe parameter not used")}

  if(!"name_of_experiment" %in% data_colnames)
  {
    data$name_of_experiment <- expe
  }

  data2return <- merge(data,repo$Fields[,c("name_of_experiment","field_name","field_id")],
                       all.x=T)
  if(nrow(data2return)>nrow(data)) {
    print("Can not merge data without ambiguity. Function stopped without return")
    return(NA)
  }
  if("field_name" %in% data_colnames & "name_of_experiment" %in% data_colnames )
  {
  data2return$field_id[is.na(data2return$field_id)] <- paste(data2return$name_of_experiment[is.na(data2return$field_id)],
                                                             data2return$field_name[is.na(data2return$field_id)],sep=":")
  }
  return(data2return)
}

