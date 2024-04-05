
#' Conversion data frame text selon type de variables
#'
#' @param data data frame with standardized col to convert
#'
#' @return
#' @export
#'
#' @examples
convertDataType <- function(data)
{
  repo <- the$entrepot ##  the repository where to work define by setRepo
  if(!exists("repo")) {stop("No working environment defined, please use the setRepo() function")}

  if(is.null(repo$DataTables)) {
    dataTables(expes = "all",noreturn = T)
  }
  DT <- repo$DataTables

    ### mutate var type according to var_type and convert if necessary
  var_types <- DT$data_type[match(colnames(data),DT$ref_var)]
  var_types <- iconv(var_types, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  for (i in 1:length(var_types))
  {
    if(var_types[i]=="numerique" | var_types[i]=="number")
      data[,i] <- as.numeric(data[,i])
    if(var_types[i]=="interger")
      data[,i] <- as.integer(data[,i])
   }
  return(data)
}
