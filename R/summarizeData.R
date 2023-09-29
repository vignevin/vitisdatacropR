#' Title
#'
#' @param data2sum a dataframe with variables to summarize
#' @param groups a vector of grouping variables
#' @param variables a vector of numeric variables to summarize with mean, sd, count
#'
#' @return
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr summarise filter group_by_at
#' @importFrom stats median na.omit quantile
#' @examples
summarizeData <- function(data2sum,variables,groups)
{
  if(!all(groups %in% colnames(data2sum))) {stop("AU moins un nom de groupe n'est pas dans le tableau de données")}
  if(!all(variables %in% colnames(data2sum))) {stop("AU moins un nom de variables n'est pas dans le tableau de données")}
  variable <- value <- NULL ## binding the variable locally to the function
  dataSum <- data2sum %>%
    tidyr::pivot_longer(dplyr::where(is.numeric),names_to ="variable") %>%
    dplyr::filter(variable %in% variables) %>% #to extract only var_selected
    dplyr::group_by_at(c(groups,"variable")) %>% # to group by variable_for_groups
    dplyr::summarise(m = mean(value,na.rm=T),
              med = median(value,na.rm=T),
              min = min(value,na.rm=T),
              max = max(value,na.rm=T),
              q1 = quantile(value,probs=0.25,na.rm=T),
              q3 = quantile(value,probs=0.75,na.rm=T),
              nb = sum(!is.na(value)))
  return(dataSum)
}
