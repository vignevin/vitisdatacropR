## function to extract a sheet from a standardized file
## and rename the cols with their standardized name
## this is an internal function

#' extractSheet
#'
#' @param wb
#' @param sheet
#'
#' @return
#' @noRd
#' @importFrom openxlsx readWorkbook getNamedRegions
#'
#' @examples
extractSheet <- function(wb,sheet)
{

sheetDF <- openxlsx::readWorkbook(wb,sheet=sheet,rowNames = F, detectDates = TRUE) ## to read the data dictionnary
# rename cols with var_name instead of label
namedR <- openxlsx::getNamedRegions(wb) ## to extract all named regions in excel
noms <- namedR[attributes(namedR)$sheet==sheet] ## find named region in the sheet
#colL <- substr(attributes(namedR)$position[namedR %in% noms],start=1,stop=1) ## find the letters of cols for each named region (named region are in cols)
colL <- substr(attributes(namedR)$position[namedR %in% noms],start=1,stop=2) ## find the letters of cols for each named region (named region are in cols)
colL <- colL[nchar(colL)>0] ## to remove not found colonnes (may happens sometimes)
colL <- gsub('[0-9]+', '', colL) ## letters only
colPos <- rep(NA,length(colL))
colPos[nchar(colL)==1] <- match(unlist(strsplit(paste(colL[nchar(colL)==1], collapse = ''), split="")), LETTERS) ## convert to the number corresponding to the letter : A=1 , C =3 ...
colPos[nchar(colL)==2] <- 26+match(unlist(strsplit(paste(substr(colL[nchar(colL)==2],start=2,stop=2), collapse = ''), split="")), LETTERS)
colnames(sheetDF)[colPos] <- noms ## change colnames of data.frame
sheetDF <- sheetDF[,colPos] ## keep only col with a referenced name
## to format date
is.date <- function(x) inherits(x, 'Date')
dateCol <- sapply(sheetDF, is.date)
sheetDF[,dateCol] <- format(sheetDF[,dateCol],"%Y-%m-%d")
return(sheetDF)
}
