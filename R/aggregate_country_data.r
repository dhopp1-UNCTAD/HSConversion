#' @import data.table dplyr
#' @title Aggregate country data
#' @name aggregate_country_data
#' @description Combine all the years of data for a country in a particular HS classification. Purpose is to have a more robust distribution for 1->n conversions for \code{covert_hs} function, \code{map_df} parameter.
#' @param data Dataframe with data for all years of a country in a given HS classification. Dataframe should be raw output from Comtrade with columns:
#' 
#' \item{Year}{the year of the trade data}
#' \item{FlowCode}{the HS version of the data}
#' \item{ReporterCode}{code of the trade flow (export, import, etc.)}
#' \item{ReporterLabel}{the name of the reporter country}
#' \item{PartnerCode}{the code of the partner country}
#' \item{CommodityCode}{the commodity code}
#' \item{PartnerLabel}{the name of the partner country}
#' \item{Value}{the value of recorded trade}
#' 
#' @return A \code{dataframe} containing the following same columns as the input dataframe, with \code{Year} set to 999 to denote it is an aggregation, but to make sure output df has the same columns as the input df.
#'
#' @export

aggregate_country_data <- function (data) {
  output <- data %>% 
    data.table %>% 
    mutate(Year = 999) # filler for Year column
  
  output <- output[, `:=` (Value = sum(Value, na.rm=TRUE)), by = list(Year, FlowCode, ReporterCode, ReporterLabel, PartnerCode, PartnerLabel, CommodityCode)] %>% 
    distinct() %>% 
    data.frame()
  
  return (output)
}

# GTCDIT way
aggregate_country_data <- function (data) {
  output <- data %>% 
    data.table
  
  output <- output[, `:=` (
    CIFValue = sum(CIFValue, na.rm=TRUE), 
    FOBValue = sum(FOBValue, na.rm=TRUE), 
    Qty = sum(Qty, na.rm=TRUE)
  ), 
  by = list(CommodityCode, Origin, Transit, Mot, QtyUnitCode)] %>% 
    distinct() %>% 
    tibble()
  output[output == FALSE] <- NA
  
  return (output)
}
