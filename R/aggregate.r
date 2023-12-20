#' @import data.table dplyr stringr
#' @title Aggregate country data
#' @name aggregate_country_data
#' @description Combine all the years of data for a country in a particular HS classification. Purpose is to have a more robust distribution for 1->n conversions for \code{covert_hs} function, \code{map_df} parameter.
#' @param data Dataframe with data for all years of a country in a given HS classification. Dataframe should be raw output from Comtrade
#' @param agg_columns list of column names to be aggregated/summed up
#' @param group_columns list of columns to group the aggregation by
#' 
#' @return A \code{dataframe} containing the same columns as the input dataframe, less those not either aggregation columns or group by columns
#'
#' @export
aggregate_country_data <- function (data, agg_columns, group_columns) {
  # drop rows with commodity values that are not made up of numbers (e.g., "TOTAL")
  
  # only consider columns that are either to be aggregated or grouped by
  columns <- colnames(data)[colnames(data) %in% c(agg_columns, group_columns)]
  output <- data %>% 
    data.table %>% 
    select(all_of(columns))
  
  # perform aggregation
  agg_string <- str_interp("output[, `:=` (${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = sum(', x, ', na.rm=TRUE)')))), collapse = ', ')}), by = list(${paste0(group_columns, collapse = ',')})]")
  output <- eval(parse(text = agg_string)) %>% 
    distinct() %>% 
    tibble()
  
  return (output)
}


#' @import data.table dplyr stringr
#' @title Aggregate comtrade data to a different digit level
#' @name aggregate_digit_level
#' @description Aggregate comtrade data to a different digit level.
#' @param df The dataframe to aggregate
#' @param agg_columns list of column names to be aggregated/summed up
#' @param group_columns list of columns to group the aggregation by
#' @param commodity_column name of column with commodity codes
#' @param digit_level number of digits to aggregate to, e.g., '6' for 6-digit, etc. 
#' @return A \code{dataframe} 
#'
#' @export

aggregate_digit_level <- function (df, agg_columns, group_columns, commodity_column, digit_level) {
  # only consider columns that are either to be aggregated or grouped by
  columns <- colnames(df)[colnames(df) %in% c(agg_columns, group_columns, commodity_column)]
  output <- df %>% 
    data.table %>% 
    select(all_of(columns))
  
  # adjust commodity column to desired digit level
  output <- output %>% 
    mutate(!!as.symbol(commodity_column) := substr(!!as.symbol(commodity_column), 1, digit_level))
  
  # add commodity column to list of group columns
  group_columns <- c(group_columns, commodity_column)
  
  # perform aggregation
  agg_string <- str_interp("output[, `:=` (${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = sum(', x, ', na.rm=TRUE)')))), collapse = ', ')}), by = list(${paste0(group_columns, collapse = ',')})]")
  output <- eval(parse(text = agg_string)) %>% 
    distinct() %>% 
    tibble()
  
  return (output)
}
