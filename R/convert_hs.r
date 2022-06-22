#' @title Convert Comtrade data from one HS year to another
#' @name convert_hs
#' @description Convert Comtrade data from one HS year to another
#' @param correspondence_table Dataframe with HS mappings, obtained from the \code{get_correspondence_tables} function.
#' @param hs_from Integer of which HS year the original data is in (e.g., '2017').
#' @param hs_to Integer of which HS year the data should be converted to (e.g., '2017').
#' @param df The original dataframe to convert.
#' @param map_df A dataframe of the country in the desired HS year to use for 1->n mappings. If omitted, will use equal distribution for 1-n mappings.
#' @return A \code{dataframe} containing the following columns:
#'
#'\itemize{
#' \item{hs_version}{the HS version of the data} 
#' \item{year}{the year of the trade data}
#' \item{flow_code}{code of the trade flow (export, import, etc.)}
#' \item{flow}{the name of the trade flow}
#' \item{reporter_code}{the code of the reporter country}
#' \item{reporter}{the name of the reporter country}
#' \item{partner_code}{the code of the partner country}
#' \item{partner}{the name of the partner country}
#' \item{commodity_code}{the commodity code}
#' \item{value}{the value of recorded trade}}
#'
#' @export

convert_hs <- function (correspondence_table, hs_from, hs_to, df, map_df=NA) {
  print("test")
}