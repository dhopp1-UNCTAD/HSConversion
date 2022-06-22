#' @title Convert Comtrade data from one HS year to another
#' @name convert_hs
#' @description Convert Comtrade data from one HS year to another
#' @param correspondence_table Dataframe with HS mappings, obtained from the \code{get_correspondence_tables} function.
#' @param hs_from Integer of which HS year the original data is in (e.g., '2017').
#' @param hs_to Integer of which HS year the data should be converted to (e.g., '2017').
#' @param df The original dataframe to convert.
#' @param df_map A dataframe of the country in the desired HS year to use for 1->n mappings. If omitted, will use equal distribution for 1-n mappings.
#' @return A \code{dataframe} containing the following columns:
#' 
#' \item{first}{the 'to' product code}
#' \item{second}{the 'from' product code}
#' \item{first_name}{the hs version of the 'to' product code}
#' \item{second_name}{the hs version of the 'from' product code}
#' \item{relationship}{the correspondence type of this product pair, e.g. 1:n, n:n, etc.}
#'
#' @export

convert_hs <- function (correspondence_table, hs_from, hs_to, df, map_df=NA) {
  print("test")
}