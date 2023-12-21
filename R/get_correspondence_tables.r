#' @import readxl dplyr
#' @title Get correspondence tables from UN stat
#' @description Gets all correspondence tables from the UN and generates a CSV with all the mappings
#' @param digit_level number of digits to create the correspondence table for, e.g., '6' for 6-digit, etc. 
#' @return A \code{dataframe} containing the following columns, also writes the dataframe to \code{data/full_correspondence_tables.csv}:
#' 
#' \item{first}{the 'to' product code}
#' \item{second}{the 'from' product code}
#' \item{first_name}{the hs version of the 'to' product code}
#' \item{second_name}{the hs version of the 'from' product code}
#' \item{relationship}{the correspondence type of this product pair, e.g. 1:n, n:n, etc.}
#'
#' @export
get_correspondence_tables <- function (digit_level = 6) {
  # getting correspondence file urls
  correspondence_url <- "https://unstats.un.org/unsd/classifications/Econ"
  files_prefix <- "https://unstats.un.org"
  html <- paste(readLines(correspondence_url), collapse="\n")
  # all links from page
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  files <- matched[[1]][,2]
  # keeping only xlsx
  files <- files[files %>% sapply(function(x) str_sub(x, -4) == "xlsx" | str_sub(x, -3) == "xls")] %>% unique
  
  # keeping only conversion tables, removing SITC and BEC conversion
  files <- files[grepl("ConversionAndCorrelationTables", files) | grepl("Correlation%20and%20conversion%20tables", files) | grepl("Correlation and conversion tables", files)]
  files <- files[!grepl("SITC", files)]
  files <- files[!grepl("BEC", files)]
  files <- sapply(files, function(x) paste0(files_prefix, str_replace_all(x, " ", "%20"))) %>% unname
  
  # generating df of correspondences
  final_df <- data.frame(first=character(), second=character(), first_name=character(), second_name=character(), relationship=character())
  for (file in files) {
    print(paste0("Getting file ", file))
    if (
      !grepl("CompleteCorrelations", file) & 
      !grepl("SITC", file) & !grepl("BEC", file) & 
      !grepl("Comtrade", file)
    ) { # skip SITC, BEC, Comtrade conversions, because they're not correspondence tables
      
      tmps <- tempfile()
      download.file(file, tmps, quiet = T, mode = "wb")
      sheet_name <- excel_sheets(tmps) %>% .[sapply(tolower(.), function(x) grepl("correlation", x))] # which sheet is the correlation table
      tmp <- read_excel(tmps, sheet=sheet_name, col_names = TRUE) %>% data.frame
      unlink(tmps)
      
      # if "relationship" in the column head, move to first row
      if ("relationship" %in% tolower(colnames(tmp))) {
        tmp[nrow(tmp) + 1,] <- colnames(tmp)
        tmp <- tmp[c(nrow(tmp), 2:(nrow(tmp)-1)),]
      }
      
      # which row does the data start
      for (col in colnames(tmp)) {
        if ("relationship" %in% tolower(tmp[,col])) {
          which_row <- which(tolower(tmp[,col]) == "relationship")
        }
      }
      tmp <- tmp[which_row:nrow(tmp),]
      # drop columns with no data
      drops <- c()
      for (i in 1:ncol(tmp)) {
        if (is.na(tmp[3, i]) | grepl("ex", tmp[3, i])) { # row 3 because sometimes row 2 is empty
          drops <- append(drops, i)
        }
      }
      if (length(drops) > 0) {
        tmp <- tmp[, -drops] 
      }
      tmp <- tmp[!is.na(tmp[,1]),] # drop empty rows
      first_name <- tmp[1,1]; second_name <- tmp[1,2]
      tmp_df <- data.frame(first=tmp[2:nrow(tmp), 1], second=tmp[2:nrow(tmp), 2],first_name=first_name, second_name=second_name, relationship=tmp[2:nrow(tmp), 3])
      final_df <- rbind(final_df, tmp_df)
      print(first_name)
    }
  }
  final_df <- final_df %>% 
    mutate(first_name = str_replace_all(first_name, "\\.", " "), second_name = str_replace_all(second_name, "\\.", " "))
  
  # make sure format is "HS YYYY"
  final_df <- final_df %>% 
    mutate(first_name = ifelse(str_count(first_name, " ") == 0, str_replace(first_name, "HS", "HS "), first_name)) %>% 
    mutate(second_name = ifelse(str_count(second_name, " ") == 0, str_replace(second_name, "HS", "HS "), second_name))
  
  # clean up format of relationships
  final_df <- final_df %>% 
    mutate(relationship = ifelse(relationship %in% c("n to 1", "'n:1"), "n:1", relationship)) %>% 
    mutate(relationship = ifelse(relationship %in% c("n to n", "'n:n"), "n:n", relationship)) %>% 
    mutate(relationship = ifelse(relationship %in% c("1 to 1", "'1:1"), "1:1", relationship)) %>% 
    mutate(relationship = ifelse(relationship %in% c("1 to n", "'1:n"), "1:n", relationship)) %>% 
    mutate(first = ifelse(!relationship %in% c("n:1", "n:n", "1:1", "1:n"), "999999", first)) %>% # if NA product, put 999999
    mutate(second = ifelse(!relationship %in% c("n:1", "n:n", "1:1", "1:n"), "999999", second)) %>% 
    mutate(relationship = ifelse(!relationship %in% c("n:1", "n:n", "1:1", "1:n"), "1:1", relationship)) %>% 
    distinct() %>% 
    tibble()
  
  # getting final digit level, if 6 just take that from the UN
  if (digit_level != 6) {
    # aggregate to desired digit level
    final_df <- final_df %>% 
      mutate(first = substr(first, 1, digit_level), second = substr(second, 1, digit_level)) %>% 
      distinct(first, second, first_name, second_name)
    
    # function to check relationship of a given row
    check_relation <- function (first, first_name, second_name) {
      seconds <- final_df %>% 
        filter(first == !!first, first_name == !!first_name, second_name == !!second_name) %>% 
        select(second) %>% 
        pull() %>% 
        unique()
      firsts <- final_df %>% 
        filter(second %in% seconds, first_name == !!first_name, second_name == !!second_name) %>% 
        select(first) %>% 
        pull() %>% 
        unique()
      
      # 1:1
      if (length(firsts) == 1 & length(seconds) == 1) {
        relationship <- ("1:1")
      } else if (length(firsts) == 1 & length(seconds) > 1) { # 1:n
        relationship <- ("1:n")
      } else if (length(firsts) > 1 & length(seconds) == 1) { # n:1
        relationship <- ("n:1")
      } else if (length(firsts) > 1 & length(seconds) > 1) { # n:n
        relationship <- ("n:n")
      } else {
        relationship <- NA
      }
      
      return (list(firsts = firsts, seconds = seconds, relationship = relationship))
    }
    
    # set all reltionships to NA
    final_df[, "relationship"] <- NA
    
    # cycle through rows
    for (i in 1:nrow(final_df)) {
      print(str_interp("getting ${digit_level}-digit relationships: ${i}/${nrow(final_df)}"))
      
      # only run the row if it wasn't covered by a previous run
      if (is.na(pull(final_df[i, "relationship"]))) {
        results <- check_relation(pull(final_df[i, "first"]), pull(final_df[i, "first_name"]), pull(final_df[i, "second_name"]))
        final_df[final_df$first %in% results$firsts & final_df$second %in% results$seconds & final_df$first_name == pull(final_df[i, "first_name"]) & final_df$second_name == pull(final_df[i, "second_name"]), "relationship"] <- results$relationship
      }
    }
  }
  
  # add reverse correspondences for converting newer HS's into older ones
  reversed_df <- final_df %>% 
    rename(
      first = second,
      second = first,
      first_name = second_name,
      second_name = first_name
    ) %>% 
    select(colnames(final_df)) %>% 
    mutate(relationship = paste0(str_split(relationship, ":")[[1]][2], ":", str_split(relationship, ":")[[1]][1]))
  
  final_df <- final_df %>% 
    rbind(reversed_df) %>% 
    distinct()
  
  return (final_df)
}


#' @title Convert HS classification names to years
#' @description Convert HS classification names to years (e.g., "H0" > 1992) or vice versa
#' @return A number or string
#' 
#' \item{hs_name}{the HS name or year}
#'
#' @export
convert_hs_name <- function (hs_name) {
  # if character, convert name to year
  if (typeof(hs_name) == "character") {
    output <- 1992 + as.numeric(substr(hs_name, 2, 2)) * 5
  } else {
    output <- paste0("H", (hs_name - 1992) / 5)
  }
  
  return (output)
}