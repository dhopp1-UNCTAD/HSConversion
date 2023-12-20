options(dplyr.summarise.inform = FALSE)

#' @import data.table dplyr stringr
#' @title Convert Comtrade data from one HS year to another
#' @name convert_hs
#' @description Convert Comtrade data from one HS year to another.Not that any commodity codes that are not made up exclusively of numbers (e.g., a value like "TOTAL"), will be dropped from the conversion
#' @param correspondence_tables Dataframe with HS mappings, obtained from the \code{get_correspondence_tables} function.
#' @param hs_from Integer of which HS year the original data is in (e.g., '2017').
#' @param hs_to Integer of which HS year the data should be converted to (e.g., '2017').
#' @param df The original dataframe to convert with columns
#' @param agg_columns list of column names to be aggregated/summed up
#' @param group_columns list of columns to group the aggregation by
#' @param commodity_column name of column with commodity codes
#' @param aggregate_order order of aggregation/values columns to use in mapping if one is missing in the mapping dataframe
#' @param map_df A dataframe of the country in the desired HS year to use for 1->n mappings. If omitted, will use equal distribution for 1-n mappings. Same columns as \code{df} parameter dataframe.
#' @param quiet A boolean of whether or not to print out progress
#' @return A \code{dataframe} 
#'
#' @export

convert_hs <- function (correspondence_tables, hs_from, hs_to, df, agg_columns, group_columns, commodity_column, aggregate_order = NA, map_df = NA, quiet = TRUE) {
  # keep only necessary columns
  column_names <- colnames(df)[colnames(df) %in% c(agg_columns, group_columns, commodity_column)]
  df <- df %>% 
    select(all_of(column_names)) %>% 
    mutate(!!as.symbol(commodity_column) := as.character(!!as.symbol(commodity_column))) # ensure commodity column is a character
  
  digit_level <- eval(parse(text = str_interp("nchar(df$${commodity_column})"))) %>% 
    max()
  
  # left pad commodity codes for leading 0s
  df <- df %>% 
    mutate(!!as.symbol(commodity_column) := str_pad(!!as.symbol(commodity_column), digit_level, "left", "0"))
    
  # creating a default aggregate_order
  if (is.na(aggregate_order[1])) {
    aggregate_order <- agg_columns
  }
  
  # drop rows with commodity values that are not made up of numbers (e.g., "TOTAL")
  df <- tibble(df) %>% 
    mutate(numer_check = as.numeric(!!as.symbol(commodity_column))) %>% 
    filter(!is.na(numer_check)) %>% 
    select(-numer_check)
  
  if (!is.na(map_df)[1]) {
    map_df <- tibble(map_df) %>% 
      mutate(numer_check = as.numeric(!!as.symbol(commodity_column))) %>% 
      filter(!is.na(numer_check)) %>% 
      select(-numer_check)
    
    # check if map df has same group columns as df, if not (likely because of a 'year' column), duplicate the map_df for each of the missing values in the column
    missing_columns <- group_columns[!group_columns %in% colnames(map_df)]
    if (length(missing_columns) > 0) {
      counter <- 1
      for (missing_column in missing_columns) {
        for (missing_value in pull(distinct(df[,missing_column]))) {
          tmp_map_df <- tibble(map_df)
          tmp_map_df[, missing_column] <- missing_value
          if (counter == 1) {
            final_map_df <- tmp_map_df
          } else {
            final_map_df <- final_map_df %>% 
              rbind(tmp_map_df)
          }
          counter <- counter + 1
        }
      }
      map_df <- final_map_df
    }
    
    map_df <- map_df %>% 
      select(all_of(column_names)) %>% 
      mutate(!!as.symbol(commodity_column) := as.character(!!as.symbol(commodity_column))) %>% # ensure commodity column is a character
      mutate(!!as.symbol(commodity_column) := str_pad(!!as.symbol(commodity_column), digit_level, "left", "0"))
  }
  
  # getting types of columns
  col_type_list <- df %>% 
    summarise_all(class) %>% as.list
  
  # converting dataframes to datatables for faster processing/filtering later on
  map_df <- data.table(map_df)
  df <- data.table(df)
  
  # if data already in latest_hs, just return the data
  if (hs_from == hs_to) {
    final_df <- df
  } else { # else calculate via proportions the old HS data in terms of new HS codes
    # initializing final dataframe
    final_df_string <- ""
    for (column in column_names) {
      final_df_string <- paste0(final_df_string, str_interp("${column} = ${col_type_list[[column]]}()"))
      if (column != column_names[length(column_names)]) {
        final_df_string <- paste0(final_df_string, ", ")
      }
    }
    final_df <- eval(parse(text = str_interp("data.frame(${final_df_string})")))
    
    # correspondence table for this HS combination
    corr_table <- correspondence_tables %>% 
      filter(first_name == paste0("HS ", hs_to), second_name == paste0("HS ", hs_from))
    
    # loop through each commodity code in the latest correspondence table
    done_codes <- c() # initialize list to keep track of codes already handled. This is because some codes, e.g. for 1:n and n:n correspondences, will show up in another codes' sections, so don't want to do them twice.
    
    counter <- 0
    for (new_code in unique(eval(parse(text = str_interp("df$${commodity_column}"))))) {
      if (!quiet) {
        print(paste0(counter, "/", length(unique(eval(parse(text = str_interp("df$${commodity_column}")))))))
      }
      counter <- counter + 1
      
      # skip the code if it was already handled in another section
      if (!(new_code %in% done_codes)) {
        tmp_corr <- corr_table %>% filter(second==new_code)
        
        # if missing this code, put in as a 1-1
        if (nrow(tmp_corr) == 0) {
          tmp_corr <- corr_table[1,]
          tmp_corr[1,"first"] <- new_code
          tmp_corr[1,"second"] <- new_code
          tmp_corr[1,"relationship"] <- "1:1"
        }
        
        all_related_new_codes <- c()
        all_related_old_codes <- tmp_corr[,"second"] %>% pull
        
        # find which corresponding codes are in the old and new data
        length_begin <- length(all_related_old_codes) + length(all_related_new_codes)
        length_end <- length_begin + 1
        # if new codes are still being added, keep going
        while (length_end > length_begin) {
          length_begin <- length(all_related_old_codes) + length(all_related_new_codes)
          # recursively add all associated new and old codes, so in end have all codes related to this one to get ratios and redistribute
          for (code in all_related_old_codes) {
            new_codes <- corr_table %>% filter(second==code) %>% select(first) %>% pull # get all the new codes associated with each of the old codes
            all_related_new_codes <- append(all_related_new_codes, new_codes) %>% unique
          }
          for (code in all_related_new_codes) {
            new_codes <- corr_table %>% filter(first==code) %>% select(second) %>% pull # get all the old codes associated with each of the new codes
            all_related_old_codes <- append(all_related_old_codes, new_codes) %>% unique
          }
          length_end <- length(all_related_old_codes) + length(all_related_new_codes)
        }
        
        # append done codes to skip list, if the code was covered in a previous section don't run it again
        done_codes <- append(done_codes, all_related_old_codes) %>% unique
        
        # only do calculations and append if there are any of that code in the data
        tmp_old <- df[CommodityCode %in% all_related_old_codes,]
        
        if (nrow(tmp_old) > 0) {
          tmp_old_string <- str_interp("tmp_old[, .(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = sum(', x, ', na.rm=TRUE)')))), collapse = ', ')}), by = list(${paste0(group_columns, collapse = ',')})]")
          tmp_old <- eval(parse(text = tmp_old_string))
          
          # putting tmp_old (all related products, collapsing the commodity code column) in terms of % of that group
          code_string <- "tmp_old %>% "
          for (agg_column in agg_columns) {
            code_string <- paste0(code_string, str_interp("mutate(${agg_column} = ${agg_column} / sum(${agg_column}, na.rm = TRUE))"))
            if (agg_column != agg_columns[length(agg_columns)]) {
              code_string <- paste0(code_string, " %>% ")
            }
          }
          tmp_old_perc <- eval(parse(text = code_string))
          
          # mapping commodity code distribution
          # only use the mapping dataframe if 1) exists 2) n:n or 1:n 3) mapping data exists for those codes
          use_map <- FALSE
          if (
            (length(map_df) > 1) &
            (length(all_related_new_codes) > 1)
          ) {
            if ((map_df %>% 
                 filter(!!as.symbol(commodity_column) %in% all_related_new_codes) %>% nrow) > 0) {
              use_map <- TRUE
            }
          }
          
          # if no all related new codes (i.e. probably 999999 1:1 correspondence not in the correspondence table), set to 99999
          if (length(all_related_new_codes) == 0) {
            all_related_new_codes <- paste0(replicate(digit_level, "9"), collapse = "")
          }
          
          if (use_map) {
            code_string <- str_interp("map_df %>% filter(!!as.symbol(commodity_column) %in% all_related_new_codes) %>% group_by(${commodity_column}) %>% summarise(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = sum(', x, ', na.rm=TRUE)')))), collapse = ', ')})")
            tmp_map <- eval(parse(text = code_string))
            
            # if missing a quantity put same distribution as another aggregate column, in order as specified
            for (agg_column in agg_columns) {
              if (eval(parse(text = str_interp("sum(tmp_map$${agg_column}, na.rm = TRUE) == 0")))) {
                for (agg_column2 in aggregate_order) {
                  if (agg_column2 != agg_column) {
                    if (eval(parse(text = str_interp("sum(tmp_map$${agg_column2}, na.rm = TRUE) != 0")))) {
                      eval(parse(text = str_interp("tmp_map$${agg_column} <- tmp_map$${agg_column2}")))
                      break
                    }
                  }
                }
              } else if (eval(parse(text = str_interp("sum(tmp_map$${agg_column}, na.rm = TRUE) == 0")))) { # if no other quantities have a value, put 1 for equal distribution
                eval(parse(text = str_interp("tmp_map$${agg_column} <- 1")))
              }
            }
          } else { # dummy equal distribution if no map_df provided
            if (length(all_related_new_codes) == 1) { # force character/quotes if only a single code
              code_string <- str_interp("data.table(${commodity_column} = '${all_related_new_codes}', ${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = 1')))), collapse = ', ')})")
            } else {
              code_string <- str_interp("data.table(${commodity_column} = ${all_related_new_codes}, ${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = 1')))), collapse = ', ')})")
            }
            tmp_map <- eval(parse(text = code_string))
          }
          
          # converting commodity code map into percentages
          code_string <- str_interp("tmp_map %>% mutate(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = ', x, ' / sum(', x, ', na.rm=TRUE)')))), collapse = ', ')})")
          tmp_map_perc <- eval(parse(text = code_string))
          
          # distributing percentages to create final harmonized data
          # group old perc by everything but commodity code
          code_string <- str_interp("tmp_old_perc %>% group_by(${paste0(group_columns[group_columns != commodity_column], collapse = ', ')})")
          code_string <- paste0(code_string, str_interp(" %>% summarise(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = ', 'sum(', x, ', na.rm=TRUE)')))), collapse = ', ')})"))
          tmp_old_perc <- eval(parse(text = code_string))
          
          tmp_final_df <- tmp_old_perc %>% 
            mutate(by=1) %>% 
            full_join(tmp_map_perc %>% mutate(by=1), by = "by") %>% 
            select(-by)
          
          code_string <- str_interp("tmp_final_df %>% mutate(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = ', x, '.x * ', x, '.y')))), collapse = ', ')})")
          tmp_final_df <- eval(parse(text = code_string))
          tmp_final_df <- tmp_final_df %>% 
            select(all_of(column_names))
          
          code_string <- code_string <- str_interp("tmp_final_df %>% mutate(${paste0(unname(unlist(sapply(agg_columns, function (x) paste0(x, ' = ', x, ' * sum(tmp_old$', x, ', na.rm=TRUE)')))), collapse = ', ')})")
          tmp_final_df <- eval(parse(text = code_string))
          
          final_df <- rbind(final_df, tmp_final_df)
        } 
      }
    }
  }
  return (final_df)
}
