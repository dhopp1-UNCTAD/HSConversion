#' @title Convert Comtrade data from one HS year to another
#' @name convert_hs
#' @description Convert Comtrade data from one HS year to another.
#' @param correspondence_tables Dataframe with HS mappings, obtained from the \code{get_correspondence_tables} function.
#' @param hs_from Integer of which HS year the original data is in (e.g., '2017').
#' @param hs_to Integer of which HS year the data should be converted to (e.g., '2017').
#' @param df The original dataframe to convert with columns
#' @param map_df A dataframe of the country in the desired HS year to use for 1->n mappings. If omitted, will use equal distribution for 1-n mappings. Same columns as \code{df} parameter dataframe.
#' @param quiet A boolean of whether or not to print out progress
#' @return A \code{dataframe} 
#'
#' @export

convert_hs <- function (correspondence_tables, hs_from, hs_to, df, map_df=NA, quiet=TRUE) {
  # column names of data
  column_names <- colnames(df)
  
  # converting dataframes to datatables for faster processing/filtering later on
  map_df <- data.table(map_df)
  df <- data.table(df)
  
  # if data already in latest_hs, just return the data
  if (hs_from == hs_to) {
    final_df <- df
  } else { # else calculate via proportions the old HS data in terms of new HS codes
    # initializing final dataframe
    final_df <- data.frame(
      CommodityCode = character(), 
      Origin = character(),
      Transit = character(),
      Mot = integer(),
      CIFValue = numeric(),
      FOBValue = numeric(),
      Qty = numeric(),
      QtyUnitCode = integer(),
      QtyKg = numeric()
    )
    
    # correspondence table for this HS combination
    corr_table <- correspondence_tables %>% 
      filter(first_name == paste0("HS ", hs_to), second_name == paste0("HS ", hs_from))
    
    # loop through each commodity code in the latest correspondence table
    done_codes <- c() # initialize list to keep track of codes already handled. This is because some codes, e.g. for 1:n and n:n correspondences, will show up in another codes' sections, so don't want to do them twice.
    
    counter <- 0
    for (new_code in unique(df$CommodityCode)) {
      if (!quiet) {
        print(paste0(counter, "/", length(unique(df$CommodityCode)))) 
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
          # grouped by Origin-Transit-Mot-QtyUnitCode, get the total value for all related old codes to distribute according to new data distribution
          tmp_old <- tmp_old[, .(CIFValue=sum(CIFValue, na.rm=TRUE), FOBValue=sum(FOBValue, na.rm=TRUE), Qty=sum(Qty, na.rm=TRUE), QtyKg=sum(QtyKg, na.rm=TRUE)), by=c("Origin", "Transit", "Mot", "QtyUnitCode")]
          
          # putting tmp_old (all related products, collapsing the commodity code column) in terms of %
          tmp_old_perc <- tmp_old %>%
            mutate(
              CIFValue = CIFValue / sum(CIFValue, na.rm=TRUE),
              FOBValue = FOBValue / sum(FOBValue, na.rm=TRUE),
              Qty = Qty / sum(Qty, na.rm=TRUE),
              QtyKg = QtyKg / sum(QtyKg, na.rm=TRUE)
            )
          
          # mapping commodity code distribution
          # only use the mapping dataframe if 1) exists 2) n:n or 1:n 3) mapping data exists for those codes
          use_map <- FALSE
          if (
            (length(map_df) > 1) &
            (length(all_related_new_codes) > 1)
          ) {
            if ((map_df %>% 
                 filter(CommodityCode %in% all_related_new_codes) %>% nrow) > 0) {
              use_map <- TRUE
            }
          }
          
          # if no all related new codes (i.e. probably 999999 1:1 correspondence not in the correspondence table), set to 99999
          if (length(all_related_new_codes) == 0) {
            all_related_new_codes <- "999999"
          }
          
          if (use_map) {
            tmp_map <- map_df %>% 
              filter(CommodityCode %in% all_related_new_codes) %>% 
              group_by(CommodityCode) %>% 
              summarise(
                CIFValue = sum(CIFValue, na.rm=TRUE),
                FOBValue = sum(FOBValue, na.rm=TRUE),
                Qty = sum(Qty, na.rm=TRUE),
                QtyKg = sum(QtyKg, na.rm=TRUE)
              )
            # if missing a quantity, put same distribution as CIF, FOB, or equal distribution
            if (sum(tmp_map$Qty, na.rm=TRUE) == 0) {
              if (sum(tmp_map$CIFValue, na.rm=TRUE) != 0) { # use CIFValue if there
                tmp_map$Qty <- tmp_map$CIFValue 
              } else if (sum(tmp_map$FOBValue, na.rm=TRUE) != 0) { # if not CIF, use FOBValue if there
                tmp_map$Qty <- tmp_map$FOBValue
              } else { # if neither, use 1 for equal distribution
                tmp_map$Qty <- 1
              }
            }
            # if missing a CIF, put same distribution as FOB, Qty, or equal distribution
            if (sum(tmp_map$CIFValue, na.rm=TRUE) == 0) {
              if (sum(tmp_map$FOBValue, na.rm=TRUE) != 0) { # use FOBValue if there
                tmp_map$CIFValue <- tmp_map$FOBValue 
              } else if (sum(tmp_map$Qty, na.rm=TRUE) != 0) { # if not FOB, use Qty if there
                tmp_map$CIFValue <- tmp_map$Qty
              } else { # if neither, use 1 for equal distribution
                tmp_map$CIFValue <- 1
              }
            }
            # if missing a FOB, put same distribution as CIF
            if (sum(tmp_map$FOBValue, na.rm=TRUE) == 0) {
              if (sum(tmp_map$CIFValue, na.rm=TRUE) != 0) { # use CIFValue if there
                tmp_map$FOBValue <- tmp_map$CIFValue 
              } else if (sum(tmp_map$Qty, na.rm=TRUE) != 0) { # if not CIF, use Qty if there
                tmp_map$FOBValue <- tmp_map$Qty
              } else { # if neither, use 1 for equal distribution
                tmp_map$FOBValue <- 1
              }
            }
            # if missing a QtyKg, put same distribution as Qty
            if (sum(tmp_map$QtyKg, na.rm=TRUE) == 0) {
              if (sum(tmp_map$Qty, na.rm=TRUE) != 0) { # use Qty if there
                tmp_map$QtyKg <- tmp_map$Qty 
              } else if (sum(tmp_map$CIFValue, na.rm=TRUE) != 0) { # use CIFValue if there
                tmp_map$QtyKg <- tmp_map$CIFValue 
              } else if (sum(tmp_map$FOBValue, na.rm=TRUE) != 0) { # if not CIFValue, use FOBValue if there
                tmp_map$QtyKg <- tmp_map$FOBValue
              } else { # if neither, use 1 for equal distribution
                tmp_map$QtyKg <- 1
              }
            }
          } else { # dummy equal distribution if no map_df provided
            tmp_map <- data.table(
              CommodityCode = all_related_new_codes,
              CIFValue = 1,
              FOBValue = 1,
              Qty = 1,
              QtyKg = 1
            )
          }
          
          # converting commodity code map into percentages
          tmp_map_perc <- tmp_map %>% 
            mutate(
              CIFValue = CIFValue / sum(CIFValue, na.rm=TRUE),
              FOBValue = FOBValue / sum(FOBValue, na.rm=TRUE),
              Qty = Qty / sum(Qty, na.rm=TRUE),
              QtyKg = QtyKg / sum(QtyKg, na.rm=TRUE)
            )
          
          # distributing percentages to create final harmonized data
          tmp_final_df <- tmp_old_perc %>% 
            mutate(by=1) %>% 
            full_join(tmp_map_perc %>% mutate(by=1), by="by") %>% 
            select(-by) %>% 
            mutate(
              CIFValue = CIFValue.x * CIFValue.y,
              FOBValue = FOBValue.x * FOBValue.y,
              Qty = Qty.x * Qty.y,
              QtyKg = QtyKg.x * QtyKg.y
            ) %>% 
            select(column_names)
          
          tmp_final_df <- tmp_final_df %>% 
            mutate(
              CIFValue = CIFValue * sum(tmp_old$CIFValue, na.rm=TRUE),
              FOBValue = FOBValue * sum(tmp_old$FOBValue, na.rm=TRUE),
              Qty = Qty * sum(tmp_old$Qty, na.rm=TRUE),
              QtyKg = QtyKg * sum(tmp_old$QtyKg, na.rm=TRUE)
            )
          
          final_df <- rbind(final_df, tmp_final_df)
        } 
      }
    }
  }
  return (final_df)
}
