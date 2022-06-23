#' @import dplyr stringr
#' @title Convert Comtrade data from one HS year to another
#' @name convert_hs
#' @description Convert Comtrade data from one HS year to another. Will additionally add a "World" aggregate. Note that the function may take some time to run even on small datasets due to the complex logic necessary for n:n transformations.
#' @param correspondence_table Dataframe with HS mappings, obtained from the \code{get_correspondence_tables} function.
#' @param hs_from Integer of which HS year the original data is in (e.g., '2017').
#' @param hs_to Integer of which HS year the data should be converted to (e.g., '2017').
#' @param df The original dataframe to convert with columns:
#' 
#' \itemize{
#' \item{Year}{the year of the trade data}
#' \item{FlowCode}{the HS version of the data}
#' \item{ReporterCode}{code of the trade flow (export, import, etc.)}
#' \item{ReporterLabel}{the name of the reporter country}
#' \item{PartnerCode}{the code of the partner country}
#' \item{PartnerLabel}{the name of the partner country}
#' \item{CommodityCode}{the commodity code}
#' \item{Value}{the value of recorded trade}}
#' 
#' @param map_df A dataframe of the country in the desired HS year to use for 1->n mappings. If omitted, will use equal distribution for 1-n mappings. Same columns as \code{df} parameter dataframe.
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
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 100)
  
  # if no map df is provided, create a dummy dataframe. NA = length 1, otherwise should be 8 for the 8 columns
  if (length(map_df) == 1) { 
    map_df <- df %>% 
      slice(1) %>% 
      mutate(commodity_code = 999999)
  }
  
  # make sure commodity code is a character, since can lead with 0s
  df$CommodityCode <- as.character(df$CommodityCode)
  map_df$CommodityCode <- as.character(map_df$CommodityCode)
  
  # column names of raw data from Comtrade/Yoann
  orig_names <- c("Year", "FlowCode", "ReporterCode", "ReporterLabel", "PartnerCode", "PartnerLabel", "CommodityCode", "Value")
  # preferred column names of the processed data
  renames <- c("year", "flow_code", "reporter_code", "reporter", "partner_code", "partner", "commodity_code", "value")
  
  # renaming columns
  map_df <- map_df %>% 
    select(orig_names) %>% 
    mutate(Year=data.frame(df)[1,"Year"]) %>% # set year to year of old data, as this will be the data joined on later
    filter(nchar(CommodityCode)==6 | CommodityCode=="TOTAL") %>%  # only keep full codes
    mutate(ReporterCode = str_pad(as.character(ReporterCode), 3, side="left", pad="0")) %>% # pad country codes to three digits
    mutate(PartnerCode = str_pad(as.character(PartnerCode), 3, side="left", pad="0"))
  colnames(map_df) <- renames
  
  df <- df %>%
    select(orig_names) %>% 
    filter(nchar(CommodityCode)==6 | CommodityCode=="TOTAL") %>% # only keep full codes
    mutate(ReporterCode = str_pad(as.character(ReporterCode), 3, side="left", pad="0")) %>%  # pad country codes to three digits
    mutate(PartnerCode = str_pad(as.character(PartnerCode), 3, side="left", pad="0")) # pad country codes to three digits
  colnames(df) <- renames
  
  # converting value to numeric just in case
  map_df <- map_df %>% 
    mutate(value = as.numeric(value))
  df <- df %>% 
    mutate(value = as.numeric(value))
  
  # converting dataframes to datatables for faster processing/filtering later on
  map_df <- data.table(map_df)
  df <- data.table(df)
  
  # if data already in latest_hs, return the data with renamed columns
  if (hs_from == hs_to) {
    final_df <- df
  } else { # else calculate via proportions the old HS data in terms of new HS codes
    # add all possible partner flow products from products to latest, fill with 0.01, so that products that are in old but not new will be retained
    # get all combination of products and partners for the case where combination doesn't exist in new data
    all_codes <- correspondence_table %>% 
      filter(first_name==paste0("HS ", hs_to), second_name==paste0("HS ", hs_from)) %>% 
      select(first) %>% 
      unique %>% 
      pull
    all_partners <- df$partner_code %>% 
      unique
    all_flows <- c(1, 2)
    
    # initialize a dataframe that will have every partner-code-flow combination
    df_complete <- data.table(year=numeric(), flow_code=integer(), reporter_code=character(), reporter=character(), partner_code=character(), partner=character(), commodity_code=character(), value=numeric()) 
    tmp_cmp <- expand.grid(all_codes, all_partners, all_flows)
    df_complete <- df_complete[1:nrow(tmp_cmp),]
    df_complete$commodity_code <- tmp_cmp$Var1
    df_complete$partner_code <- tmp_cmp$Var2
    df_complete$flow_code <- tmp_cmp$Var3
    df_complete$year <- df[1,"year"] %>% pull
    df_complete$reporter_code <- df[1,"reporter_code"] %>% pull
    df_complete$reporter <- df[1,"reporter"] %>% pull
    
    # initialize all with 1e-6 for even distribution if not in old data, will be replaced with actual ratios from new data if available
    df_complete$value <- 1e-6
    
    # adding partner name from code
    df_complete <- df_complete %>% 
      left_join(unique(df[,c("partner", "partner_code")]), by="partner_code")
    df_complete <- df_complete %>% 
      mutate(partner.x=partner.y) %>% 
      select(-partner.y) %>% 
      rename(partner=partner.x)
    
    # replace 1e-6's with real data when available
    x <- data.table(df_complete)
    y <- data.table(map_df)
    setkey(x, year, flow_code, reporter_code, reporter, partner_code, partner, commodity_code)
    setkey(y, year, flow_code, reporter_code, reporter, partner_code, partner, commodity_code)
    
    new <- merge(x, y, all.x=TRUE) %>% data.frame
    df_complete <- transform(new, value = pmax(value.x, value.y, na.rm=T)) # keep the real value if available, if not the synthetic 0.1 value
    df_complete <- df_complete %>% 
      select(-value.x, -value.y) %>% 
      data.table
    
    # initializing final dataframe
    final_df <- data.frame(year=numeric(), flow_code=character(), reporter_code=character(), reporter=character(), partner_code=character(), partner=character(), commodity_code=character(), value=numeric())
    # correspondence table for this HS combination
    corr_table <- correspondence_table %>% 
      filter(first_name == paste0("HS ", hs_to), second_name == paste0("HS ", hs_from))
    # loop through each commodity code in the latest correspondence table
    done_codes <- c() # initialize list to keep track of codes already handled. This is because some codes, e.g. for 1:n and n:n correspondences, will show up in another codes' sections, so don't want to do them twice.
    
    for (new_code in unique(corr_table$first)) {
      # skip the code if it was already handled in another section
      if (!(new_code %in% done_codes)) {
        tmp_corr <- corr_table %>% filter(first==new_code)
        all_related_new_codes <- c(new_code)
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
        done_codes <- append(done_codes, all_related_new_codes) %>% unique
        
        # only do calculations and append if there are any of that code the data
        tmp_old <- df[commodity_code %in% all_related_old_codes,]
        if (nrow(tmp_old) > 0) {
          # grouped by partner-flow, get the total value for all related old codes to distribute according to new data distribution
          tmp_old <- tmp_old[, .(value=sum(value)), by=c("year", "flow_code", "reporter_code", "reporter", "partner_code", "partner")]
          # grouped by partner-flow, get total value and proportion by code for all related new codes in the new data
          tmp_latest <- df_complete[commodity_code %in% all_related_new_codes,]
          # totals to get the percent/ratio
          totals <- tmp_latest[, .(total=sum(value)), by=c("year", "flow_code", "reporter_code", "reporter", "partner_code", "partner")]
          tmp_latest <- tmp_latest %>%
            left_join(totals, by=c("year", "flow_code", "reporter_code", "reporter", "partner_code", "partner")) %>%
            mutate(perc=value/total)
          # apply this proportion to the sum of the group in the old codes to get the new latest HS distribution for the older data
          tmp_final_df <- tmp_latest %>%
            left_join(tmp_old[,c("partner", "flow_code", "value")], by=c("partner", "flow_code")) %>%
            mutate(value=perc*value.y) %>%
            ungroup() %>%
            filter(!is.na(value), value > 1e-5) %>% # also filter out 0.01 rows
            select(year, flow_code, reporter_code, reporter, partner_code, partner, commodity_code, value)
          
          # have to redistribute World values and calculate from constituents, not take from ratios from latest HS data
          new_world <- tmp_final_df %>%
            filter(partner != "World") %>%
            mutate(partner_code = "000", partner = "World") %>% # changing so 
            group_by(year, flow_code, reporter_code, reporter, partner_code, partner, commodity_code) %>%
            summarise(value = sum(value)) %>%
            ungroup()
          # replacing incorrectly distributed world figures with correctly distributed
          tmp_final_df <- tmp_final_df %>%
            filter(partner != "World") %>%
            rbind(new_world)
          
          final_df <- rbind(final_df, tmp_final_df)
        } 
      }
    }
  }
  final_df <- unique(final_df) # get rid of any duplicated rows
  final_df <- final_df %>% 
    mutate(flow = ifelse(flow_code == 1, "imports", ifelse(flow_code == 2, "exports", NA))) %>% 
    mutate(hs_version = paste0("HS ", hs_to)) %>% 
    select(hs_version, year, flow_code, flow, reporter_code, reporter, partner_code, partner, commodity_code, value)
  
  # exclude any values < 1, this is from the equal distribution step
  final_df <- final_df %>% 
    filter(value >= 1e-5)
  return (data.frame(final_df))
}