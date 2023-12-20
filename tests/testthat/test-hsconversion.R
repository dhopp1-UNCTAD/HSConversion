library(dplyr)
library(readr)
#source("../../R/aggregate.r")
#library(data.table)

# correspondence tables
correspondence_tables_6 <- read_csv("../correspondence_tables_6.csv", show_col_types = FALSE)
correspondence_tables_4 <- read_csv("../correspondence_tables_4.csv", show_col_types = FALSE)
correspondence_tables_2 <- read_csv("../correspondence_tables_2.csv", show_col_types = FALSE)

# test cases
# HS2017  HS2012	
# 100890	100890	1:1
# 540253	540259	n:1
# 540259	540259	n:1
# 370500	370510	1:n
# 370500	370590	1:n
# 852862	852869	n:n (changing code)
# 847340	847310	n:n
# 847340	847340	n:n
# 854231	847340	n:n

# test dataframes
hs17_map <- data.frame(
  Year = c(2017,2017,2017,2018,2018,2017,2017),
  FlowCode = c(1,2,2,2,2,2,2),
  ReporterCode = rep("528", 7),
  ReporterLabel = rep("Netherlands", 7),
  PartnerCode = rep("608", 7),
  PartnerLabel = rep("Philippines", 7),
  CommodityCode = c("100890", "540253", "540259", "540253", "540259", "847340", "854231"),
  Value = c(100, 10, 15, 1, 19, 40, 60),
  CIF = c(101, 11, 16, 2, 21, 42, 66),
  Qty = c(NA, NA, NA, NA, NA, NA, NA)
)

hs12_to_convert <- data.frame(
  Year = rep(2012, 7),
  FlowCode = c(1,2,1,1,2,2,2),
  ReporterCode = rep("528", 7),
  ReporterLabel = rep("Netherlands", 7),
  PartnerCode = rep("608", 7),
  PartnerLabel = rep("Philippines", 7),
  CommodityCode = c("100890", "540259", "370510", "370590", "852869", "847310", "847340"),
  Value = c(100, 20, 5, 7, 50, 10, 50),
  CIF = c(101, 21, 6, 9, 52, 13, 55),
  Qty = c(1, 2, 4, 5, 1, 9, 10)
)

agg_columns <- c("Value", "CIF", "Qty")
group_columns <- c("Year", "FlowCode", "ReporterCode", "ReporterLabel", "PartnerCode", "PartnerLabel", "CommodityCode")
commodity_column <- "CommodityCode"

### 6-digit conversions
# forward conversions
forward_country_aggregation <- aggregate_country_data(hs17_map, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"])
hs17_conversion <- convert_hs(
  correspondence_tables = correspondence_tables_6,
  hs_from = 2012,
  hs_to = 2017,
  df = hs12_to_convert,
  agg_columns = agg_columns,
  group_columns = group_columns,
  commodity_column = commodity_column,
  aggregate_order = c("Value", "CIF", "Qty"),
  map_df = forward_country_aggregation,
  quiet = TRUE
)

hs17_conversion_no_map <- convert_hs(
  correspondence_tables = correspondence_tables_6,
  hs_from = 2012,
  hs_to = 2017,
  df = hs12_to_convert,
  agg_columns = agg_columns,
  group_columns = group_columns,
  commodity_column = commodity_column,
  aggregate_order = c("Value", "CIF", "Qty"),
  map_df = NA,
  quiet = TRUE
)

# backward conversions
backward_country_aggregation <- aggregate_country_data(hs12_to_convert, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"])
hs12_conversion <- convert_hs(
  correspondence_tables = correspondence_tables_6,
  hs_from = 2017,
  hs_to = 2012,
  df = hs17_map,
  agg_columns = agg_columns,
  group_columns = group_columns,
  commodity_column = commodity_column,
  aggregate_order = c("Value", "CIF", "Qty"),
  map_df = backward_country_aggregation,
  quiet = TRUE
)

hs12_conversion_no_map <- convert_hs(
  correspondence_tables = correspondence_tables_6,
  hs_from = 2017,
  hs_to = 2012,
  df = hs17_map,
  agg_columns = agg_columns,
  group_columns = group_columns,
  commodity_column = commodity_column,
  aggregate_order = c("Value", "CIF", "Qty"),
  map_df = NA,
  quiet = TRUE
)

### 4-digit conversions
hs12_4 <- aggregate_digit_level(df = hs12_to_convert, agg_columns = agg_columns, group_columns = group_columns, commodity_column = commodity_column, digit_level = 4)
hs17_map_4 <- aggregate_digit_level(df = hs17_map, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"], commodity_column = commodity_column, digit_level = 4)
country_aggregation_4 <- aggregate_country_data(hs17_map_4, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"])
hs12_conversion_4 <- convert_hs(
  correspondence_tables = correspondence_tables_4,
  hs_from = 2012,
  hs_to = 2017,
  df = hs12_4,
  agg_columns = agg_columns,
  group_columns = group_columns,
  commodity_column = commodity_column,
  aggregate_order = c("Value", "CIF", "Qty"),
  map_df = country_aggregation_4,
  quiet = TRUE
)

### 2-digit conversions
  hs12_2 <- aggregate_digit_level(df = hs12_to_convert, agg_columns = agg_columns, group_columns = group_columns, commodity_column = commodity_column, digit_level = 2)
  hs17_map_2 <- aggregate_digit_level(df = hs17_map, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"], commodity_column = commodity_column, digit_level = 2)
  country_aggregation_2 <- aggregate_country_data(hs17_map_2, agg_columns = agg_columns, group_columns = group_columns[group_columns != "Year"])
  hs12_conversion_2 <- convert_hs(
    correspondence_tables = correspondence_tables_2,
    hs_from = 2012,
    hs_to = 2017,
    df = hs12_2,
    agg_columns = agg_columns,
    group_columns = group_columns,
    commodity_column = commodity_column,
    aggregate_order = c("Value", "CIF", "Qty"),
    map_df = country_aggregation_2,
    quiet = TRUE
  )
  
### 6-digit tests
# HS 2012 to HS 2017 tests
test_that("6-digit HS 2012 -> HS 2017 1:1 test", {
  expect_equal(
    hs17_conversion[(hs17_conversion$CommodityCode == "100890") & (hs17_conversion$PartnerLabel == "Philippines"), "Value"] %>% pull(),
    100
  )
})

test_that("6-digit HS 2012 -> HS 2017 1:n test", {
  expect_equal(
    hs17_conversion[(hs17_conversion$CommodityCode == "370500") & (hs17_conversion$PartnerLabel == "Philippines"), "Value"] %>% pull(),
    12
  )
})

if (FALSE) {
  
  # HS 2012 to HS 2017 tests
  test_that("HS 2012 -> HS 2017 1:n test", {
    expect_equal(
      hs17_conversion[(hs17_conversion$commodity_code == "370500") & (hs17_conversion$partner == "Philippines"), "value"],
      12
    )
  })
  
  test_that("HS 2012 -> HS 2017 n:1 test 1", {
    expect_equal(
      round(hs17_conversion[(hs17_conversion$commodity_code == "540253") & (hs17_conversion$partner == "Philippines"), "value"], 0),
      5
    )
  })
  
  test_that("HS 2012 -> HS 2017 n:1 test 2", {
    expect_equal(
      round(hs17_conversion[(hs17_conversion$commodity_code == "540259") & (hs17_conversion$partner == "Philippines"), "value"], 0),
      15
    )
  })
  
  test_that("HS 2012 -> HS 2017 n:n test 1", {
    expect_equal(
      round(hs17_conversion[(hs17_conversion$commodity_code == "847340") & (hs17_conversion$partner == "Philippines"), "value"], 0),
      24
    )
  })
  
  test_that("HS 2012 -> HS 2017 n:n test 2", {
    expect_equal(
      round(hs17_conversion[(hs17_conversion$commodity_code == "854231") & (hs17_conversion$partner == "Philippines"), "value"], 0),
      36
    )
  })
  
  test_that("HS 2012 -> HS 2017 no map_df", {
    expect_equal(
      nrow(hs17_conversion_no_map),
      362 # many rows because without a map, 847310 gets distributed to many different products
    )
  })
  
  test_that("HS 2012 -> HS 2017 final sum is equal", {
    expect_equal(
      round(hs17_conversion_no_map %>% filter(partner != "World") %>% select(value) %>% sum(), 0),
      hs12_to_convert %>% select(Value) %>% sum()
    )
  })
  
  # Backwards, HS 2017 to HS 2012 conversion
  test_that("HS 2017 -> HS 2012 n:n test", {
    expect_equal(
      round(hs12_conversion[(hs12_conversion$commodity_code == "847340") & (hs12_conversion$partner == "Philippines"), "value"], 0),
      83
    )
  })
  
  test_that("HS 2017 -> HS 2012 final sum is equal", {
    expect_equal(
      round(hs12_conversion %>% filter(partner != "World") %>% select(value) %>% sum(), 0),
      (hs17_map %>% filter(Year == 2017)) %>% select(Value) %>% sum()
    )
  })
} 