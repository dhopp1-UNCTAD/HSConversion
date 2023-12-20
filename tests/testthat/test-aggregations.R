library(dplyr)
library(readr)

# test dataframe and parameters
sample_data <- data.frame(
  Year = c(2017,2017,2017,2018,2018,2017,2017),
  FlowCode = c(1,2,2,2,2,2,2),
  ReporterCode = rep("528", 7),
  ReporterLabel = rep("Netherlands", 7),
  PartnerCode = rep("608", 7),
  PartnerLabel = rep("Philippines", 7),
  CommodityCode = c("100890", "540253", "540259", "540253", "540259", "847340", "854231"),
  Value = c(100, 10, 15, 1, 19, 40, 60),
  CIF = c(101, 11, 16, 2, 21, 42, 66)
)

agg_columns <- c("Value", "CIF")
group_columns <- c("FlowCode", "ReporterCode", "ReporterLabel", "PartnerCode", "PartnerLabel", "CommodityCode")
commodity_column <- "CommodityCode"

# test country aggregation
result <- aggregate_country_data(data = sample_data, agg_columns = agg_columns, group_columns = group_columns)

test_that(str_interp("Aggregating country data"), {
  expect_equal(
    nrow(result),
    5
  )
})

test_that(str_interp("Aggregating country data"), {
  expect_equal(
    result %>% filter(CommodityCode == 540253) %>% select(CIF) %>% pull(),
    13
  )
})

# test 4-digit aggregation
result <- aggregate_digit_level(sample_data, agg_columns = agg_columns, group_columns = group_columns, commodity_column = commodity_column, digit_level = 4)
test_that(str_interp("Aggregating to 4-digit"), {
  expect_equal(
    result %>% filter(CommodityCode == "5402") %>% select(CIF) %>% pull(),
    50
  )
})

# test 2-digit aggregation
result <- aggregate_digit_level(sample_data, agg_columns = agg_columns, group_columns = group_columns, commodity_column = commodity_column, digit_level = 2)
test_that(str_interp("Aggregating to 2-digit"), {
  expect_equal(
    result %>% filter(CommodityCode == "54") %>% select(CIF) %>% pull(),
    50
  )
})