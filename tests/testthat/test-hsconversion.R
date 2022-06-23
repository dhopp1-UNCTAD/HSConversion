# correspondence tables
correspondence_tables <- read_csv("../../data/correspondence_tables.csv")

# HS2017 to HS2012 conversion test dataframes
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

hs17_map <- data.frame(
  Year = c(2017,2017,2017,2018,2018,2017,2017),
  FlowCode = c(1,2,2,2,2,2,2),
  ReporterCode = rep("528", 7),
  ReporterLabel = rep("Netherlands", 7),
  PartnerCode = rep("608", 7),
  PartnerLabel = rep("Philippines", 7),
  CommodityCode = c("100890", "540253", "540259", "540253", "540259", "847340", "854231"),
  Value = c(100, 10, 15, 1, 19, 40, 60)
)

hs12_to_convert <- data.frame(
  Year = rep(2012, 7),
  FlowCode = c(1,2,1,1,2,2,2),
  ReporterCode = rep("528", 7),
  ReporterLabel = rep("Netherlands", 7),
  PartnerCode = rep("608", 7),
  PartnerLabel = rep("Philippines", 7),
  CommodityCode = c("100890", "540259", "370510", "370590", "852869", "847310", "847340"),
  Value = c(100, 20, 5, 7, 50, 10, 50)
)

country_aggregation <- aggregate_country_data(hs17_map)
hs17_conversion <- convert_hs(correspondence_table = correspondence_tables, hs_from = 2012, hs_to = 2017, df = hs12_to_convert, map_df = country_aggregation)

# country aggregation test
test_that("Country aggregation for mapping", {
  expect_equal(
    country_aggregation[country_aggregation$CommodityCode == "540253", "Value"],
    11
  )
})

# HS 2012 to HS 2017 tests
test_that("HS 2012 -> HS 2017 1:1 test", {
  expect_equal(
    hs17_conversion[(hs17_conversion$commodity_code == "100890") & (hs17_conversion$partner != "World"), "value"],
    100
  )
})

test_that("HS 2012 -> HS 2017 1:n test", {
  expect_equal(
    hs17_conversion[(hs17_conversion$commodity_code == "370500") & (hs17_conversion$partner != "World"), "value"],
    12
  )
})

#test_that("HS 2012 -> HS 2017 n:1 test 1", {
#  expect_equal(
#    round(hs17_conversion[(hs17_conversion$commodity_code == "540253") & (hs17_conversion$partner != "World"), "value"], 0),
#    5
#  )
#})

#test_that("HS 2012 -> HS 2017 n:1 test 2", {
#  expect_equal(
#    round(hs17_conversion[(hs17_conversion$commodity_code == "540259") & (hs17_conversion$partner != "World"), "value"], 0),
#    15
#  )
#})

#test_that("HS 2012 -> HS 2017 n:n test 1", {
#  expect_equal(
#    hs17_conversion[(hs17_conversion$commodity_code == "847340") & (hs17_conversion$partner != "World"), "value"],
#    24
#  )
#})

#test_that("HS 2012 -> HS 2017 n:n test 2", {
#  expect_equal(
#    hs17_conversion[(hs17_conversion$commodity_code == "854231") & (hs17_conversion$partner != "World"), "value"],
#    36
#  )
#})
