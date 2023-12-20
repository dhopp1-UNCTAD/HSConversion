library(readr)
library(stringr)

corr_table_colnames <- c("first", "second", "first_name", "second_name", "relationship")

# generic function for testing a correspondence table
test_correspondence_table <- function (digit_level) {
  if (!file.exists(str_interp("../correspondence_tables_${digit_level}.csv"))) {
    correspondence_tables <- get_correspondence_tables(digit_level = digit_level)
    correspondence_tables %>% 
      write_csv(str_interp("../correspondence_tables_${digit_level}.csv"))
  } else {
    correspondence_tables <- read_csv(str_interp("../correspondence_tables_${digit_level}.csv"), show_col_types = FALSE)
  }
  test_that(str_interp("Getting correspondence tables ${digit_level}-digit level"), {
    expect_equal(
      all(colnames(correspondence_tables) == corr_table_colnames),
      TRUE
    )
  })  
}

# test getting 6-digit correspondence table
test_correspondence_table(digit_level = 6)

# test getting 4-digit correspondence table
test_correspondence_table(digit_level = 4)

# test getting 2-digit correspondence table
test_correspondence_table(digit_level = 2)
