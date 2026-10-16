# Post-Graduation Monthly Budget Calculator
# -----------------------------------------
# Install dependencies once:
#   install.packages(c("shiny", "ggplot2"))
# Run with: shiny::runApp("app.R")

library(shiny)
library(ggplot2)

# ---- Lookup table -----------------------------------------------------------
# Expected columns: school, program, degree_level, tuition_per_year,
# room_board_per_year, program_years, median_earnings, earnings_years_after_grad
# Swap in your real data (e.g., from the College Scorecard) with the same columns.
school_data <- if (file.exists("school_data")) {
  read.csv("school_data", stringsAsFactors = FALSE)
} else {
  data.frame(school = character(), program = character())
}

# ---- Helper functions -------------------------------------------------------

# Standard amortized monthly payment
monthly_payment <- function(principal, annual_rate, years) {
  n <- years * 12
  r <- annual_rate / 100 / 12
  if (n <= 0) return(0)
  if (r == 0) return(principal / n)
  principal * r / (1 - (1 + r)^(-n))
}

# Balance when repayment starts. If interest accrues during school and the
# grace period, it is added to the principal (capitalized) using simple interest.
starting_balance <- function(principal, annual_rate, accrue, years_in_school, grace_months) {
  if (!accrue) return(principal)
  years_accruing <- years_in_school + grace_months / 12
  principal * (1 + (annual_rate / 100) * years_accruing)
}

fmt_money <- function(x) paste0(ifelse(x < 0, "-$", "$"), formatC(abs(x), format = "f", digits = 0, big.mark = ","))