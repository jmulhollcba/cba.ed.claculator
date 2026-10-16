# Post-Graduation Monthly Budget Calculator
# -----------------------------------------
# Install dependencies once:
#   install.packages(c("shiny", "ggplot2"))
# Run with: shiny::runApp("app.R")

library(shiny)
library(ggplot2)

# ---- Lookup table -----------------------------------------------------------
# Make sure to load College Scorecard data into Environment with column/var names the same as UI and server pulls
school_data <- read.csv("school_data.csv", stringsAsFactors = FALSE)

# Earnings column used for the income slider and RAP AGI.
# Test data uses "median_earnings"; switch to "EARN_MDN_4YR" when the real data is loaded.
EARN_COL <- "median_earnings"

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

# RAP monthly payment (whole AGI x rate that rises 1 point per $10,000 band:
# $10,001-20,000 = 1% ... over $100,000 = 10%). AGI <= $10,000 pays $10/month.
# Annual amount / 12, minus $50 per dependent, $10/month minimum.
rap_payment <- function(agi, dependents = 0) {
  agi <- max(agi, 0)
  if (agi <= 10000) return(10)
  pct <- min(ceiling(agi / 10000) - 1, 10)
  max(10, agi * pct / 100 / 12 - 50 * dependents)
}

# Month-by-month balance for one RAP loan (30-year cap).
# Interest not covered by the payment is waived (balance never grows), and the
# principal match raises the principal reduction to at least `min_prin`.
rap_schedule <- function(start, annual_rate, payment, min_prin, max_months = 360) {
  r <- annual_rate / 100 / 12
  bal <- numeric(max_months + 1)
  paid <- numeric(max_months)
  bal[1] <- start
  for (m in seq_len(max_months)) {
    b <- bal[m]
    if (b <= 0.005) break
    interest <- b * r
    if (payment >= b + interest) {
      paid[m] <- b + interest
      bal[m + 1] <- 0
      next
    }
    principal <- max(payment - interest, min_prin, 0)
    bal[m + 1] <- max(b - principal, 0)
    paid[m] <- payment
  }
  list(bal = bal, paid = paid, n_paid = sum(paid > 0))
}

fmt_money <- function(x) paste0(ifelse(x < 0, "-$", "$"), formatC(abs(x), format = "f", digits = 0, big.mark = ","))
allschools<-sort(unique(school_data$school))
# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Post-Graduation Monthly Budget Calculator"),
  tags$head(tags$style(HTML("
    .metric {padding:14px; border-radius:8px; background:#f4f6f8; margin-bottom:12px; text-align:center;}
    .metric h3 {margin:0; font-size:26px;}
    .metric p {margin:0; color:#555;}
    .good h3 {color:#1b7f3b;} .bad h3 {color:#b3261e;}
  "))),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("0. School & Program (optional)"),
      selectInput("school", 
                  "School",
                  choices = c("Enter values manually" = "", sort(unique(school_data$school)))),
      selectInput("program", 
                  "Program", 
                  choices = c("Select a school first" = "",school_data$program)),
      uiOutput("lookup_note"),
      
      hr(),
      h4("1. Income"),
      sliderInput("salary", "Expected annual gross salary ($)", 0, 500000, 75000, step = 5000),
      sliderInput("tax_rate", "Estimated total tax rate (federal + state + FICA, %)", 0, 45, 22, step = 0.5),
      sliderInput("retirement", "Pre-tax retirement contribution (% of gross)", 0, 20, 3, step = 0.5),
      
      hr(),
      h4("2. School Costs"),
      numericInput("cost_per_year", "Total cost of attendance per year ($)", 30000, min = 0, step = 500),
      numericInput("years_school", "Years in school", 4, min = 1, max = 10, step = 0.5),
      numericInput("grants", "Total scholarships & grants ($)", 20000, min = 0, step = 500),
      numericInput("savings", "Savings / family contributions / work earnings ($)", 10000, min = 0, step = 500),
      
      hr(),
      h4("3. Loans"),
      numericInput("n_loans", "Number of loans", 2, min = 0, max = 6, step = 1),
      checkboxInput("accrue", "Interest accrues while in school (unsubsidized / private)", TRUE),
      numericInput("grace", "Grace period after graduation (months)", 6, min = 0, max = 24, step = 1),
      uiOutput("loan_inputs"),
      
      hr(),
      h4("4. Monthly Expenses"),
      numericInput("rent", "Rent ($)", 1500, min = 0, step = 25),
      numericInput("utilities", "Utilities & internet ($)", 150, min = 0, step = 10),
      numericInput("groceries", "Groceries ($)", 400, min = 0, step = 10),
      numericInput("transport", "Transportation (car, gas, transit) ($)", 250, min = 0, step = 10),
      numericInput("insurance", "Health / renter's / car insurance ($)", 200, min = 0, step = 10),
      numericInput("phone", "Phone ($)", 60, min = 0, step = 5),
      numericInput("subs", "Subscriptions & entertainment ($)", 100, min = 0, step = 5),
      numericInput("dining", "Dining out & misc. ($)", 250, min = 0, step = 10),
      numericInput("other", "Other ($)", 100, min = 0, step = 10)
    ),
    
    mainPanel(
      width = 8,
      fluidRow(
        column(4, uiOutput("box_net")),
        column(4, uiOutput("box_loans")),
        column(4, uiOutput("box_left"))
      ),
      uiOutput("funding_msg"),
      h4("Where your monthly income goes"),
      plotOutput("budget_plot", height = "320px"),
      h4("Monthly summary"),
      tableOutput("summary_table"),
      h4("Loan details"),
      tableOutput("loan_table"),
      h4("Total balance over time"),
      plotOutput("balance_plot", height = "300px")
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- School / program lookup ---
  
  # When the school changes, update the program dropdown
  observeEvent(input$school, {
    progs <- sort(school_data$program[school_data$school == input$school])
    label <- if (length(progs) == 0) "Select a school first" else "Select a program"
    updateSelectInput(session, "program", choices = c(setNames("", label), progs))
  }, ignoreInit = TRUE)
  
  # The matching row of the lookup table (only when both are chosen)
  selected_row <- reactive({
    req(input$school, input$program)
    rows <- school_data[school_data$school == input$school &
                          school_data$program == input$program, ]
    req(nrow(rows) > 0)
    rows[1, ]
  })
  
  # AGI for RAP: earnings for the selected school/program, otherwise the salary slider
  rap_agi <- reactive({
    if (isTruthy(input$school) && isTruthy(input$program)) {
      rows <- school_data[school_data$school == input$school &
                            school_data$program == input$program, ]
      v <- suppressWarnings(as.numeric(rows[[EARN_COL]][1]))
      if (length(v) == 1 && !is.na(v)) return(v)
    }
    input$salary
  })
  
  # Populate the manual inputs with the looked-up values.
  # The user can still edit them afterward.
  observeEvent(selected_row(), {
    r <- selected_row()
    earn <- suppressWarnings(as.numeric(r[[EARN_COL]]))
    if (length(earn) == 1 && !is.na(earn)) {
      updateSliderInput(session, "salary", value = earn)
    }
    updateNumericInput(session, "cost_per_year",
                       value = r$tuition_per_year + r$room_board_per_year)
    updateNumericInput(session, "years_school", value = r$program_years)
  })
  
  output$lookup_note <- renderUI({
    r <- selected_row()
    div(style = "font-size:12px; color:#666; margin-bottom:8px;",
        paste0("Pre-filled from lookup table: ", r$degree_level, ", median earnings ",
               r$earnings_years_after_grad, " yr after graduation. ",
               "You can edit any value below."))
  })
  
  # Dynamic loan inputs
  output$loan_inputs <- renderUI({
    n <- input$n_loans
    req(n)
    if (n < 1) return(NULL)
    defaults_amt  <- c(5500, 5000, 0, 0, 0, 0)
    defaults_rate <- c(6.5, 7.5, 6.5, 6.5, 6.5, 6.5)
    lapply(seq_len(n), function(i) {
      type_id <- paste0("loan_type_", i)
      plan_id <- paste0("loan_plan_", i)
      wellPanel(
        strong(paste("Loan", i)),
        selectInput(type_id, "Loan type", choices = c("Federal", "Private")),
        conditionalPanel(
          condition = sprintf("input.%s == 'Federal'", type_id),
          selectInput(plan_id, "Repayment plan",
                      choices = c("Standard" = "Standard", "RAP" = "RAP"))
        ),
        numericInput(paste0("loan_amt_", i), "Amount borrowed ($)", defaults_amt[i], min = 0, step = 500),
        numericInput(paste0("loan_rate_", i), "Annual interest rate (%)", defaults_rate[i], min = 0, max = 30, step = 0.1),
        # RAP has no fixed term (30-year cap), so hide the term for RAP loans
        conditionalPanel(
          condition = sprintf("!(input.%s == 'Federal' && input.%s == 'RAP')", type_id, plan_id),
          numericInput(paste0("loan_term_", i), "Repayment term (years)", 10, min = 1, max = 30, step = 1)
        )
      )
    })
  })
  
  # Loan table
  loans <- reactive({
    n <- input$n_loans
    req(n)
    if (n < 1) {
      return(data.frame(Loan = character(), Plan = character(), Borrowed = numeric(),
                        Rate = numeric(), Term = numeric(), StartBal = numeric(),
                        Payment = numeric(), MinPrin = numeric(), TotalInterest = numeric()))
    }
    rows <- lapply(seq_len(n), function(i) {
      type <- input[[paste0("loan_type_", i)]]
      amt  <- input[[paste0("loan_amt_", i)]]
      rate <- input[[paste0("loan_rate_", i)]]
      term <- input[[paste0("loan_term_", i)]]
      req(type, amt, rate, term)
      plan <- if (type == "Federal") input[[paste0("loan_plan_", i)]] else "Private"
      if (is.null(plan)) plan <- "Standard"
      start <- starting_balance(amt, rate, input$accrue, input$years_school, input$grace)
      pay   <- monthly_payment(start, rate, term)
      data.frame(
        Loan = paste("Loan", i),
        Plan = plan,
        Borrowed = amt,
        Rate = rate,
        Term = term,
        StartBal = start,
        Payment = pay,
        MinPrin = 0,
        TotalInterest = pay * term * 12 - amt  # includes interest accrued in school
      )
    })
    out <- do.call(rbind, rows)
    
    # RAP loans: one RAP payment (no dependents, AGI = earnings) covers all RAP
    # loans, split by starting balance.
    is_rap <- out$Plan == "RAP"
    if (any(is_rap)) {
      total_pay <- rap_payment(rap_agi(), dependents = 0)
      rap_total_bal <- sum(out$StartBal[is_rap])
      for (i in which(is_rap)) {
        share <- if (rap_total_bal > 0) out$StartBal[i] / rap_total_bal else 0
        sch <- rap_schedule(out$StartBal[i], out$Rate[i], total_pay * share, min(50, total_pay) * share)
        out$Payment[i] <- total_pay * share
        out$MinPrin[i] <- min(50, total_pay) * share
        out$Term[i] <- as.numeric(sch$n_paid) / 12
        out$TotalInterest[i] <- sum(sch$paid) - out$Borrowed[i]
      }
    }
    out
  })
  
  # Core budget calculations
  budget <- reactive({
    gross_m      <- input$salary / 12
    retirement_m <- gross_m * input$retirement / 100
    taxable_m    <- gross_m - retirement_m
    tax_m        <- taxable_m * input$tax_rate / 100
    net_m        <- taxable_m - tax_m
    loan_pay_m   <- sum(loans()$Payment)
    expenses     <- c(
      Rent = input$rent, Utilities = input$utilities, Groceries = input$groceries,
      Transportation = input$transport, Insurance = input$insurance, Phone = input$phone,
      Subscriptions = input$subs, Dining = input$dining, Other = input$other
    )
    expenses_m <- sum(expenses)
    list(
      gross = gross_m, retirement = retirement_m, tax = tax_m, net = net_m,
      loan_pay = loan_pay_m, expenses = expenses, expenses_total = expenses_m,
      left = net_m - loan_pay_m - expenses_m
    )
  })
  
  # Metric boxes
  output$box_net <- renderUI({
    div(class = "metric", h3(fmt_money(budget()$net)), p("Monthly take-home pay"))
  })
  output$box_loans <- renderUI({
    div(class = "metric", h3(fmt_money(budget()$loan_pay)), p("Total monthly loan payment"))
  })
  output$box_left <- renderUI({
    b <- budget()
    div(class = paste("metric", ifelse(b$left >= 0, "good", "bad")),
        h3(fmt_money(b$left)), p("Left over each month"))
  })
  
  # Funding check
  output$funding_msg <- renderUI({
    total_cost <- input$cost_per_year * input$years_school
    need <- total_cost - input$grants - input$savings
    borrowed <- sum(loans()$Borrowed)
    gap <- need - borrowed
    msg <- paste0(
      "Total cost: ", fmt_money(total_cost),
      " | After grants & savings, you need to cover: ", fmt_money(max(need, 0)),
      " | Borrowed: ", fmt_money(borrowed)
    )
    warn <- if (gap > 1) {
      paste0(" \u26A0 You have a ", fmt_money(gap), " funding gap (unaccounted-for costs).")
    } else if (gap < -1) {
      paste0(" You're borrowing ", fmt_money(-gap), " more than needed.")
    } else ""
    div(style = "margin-bottom:12px; color:#444;", msg, strong(warn))
  })
  
  # Budget plot
  output$budget_plot <- renderPlot({
    b <- budget()
    df <- data.frame(
      Category = c(names(b$expenses), "Student loans", "Left over"),
      Amount = c(unname(b$expenses), b$loan_pay, max(b$left, 0))
    )
    df$Category <- factor(df$Category, levels = rev(df$Category))
    df$Type <- ifelse(df$Category == "Left over", "Left over",
                      ifelse(df$Category == "Student loans", "Loans", "Expenses"))
    ggplot(df, aes(Category, Amount, fill = Type)) +
      geom_col() +
      geom_text(aes(label = fmt_money(Amount)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_manual(values = c(Expenses = "#4C78A8", Loans = "#E45756", `Left over` = "#54A24B")) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = NULL, y = "Dollars per month", fill = NULL) +
      theme_minimal(base_size = 13)
  })
  
  # Summary table
  output$summary_table <- renderTable({
    b <- budget()
    dti <- if (b$gross > 0) b$loan_pay / b$gross * 100 else NA
    data.frame(
      Item = c("Gross monthly income", "Pre-tax retirement", "Estimated taxes",
               "Net (take-home) income", "Student loan payments", "Living expenses",
               "Left over", "Loan payment as % of gross income"),
      Amount = c(fmt_money(b$gross), paste0("-", fmt_money(b$retirement)), paste0("-", fmt_money(b$tax)),
                 fmt_money(b$net), paste0("-", fmt_money(b$loan_pay)),
                 paste0("-", fmt_money(b$expenses_total)), fmt_money(b$left),
                 sprintf("%.1f%%", dti))
    )
  }, striped = TRUE, spacing = "s", width = "100%")
  
  # Loan detail table
  output$loan_table <- renderTable({
    l <- loans()
    if (nrow(l) == 0) return(data.frame(Message = "No loans entered."))
    out <- data.frame(
      Loan = l$Loan,
      `Type / plan` = ifelse(l$Plan == "Private", "Private", paste("Federal -", l$Plan)),
      Borrowed = fmt_money(l$Borrowed),
      `Rate (%)` = sprintf("%.2f", l$Rate),
      `Term (yrs)` = ifelse(l$Plan == "RAP", sprintf("%.1f", l$Term), as.character(l$Term)),
      `Balance at repayment start` = fmt_money(l$StartBal),
      `Monthly payment` = fmt_money(l$Payment),
      `Total interest paid` = fmt_money(l$TotalInterest),
      check.names = FALSE
    )
    out
  }, striped = TRUE, spacing = "s", width = "100%")
  
  # Balance over time
  output$balance_plot <- renderPlot({
    l <- loans()
    validate(need(nrow(l) > 0, "Add at least one loan to see the payoff curve."))
    max_months <- max(l$Term) * 12
    mths <- 0:max_months
    total <- rep(0, length(mths))
    for (i in seq_len(nrow(l))) {
      r <- l$Rate[i] / 100 / 12
      bal <- numeric(length(mths))
      bal[1] <- l$StartBal[i]
      if (l$Plan[i] == "RAP") {
        b <- rap_schedule(l$StartBal[i], l$Rate[i], l$Payment[i], l$MinPrin[i])$bal
        bal <- b[seq_along(mths)]
        bal[is.na(bal)] <- 0
      } else {
        for (m in 2:length(mths)) {
          if (mths[m] <= l$Term[i] * 12) {
            bal[m] <- max(bal[m - 1] * (1 + r) - l$Payment[i], 0)
          } else bal[m] <- 0
        }
      }
      total <- total + bal
    }
    ggplot(data.frame(Year = mths / 12, Balance = total), aes(Year, Balance)) +
      geom_area(fill = "#E45756", alpha = 0.6) +
      geom_line(color = "#E45756", linewidth = 1) +
      scale_y_continuous(labels = function(x) fmt_money(x)) +
      labs(x = "Years since repayment began", y = "Remaining loan balance") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)
