
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
      selectInput(inputID = "school",
                  label   = "School",
                  choices = c("Enter values manually" = "", sort(unique(school_data$school)))),
      selectInput("program", "Program", choices = c("Select a school first" = "")),
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
      h4("Total balance over time (standard repayment)"),
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
  
  # Populate the manual inputs with the looked-up values.
  # The user can still edit them afterward.
  observeEvent(selected_row(), {
    r <- selected_row()
    updateNumericInput(session, "salary", value = r$median_earnings)
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
      wellPanel(
        strong(paste("Loan", i)),
        numericInput(paste0("loan_amt_", i), "Amount borrowed ($)", defaults_amt[i], min = 0, step = 500),
        numericInput(paste0("loan_rate_", i), "Annual interest rate (%)", defaults_rate[i], min = 0, max = 30, step = 0.1),
        numericInput(paste0("loan_term_", i), "Repayment term (years)", 10, min = 1, max = 30, step = 1)
      )
    })
  })
  
  # Loan table
  loans <- reactive({
    n <- input$n_loans
    req(n)
    if (n < 1) {
      return(data.frame(Loan = character(), Borrowed = numeric(), Rate = numeric(),
                        Term = numeric(), StartBal = numeric(), Payment = numeric(),
                        TotalInterest = numeric()))
    }
    rows <- lapply(seq_len(n), function(i) {
      amt  <- input[[paste0("loan_amt_", i)]]
      rate <- input[[paste0("loan_rate_", i)]]
      term <- input[[paste0("loan_term_", i)]]
      req(amt, rate, term)
      start <- starting_balance(amt, rate, input$accrue, input$years_school, input$grace)
      pay   <- monthly_payment(start, rate, term)
      data.frame(
        Loan = paste("Loan", i),
        Borrowed = amt,
        Rate = rate,
        Term = term,
        StartBal = start,
        Payment = pay,
        TotalInterest = pay * term * 12 - amt  # includes interest accrued in school
      )
    })
    do.call(rbind, rows)
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
      Borrowed = fmt_money(l$Borrowed),
      `Rate (%)` = sprintf("%.2f", l$Rate),
      `Term (yrs)` = l$Term,
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
    months <- 0:max_months
    total <- rep(0, length(months))
    for (i in seq_len(nrow(l))) {
      r <- l$Rate[i] / 100 / 12
      bal <- numeric(length(months))
      bal[1] <- l$StartBal[i]
      for (m in 2:length(months)) {
        if (months[m] <= l$Term[i] * 12) {
          bal[m] <- max(bal[m - 1] * (1 + r) - l$Payment[i], 0)
        } else bal[m] <- 0
      }
      total <- total + bal
    }
    ggplot(data.frame(Year = months / 12, Balance = total), aes(Year, Balance)) +
      geom_area(fill = "#E45756", alpha = 0.6) +
      geom_line(color = "#E45756", linewidth = 1) +
      scale_y_continuous(labels = function(x) fmt_money(x)) +
      labs(x = "Years since repayment began", y = "Remaining loan balance") +
      theme_minimal(base_size = 13)
  })
}