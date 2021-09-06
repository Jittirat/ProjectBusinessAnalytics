





server <- function(input, output, session) {
  tab_list <- NULL
  
  showNotification("Created by Jittirat", duration = NULL, type = "message")
  
  # Use a reactive() function to prepare the base SQL query that all the elements in the dashboard
  # will use. The reactive() allows us to evaluate the input variables
  
  #read files
  sales = read.csv("sales.csv")
  Gp = readxl::read_xlsx("Gp.xlsx")
  
  base_electric <- reactive({
    res <- sales %>%
      filter(ItemCode == input$sale) %>%
      select(Time, year, month, day, Num, Customer, CustomerCode, Item, ItemCode, ItemCodeSub, Quantity, UnitPrice, Tax, Amount, Des, ReceiptNo) %>%
      
      left_join(Gp, by="ReceiptNo") %>%
      
      
      select(-AccountGp, -Num.y, -TimeA, -day.y, -month.y, -year.y, -Customer.y, -Product, -Amount1, -Des.y) %>%
      left_join(GlRevenue, by= "ItemCode") %>% 
      select(-Other, -Other1) %>%
      
      rename(cust_name = CustomerCode)
    if (input$month != 99) res <- filter(res, month.x == input$month)
    res
  })
  
  
  
  # Total Sales (server) ------------------------------------------
  output$total_sales <- renderValueBox({
    # The following code runs inside the database.
    #tally() is a convenient wrapper for summarise that will either call n() or sum(n) depending on whether you're tallying for the first time, or re-tallying.
    # pull() bring the results into R, which then
    # it's piped directly to a valueBox()
    base_electric() %>%
      group_by(day.x, month.x, Amount)%>%
      
      tally() %>%
      ungroup() %>%
      summarise(TotalSales = sum(Amount)) %>%
      
      pull(TotalSales) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "purple",subtitle = "Total Sales")
  })
  
  # Avg per Day (server) --------------------------------------------
  output$per_day <- renderValueBox({
    # The following code runs inside the database
    
    base_electric() %>%
      
      group_by(day.x, month.x, Amount)%>%
      
      
      tally() %>%
      
      ungroup() %>%
      
      summarise(avg = mean(Amount)) %>%
      pull(avg) %>%
      round() %>%
      prettyNum(big.mark = ",") %>%
      
      valueBox(icon = icon("balance-scale"), color = "fuchsia",subtitle = "Average Sales per day") 
  })
  
  # Percent profit per day (server) ----------------------------------------
  output$percent_profit <- renderValueBox({
    base_electric() %>%
      filter(!is.na(Profit)) %>%
      
      summarise(
        profits = mean(Profit[Profit>0]),
        
      ) %>%
      mutate(percent = profits) %>%
      pull() %>%
      
      round() %>%
      paste0("%") %>%
      valueBox(icon = icon("percent"), color = "teal", subtitle = "Profit per day")
  })
  
  # Montly/daily trend (server) -------------------------------------
  output$group_totals <- renderD3({
    grouped <- ifelse(input$month != 99, expr(day.x), expr(month.x))
    
    
    
    res <- base_electric() %>%
      
      group_by(!!grouped) %>%
      summarise(sum_sales = sum(Amount)) %>%
      group_by(!!grouped, sum_sales) %>%
      tally() %>%
      ungroup()%>%
      collect() %>%
      
      mutate(
        y = sum_sales,
        x = !!grouped
      ) %>%
      select(x, y)
    
    if (input$month == 99) {
      res <- res %>%
        inner_join(
          tibble(x = 1:12, label = substr(month.name, 1, 3)),
          by = "x"
        )
    } else {
      res <- res %>%
        mutate(label = x)
    }
    r2d3(res, "col_plot.js")
  })
  
  # Top customers (server) -------------------------------------------
  output$top_customers <- renderD3({
    # The following code runs inside the database
    base_electric() %>%
      group_by(Customer.x, cust_name, Amount) %>%
      tally() %>%
      
      collect() %>%
      arrange(desc(n)) %>%
      head(10) %>%
      arrange(cust_name) %>%
      mutate(cust_name=str_sub(cust_name,1, 30)) %>%
      
      rename(
        x = Customer.x,
        y = Amount,
        label = cust_name
      ) %>%
      r2d3("bar_plot.js")
  })
  
  
  # Get details (server) --------------------------------------------
  get_details <- function(customer = NULL, day = NULL) {
    # Create a generic details function that can be called
    # by different dashboard events
    
    res <- base_electric() 
    
    if (!is.null(customer)) res <- filter(res, Customer.x == as.character(customer))
    if (!is.null(day)) res <- filter(res, day.x == !!as.integer(day))
    
    res %>%
      head(100) %>% 
      select(
        month.x, day.x, Customer.x, Item, Quantity, UnitPrice,
        cust_name, Amount, Cost, Profit
      ) %>%
      collect() %>%
      mutate(month.x = month.name[as.integer(month.x)])
    
    
  }
  
  # Month/Day column click (server) ---------------------------------
  observeEvent(input$column_clicked != "", {
    if (input$month == "99") {
      updateSelectInput(session, "month", selected = input$column_clicked)
    } else {
      day.x <- unique(base_electric()$day.x)[as.integer(input$column_clicked)]
      month.x <- input$month
      tab_title <- paste(
        input$sale, "-", month.name[as.integer(month.x)], "-", as.integer(day.x)
      )
      
      
      if (!(tab_title %in% tab_list)) {
        appendTab(
          inputId = "tabs",
          tabPanel(
            tab_title,
            DT::renderDataTable(
              get_details(day = as.integer(day.x))
              
              
              
            )
            
          )
        )
        tab_list <<- c(tab_list, tab_title)
      }
      updateTabsetPanel(session, "tabs", selected = tab_title)
    }
  },
  ignoreInit = TRUE
  )
  
  
  
  
  
  # Bar clicked (server) --------------------------------------------
  observeEvent(input$bar_clicked , {
    customer <- as.character(input$bar_clicked)
    month <- input$month
    tab_title <- paste(
      input$sale, "-", customer,
      if (month != 99) {
        paste("-", month.name[as.integer(month)])
      }
    )
    if (!(tab_title %in% tab_list)) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          tab_title,
          DT::renderDataTable(
            get_details(customer = customer)
            
            
          )
        )
      )
      
      tab_list <<- c(tab_list, tab_title)
    }
    updateTabsetPanel(session, "tabs", selected = tab_title)
  })
  
  # Remote tabs (server) --------------------------------------------
  observeEvent(input$remove, {
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~ removeTab("tabs", .x))
    tab_list <<- NULL
    
    
    
  })
}
