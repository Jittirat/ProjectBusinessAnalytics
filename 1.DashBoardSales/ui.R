source("global.R", local = TRUE)



ui <- dashboardPage(title = "www.baktisiregar1.com",
                    
                    # Header
                    dashboardHeader(title = "Sales-Dashboard",titleWidth = 300),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                        selectInput(
                            inputId = "sale",
                            label = "Name of Item:",
                            choices = item_list,
                            selected = "AB20",
                      
                            selectize = FALSE),
                        
                        # Side menu of the Dashboard  
                        sidebarMenu(
                            selectInput(
                                inputId = "month",
                                label = "Month",
                                choices = month_list,
                                selected = 99,
                                size = 13,
                                selectize = FALSE),
                            actionLink("remove", icon = icon("sync-alt"),"Remove detail tabs"),
                            menuItem("Source Code", icon = icon("github"), href = "https://github.com/Jittirat/ProjectBusinessAnalytics/tree/main/1.DashBoardSales"),
                            menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/jittirat-pushsondok-2403b6211/"),
                            menuItem("Video", icon = icon("youtube"), href = "https://drive.google.com/file/d/1Svkv8wzq7V5WMB8IPbpNf3Fm9M1ZVcxA/view?usp=s haring")
                        )
                    ),
                    
                    # The body of the dashboard
                    dashboardBody(
                        tabsetPanel(id = "tabs",
                                    tabPanel(title = "Main Dashboard",
                                             value = "page1",
                                             
                                             fluidRow(valueBoxOutput("total_sales"),
                                                      valueBoxOutput("per_day"),
                                                      valueBoxOutput("percent_profit")),
                                             fluidRow(column(width = 6,d3Output("group_totals")),
                                                      column(width = 6,d3Output("top_customers"))
                                                    
                                                      
                                                     
                                                      
                                                      
                                                      )
                                    )
                        )
                    )
)

