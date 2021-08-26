#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)
library(RColorBrewer)
library(dplyr)
library(readxl)


#devtools::install_github("tidyverse/readxl") #UseThisWhen download"tidyverse error"


#remove.packages("rlang")
#remotes::install_github("r-lib/rlang")
#install.packages("rlang")
#library(sp)
#readxl::read_xlsx("~/Desktop/Project/AllFolders/DashRisk/Risk/GL2018.xlsx")
 
df <- GL2018

GL_df <- df %>% 
    
    select(date,month,`Type Account`, `Sub type account`, Amount, DocType, Users, DocNo) %>% 
    
    group_by(date, month, `Type Account`, `Sub type account`, Amount, DocType, Users, DocNo) %>% 
    
    ungroup()

#For download data dashboard
GL2018_1<-GL2018%>% select(
                           `Sub type account`, date, Name, Memo, Amount
                           , DocNo, DocType, Users)


Doc <- GL2018_1 %>% select(DocType) %>% distinct() %>% pull()

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
    dashboardHeader(title = "GL 2018 - Risk"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Look Overall GL",
                     tabName = "weight_class_tab",
                     icon = icon("dashboard")),
            menuItem("Filter GL",
                     tabName = "head_tab",
                     icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "weight_class_tab",
                    
                    
                    fluidRow(
                     
                    box(plotlyOutput("elo_timeseries"), title = "Number of Journals by user", solidHeader = TRUE),
                    box(plotlyOutput("elo_dist")),
                    ),
                   
                    
                    fluidRow(
                   
                    valueBoxOutput("Total_transactions", width = 6),
                    valueBoxOutput("docblank_card", width = 6),
                    
                    ),
                   
                    fluidRow(
                    
                    valueBoxOutput("number_users", width = 4),
                    valueBoxOutput("number_zeroamount", width = 4), 
                    valueBoxOutput("number_greater", width = 4),
                    
                    ),
                  
                    
                    
               
                    
            ),
            tabItem(tabName = "head_tab",
                    fluidPage(
                      
                      tags$head(tags$style(HTML( '.has-feedback .form-control { padding-right: 0px;}' ))),
                      
                      titlePanel("Downloading DataA"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("DocType", label = "Choose speciesA", Doc, multiple = TRUE, selected = Doc),
                                      
                          
                          downloadButton("downloadData1", "Download entire Table  as csv")), 
                       
                        mainPanel(h4("Table 1: GL 2018"),
                          dataTableOutput("table1"), title = "My box titleA"))))
                          
           
        )
    
    )                 
)                      
    
                    
                                  
# Define server logic required to draw a histogram
server <- function(input, output) {
    
  # suppress warnings  
  storeWarn<- getOption("warn")
  options(warn = -1)
  
    output$elo_timeseries <- renderPlotly({
        
        
        
        
        b<-GL2018 %>% group_by(Users, DocType) %>% summarise(n = n(), .groups = 'drop')
        
        
        fig <- plot_ly(b,x = ~Users, y = ~n, color = ~DocType, type = 'bar')
        fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
        fig
        
        
    })
    
   
    
    
    output$elo_dist <- renderPlotly({
        
        
        
        
        c<-GL2018 %>% group_by(DocType, month) %>% summarise(n = n(), .groups = 'drop')
        
        
        fig1 <- plot_ly(c,x = ~n, y = ~DocType, color = ~month, type = 'bar')
        fig1 <- fig1 %>% layout(xaxis = list(title = 'Count'), yaxis = list(title = 'Type of Documents'), barmode = 'stack')
        fig1
        
        
    })
    
    output$Total_transactions <- renderValueBox({
      
      total2=summarise(GL_df, n())
      
      
      valueBox(
        value = paste(total2, sep = ""),
        
        color = "purple",
        subtitle = "Total Transactions",
        icon = icon("calculator"))
      
      
    })
    
    
    
    output$docblank_card <- renderValueBox({
        
        
        d<-filter(GL_df, DocType == "(blank)") 
        
        
        DocBlank1<-summarise(d, n())
        total=summarise(GL_df, n())
        
        e<- mutate(round((DocBlank1 / total) * 100, 2) )
        
        valueBox(
            value = paste(e, "%", sep = ""),
            
            color = "red",
            subtitle = "% Missing documents of total transactions",
            icon = icon("file-o"))
        
        
    })
    
    output$number_users <- renderValueBox({
      
      A<-GL_df %>% count(Users) %>%
        summarize(count_by_user =  n())
      
      valueBox(
        value = paste(A, sep = ""), 
        
        color = "maroon",
        subtitle = "Number of Users",
        icon = icon("users"))
      
      
    })
    
    output$number_zeroamount <- renderValueBox({
      
      h<-filter(GL_df, Amount == 0)
      
      
      DocBlank2<-summarise(h, n())
      total1=summarise(GL_df, n())
      
      i<- mutate(round((DocBlank2 / total1) * 100, 2) )
      
      valueBox(
        value = paste(i, "%", sep = ""),
        
        color = "navy",
        subtitle = "Number of ZeroAmount",
        icon = icon("bullseye"))
      
      
    })
    
    
    output$number_greater <- renderValueBox({
      
      k<-count(filter(GL_df, Amount >= 100 |  Amount <= -100)) 
      
      
      valueBox(
        value = paste(k, sep = ""),
        
        color = "yellow",
        subtitle = "Number of Journals greater than $100",
        icon = icon("dollar"))
      
      
    })
    
    
    
    
    # Reactive value for selected dataset ----
    
   thedata <- reactive({
      GL2018_1 %>% filter(DocType == input$DocType) 
     
      
      })
    

    
    # Table of selected dataset ----
    output$table1 <- renderDataTable({
      thedata()  %>% 
        datatable(extensions = 'Buttons',
                  options = list(
                    #Each letter is a dif element of a datatable view, this makes buttons the last thing that's shown.
                    dom = 'lfrtipB', 
                    buttons = c("copy", "csv", "pdf")),
                  filter = list(
                    position = 'top'), 
                  rownames = FALSE)
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste("table1_", Sys.Date(), ".csv", sep="")},
        content = function(file) {
        write.csv(thedata(), file, row.names = FALSE)
      }
    )
    
}




# Run the application 
shinyApp(ui = ui, server = server)
