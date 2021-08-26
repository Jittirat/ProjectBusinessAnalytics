

library(plotly)
library(dplyr)

GL2018 <- read_excel("Desktop/Project/AllFolders/R-bubble/GL2018.xlsx")


#Create bublb chart in Credit balance
B<-filter(GL2018, Record == "Cr")

data<-B

data_2018 <- data[which(data$year == 2018),]
data_2018 <- data_2018[order(data_2018$`Type Account`, data_2018$`Sub type account`),]

E<-table(unique(GL2018ForBubble)$`Sub type account`)[ GL2018ForBubble $'Sub type account']

colors <- c('#09eb3e', '#4b58bf', '#940d19', '#ad8c39')

fig <- plot_ly(data_2018, x = ~Amount1, y = ~Date, color = ~`Type Account`, colors = colors,
               type = 'scatter', mode = 'markers', fill = ~'',
               marker = list(symbol = 'circle', sizemode = 'diameter',
                             line = list(width = 2, color = '#FFFFFF')),
               text = ~paste('SubAccount:', `Sub type account`, '<br>ClassAccount:', `Type Account`, '<br>Amount:', Amount1, '<br>Document Number:', DocNo, '<br>Month.:', month))

fig <- fig %>% layout(title = 'Amount(Credit) by date, 2018',
                      xaxis = list(title = 'Amount (Credit)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(-1, 5.191505530708712),
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Date',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, 35),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)'
)

fig


#Create bublb chart in Debit balance

A<-filter(GL2018, Record == "Dr")

data<-A

data_2018 <- data[which(data$year == 2018),]
data_2018 <- data_2018[order(data_2018$`Type Account`, data_2018$`Sub type account`),]

E<-table(unique(GL2018ForBubble)$`Sub type account`)[ GL2018ForBubble $'Sub type account']

colors <- c('#09eb3e', '#4b58bf', '#940d19', '#ad8c39')

fig <- plot_ly(data_2018, x = ~Amount, y = ~Date, color = ~`Type Account`, colors = colors,
               type = 'scatter', mode = 'markers', fill = ~'',
               marker = list(symbol = 'circle', sizemode = 'diameter',
                             line = list(width = 2, color = '#FFFFFF')),
               text = ~paste('SubAccount:', `Sub type account`, '<br>ClassAccount:', `Type Account`, '<br>Amount:', Amount, '<br>Document Number:', DocNo, '<br>Month.:', month))

fig <- fig %>% layout(title = 'Amount(Debit) by date, 2018',
                      xaxis = list(title = 'Amount (Debit)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(-1, 5.191505530708712),
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Date',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, 35),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)'
)

fig
