

library(httr)
library(rvest)
library(janitor)
library(data.table)
library(rvest)
library(dplyr)
library(readxl)

#Import files

BS <- read_excel("Desktop/Project/AllFolders/Fs/BS.xlsx")
PL <- read_excel("Desktop/Project/AllFolders/Fs/PL.xlsx")
TB <- read_excel("Desktop/Project/AllFolders/Fs/TB.xlsx")
AggregateBs <- read_excel("Desktop/Project/AllFolders/Fs/AggregateBs.xlsx")



###1.CurrentRatio

balance_sheet2018 <-BS
income2018 <-PL

##FilterCurrent Assets and Liabilities
B <- filter(balance_sheet2018, Element == "AssetsCurrent" | Element == "LiabilitiesCurrent")
#change row to column
K<- t(B)

K = K[-1,]
L<-as.data.frame(K)
colnames(L)=c("AssetsCurrent","LiabilitiesCurrent")   #changeColumnName

L %>% transmute(CurrentRatio = as.numeric(AssetsCurrent) / as.numeric(LiabilitiesCurrent))



###2. Quick Ratio

balance_sheet2018 <-BS
income2018 <-PL


C <- filter(balance_sheet2018, Element == "AssetsCurrent" |  Element ==  "LiabilitiesCurrent" |  Element == "CashAndCashEquivalentsAtCarryingValue" |  Element == "AccountsReceivableNetCurrent")

#change row to column
K1<- t(C)

K1 = K1[-1,]
L<-as.data.frame(K1)

#ChangeNameColumn
colnames(L)=c("AssetsCurrent", "CashAndCashEquivalentsAtCarryingValue", "LiabilitiesCurrent", "AccountsReceivableNetCurrent")

L %>% transmute(Quick_Ratio = (as.numeric(CashAndCashEquivalentsAtCarryingValue) + as.numeric(AccountsReceivableNetCurrent)) / as.numeric(LiabilitiesCurrent))



###3. DaysSalesOutstanding
balance_sheet2018 <-BS
income2018 <-PL

#Filter AR account and revenue
D <- filter(balance_sheet2018, Element == "AccountsReceivableNetCurrent")
E <- filter(income2018, Element == "Revenue")

#Calculation in ARaverage
AccountReceivableAvg <- (D$`2018` + D$`2019`) /2

as.integer(AccountReceivableAvg)



#Cal Day sales outstanding in annual basisi
DaysSalesOutstanding <- (as.integer(AccountReceivableAvg)/ as.integer(D$`2019`)) * 365

#Adjust format daysalesoutstanding
format(DaysSalesOutstanding, digits = 2)



###4. Gross margin and Net margin

##4.1
F <- filter(income2018, Element == "NetIncome" | Element == "Cost of Goods" | Element == "Revenue")

#change row to column
K2<- t(F)

#change to datafram
K2 = K2[-1,]
G<-as.data.frame(K2)

#ChangeNameColumn
colnames(G)=c("NetIncome", "Cost of Goods", "Revenue")

#CalculateGrossMargin
G %>% transmute (Gross_Margin =
                   (-(as.numeric (G$Revenue)) -  as.numeric(G$`Cost of Goods`)))


#Calculate%GrossMargin
G %>% transmute (PercentGross_Margin =
                   ((as.numeric (G$Revenue))+  as.numeric(G$`Cost of Goods`))/ -(as.numeric (G$Revenue)))



##4.2 Net margin

G %>% transmute(Net_Margin = 
                  (as.numeric(G$NetIncome) / as.numeric (G$Revenue)) *100)


###5.Diff
lag(BS)

BS %>% transmute(Diff = BS$`2019` - BS$`2018`)

BalanceSheet <- BS %>% transmute(Diff = BS$`2019` - BS$`2018`)

###6 Table
library(htmlTable)
print(BS, html = TRUE, big.mark = ",", dateFormat = "%Y")


###7.Financial visualisations

##7.1 Stacked bars

library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyr)

new_BS<-BS %>% gather(year, value,`2019`:`2018`)

#7.1.1
# Small multiple(By Element)
ggplot(new_BS, aes(fill=Element, y=value, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Balance Sheet 2018 vs 2019") +
  theme_ipsum() +
  xlab("")


#7.1.2 By SubType

new_AggregateBs<-AggregateBs %>% gather(year, value,`2019`:`2018`)
# Small multiple
ggplot(new_AggregateBs, aes(fill=SubType, y=value, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Balance Sheet 2018 vs 2019") +
  theme_ipsum() +
  xlab("")


#7.2 Plotly

install.packages("plotly")
update.packages("ggplot2")
library(plotly)
library(ggplot2)


#changeFromColumntoRow
install.packages("dplyr")
library(dplyr)
new_AggregateBs<-AggregateBs %>% gather(year, value,`2019`:`2018`)

d<- new_AggregateBs 

#7.2.1 PlotlyBy SubType

fig <- plot_ly(d,x = ~year, y = ~value, color = ~SubType, type = 'bar', stat = "identity")
fig <- fig %>% layout(yaxis = list(title = 'Balance Sheet'), barmode = 'relative')
fig

#7.2.2 Plotly with negative value
d<- new_AggregateBs 
fig <- plot_ly(d,x = ~year, y = ~value, color = ~SubType, type = 'bar', stat = "identity")
fig <- fig %>% layout(yaxis = barmode = 'relative', list(title = 'Balance Sheet'))
fig 

##7.3 RevenueExpense

library(RColorBrewer)

library(plotly)
library(dplyr)
library(tidyr) # for gather

RevenueExpense<- filter(TB, `Type Account`== "Expense" | `Type Account` == "Revenue") 

#Expense

expense <- filter(RevenueExpense, `Type Account`== "Expense")
expense1<- expense %>% select( `Type Account`,`2019`, `2018`)
expense2 <- sum(expense$`2019`)

expense3 <- sum(expense$`2018`)

#Revenue
revenue <- filter(RevenueExpense, `Type Account`== "Revenue")
revenue1<- revenue %>% select( `Type Account`,`2019`, `2018`)
revenue2 <- sum(revenue$`2019`)

revenue3 <- sum(revenue$`2018`)

#PLoteGraph
fig <- plot_ly()
fig <- fig %>% add_bars(
  x = c("2018", "2019"),
  y = c(305607, 1017507),
  
  base = c(-305607, -1017507),
  marker = list(
    color = 'blueviolet '
  ),
  name = 'expenses'
)

fig <- fig %>% add_bars(
  x = c("2018", "2019"),
  y = c(411810, 755310),
  base = 0,
  marker = list(
    color = 'lightpink'
  ),
  name = 'revenue'
)
fig




