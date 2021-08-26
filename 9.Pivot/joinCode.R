
library(tidyverse)

dt <-read_excel("Desktop/Project/AllFolders/PivotJoin/sales.xlsx")

dt %>% group_by(Customer, ItemCodeSub, Amount) %>%
  count() %>%
  arrange(desc(n))

dt %>% filter(Customer == "Cole Home Builders:Phase 1 - Lot 5") %>%
group_by(ItemCodeSub) %>% 
  summarise(
    AvgBase = mean(Amount),
    MedianBase = median(Amount),
    TotalBase = sum(Amount)
  )


dt %>% filter(Customer == "Cole Home Builders:Phase 1 - Lot 5") %>%
  group_by(ItemCodeSub) %>% 
  summarise_if(is.numeric, mean)


WideData <- dt %>% filter(Customer == "Cole Home Builders:Phase 1 - Lot 5") %>%
  group_by(ItemCodeSub, month) %>% 
  summarise(TotalBase = sum(Amount), .groups = 'drop') %>% 
  pivot_wider(names_from = month, values_from = TotalBase)


WideData %>% pivot_longer(-ItemCodeSub, names_to = "Mth", values_to = "Base" )


############

#Joint

library(tidyverse)

A <-read_excel("Desktop/Project/AllFolders/PivotJoin/A.xlsx")
B <-read_excel("Desktop/Project/AllFolders/PivotJoin/B.xlsx")

View(A)
View(B)

left_join(A, B)

left_join(A, B, by = "ReceiptNo")


#WhensameinformationButColumnDifferent
left_join(A, B, by = c("ReceiptNo" = "ReceiptNo"))


right_join(A, B)
anti_join(A, B)
full_join(A, B)
inner_join(A,B)
semi_join(A,B)


C <-read_excel("Desktop/Project/AllFolders/PivotJoin/C.xlsx")

left_join(A, C)       
 
C %>% distinct(ReceiptNo)

C %>% distinct(ReceiptNo, .keep_all = T)


D <- C %>% 
arrange(desc(month)) %>% 
distinct(ReceiptNo,.keep_all = T)


left_join(A, D)


         