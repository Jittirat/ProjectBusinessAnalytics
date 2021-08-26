

library(tidyverse)
library(readxl)

B <- GL2018 <- read_excel("Desktop/Project/AllFolders/Duplicate/GL2018.xlsx")

B$DocNo[!duplicated(B$DocNo)]


C <- B %>% dplyr::distinct(DocNo)
View(C)


# Show the repeat entries
B[duplicated(B),]
#count replicate functions
sum(duplicated(B))


B[duplicated(B) | duplicated(B, fromLast=TRUE), ]


B[duplicated(B) | duplicated(B, first = TRUE, keep.all = TRUE,
                             from.last = FALSE, keep.row.names = TRUE,
                             check = TRUE), ]


E <- duplicated(B)
options(max.print = length(E))
print(E)


#######Select columns
library(dplyr)
library(data.table)
library(hablar)

F <- B %>% 
  find_duplicates(date, Name, Memo, Split, Amount)
View(F)


