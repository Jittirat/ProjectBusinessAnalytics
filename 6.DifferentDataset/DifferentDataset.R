##Find the different dataset
#1.

Data1 <- data.frame(key = c("a", "b", "c", "d", "e", "f"), col1 = c(1, 2, 3, 4, 5, 6))
Data2 <- data.frame(key = c("a", "c", "e", "f", "g"), col1 = c(7, 3, 10, 6, 11))

diff_data(Data1, Data2, id = "key", always_show_header = TRUE, show_unchanged = FALSE, show_unchanged_columns = TRUE, columns_to_ignore = "col2" )

render_diff(diff_data(Data1, Data2))




#2.HTml
devtools::install_github("edwindj/daff")

library(daff)


TbFindDiff <- read_excel("Desktop/Project/AllFolders/DaffNewAccount/TbFindDiff.xlsx")

TB2018 <- TbFindDiff <- read_excel("Desktop/Project/AllFolders/DaffNewAccount/TbFindDiff.xlsx", sheet = "TB2018")

TB2019 <- bFindDiff <- read_excel("Desktop/Project/AllFolders/DaffNewAccount/TbFindDiff.xlsx",sheet = "TB2019", col_types = c("text", 
                                                                                                                              "text", "numeric", "text", "text", 
                                                                                                                              "text", "text", "numeric", "text"))
diff_data(TB2018, TB2019)
render_diff(diff_data(TB2018, TB2019))

