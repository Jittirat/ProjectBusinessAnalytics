

library(tibble)
library(dplyr)
library(readr)

library(readxl)

add_column <- read_excel("Desktop/Project/AllFolders/If/add_column.xlsx")

depr_df <- add_column

head(depr_df)

#change column name
colnames(depr_df)[which(names(depr_df) == "ID")] <- "IDAsset"


#Add column Location

library(dplyr)
depr_df %>%
  mutate(Location = case_when(
    endsWith(IDAsset, "R") ~ "RoseBuilding",
    endsWith(IDAsset, "S") ~ "Store"
  ))


# R adding a column to dataframe based on values in other columns:

depr_df <- depr_df %>% 
  mutate(C = if_else(AssetLifeAge == AssetLifeTaxAge, AssetLifeAge + AssetLifeTaxAge, AssetLifeAge -AssetLifeTaxAge))

# creating a column to dataframe based on values in other columns:
depr_df <- depr_df %>% 
  mutate(C = if_else(AssetLifeAge == AssetLifeTaxAge, "AgeEqual", "AgeDifference"))

#
# Adding new column based on the sum of other columns:
depr_df <- depr_df %>% rowwise() %>%
  mutate(DepreciationSum = sum(c_across(Depreciation1:Depreciation5)))



# Multiple conditions when adding new column to dataframe:
depr_df <- depr_df %>% mutate(Group =
                                case_when(DepreciationSum <= 10000 ~ "NoRepair", 
                                          
                                          DepreciationSum > 10000 ~ "NeedRepair")
)


# Append a Column based on Conditions & at a Specific area in the Dataframe
library(tibble)
depr_df <- depr_df %>%
  add_column(FixForThisYear = 
               if_else(.$DepreciationSum > 10000, TRUE, FALSE),
             .after="IDAsset")

#Fillter(function smart copy down, smart copy up)
add_column1 <- read_excel("Desktop/Project/AllFolders/If/add_column1.xlsx")
df <- add_column1

df1 <- df %>% fill(c(Depreciation2, Depreciation3), .direction = "up")
df1 <- df1 %>% fill(Depreciation4, .direction = "down")


# Set NA to 0 in R

df2 <- add_column1
df2 <- mutate_all(df2, ~replace(., is.na(.), 0))
View(df2)


#
df2 <- mutate_at(df2, c("Depreciation2", "Depreciation3", "Depreciation4"), ~replace(., is.na(.), 0))


