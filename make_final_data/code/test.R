# mac/linux 
# setwd("~/Downloads/make_final_data")
#windows
setwd("C:\\Users\\lucac\\Desktop\\make_final_data\\data\\tidy\\modified")
source ("C:\\Users\\lucac\\Desktop\\make_final_data\\code\\functions.R")
library(openxlsx)
library(stringr)

data_frame<- openxlsx::read.xlsx("inab_finan_expen.xlsx", fillMergedCells = TRUE, colNames = FALSE, rowNames = FALSE);
View(data_frame)

data_frame<- remove_meta_data(data_frame)
data_frame <-remove_labels_and_rename_columns(data_frame)

df1 <- all_dfs[["pop_tot_Sheet_1"]]
df2 <- all_dfs[["pop_tot_Sheet_2"]]
merged <- merge(x = df1, y = df2, by.x = "X1", by.y = "X1")  

View(merged)
