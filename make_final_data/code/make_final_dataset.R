# mac/linux 
  # setwd("~/Desktop/oltp_to_olap/make_final_data")
#windows
  setwd("C:\\Users\\lucac\\Desktop\\oltp_to_olap\\make_final_data\\")

library(data.table)
library(openxlsx)
library(stringr)
library(dplyr)

# Source the tidy functions 
source("./code/functions.R")

# Open the legend of the datatables
legend <- openxlsx::read.xlsx(paste(getwd(), "/data/legend.xlsx", sep=""))

# Path of files to modify
dir_files <- paste(getwd(), "/data/tidy/modified", sep = "")

all_dfs <- list()

# Create list of dfs, each df corresponds to one sheet from a file
for(i in 1:nrow(legend)){
  file_path <- str_glue("{dir_files}/{legend$Cod.modificat[i]}.xlsx")

  file_sheets <- clean_files(file_path, str_glue("{legend$Cod.modificat[i]}"))
  for (nm in names(file_sheets)) {
    all_dfs[[nm]] <- file_sheets[[nm]]
  }
}

names(all_dfs)

# Identify the sheets that need to be aggregate (makes a shallow copy of the data frames that have to be aggregated)
pop_tot_dfs <- identify_sheets_to_agg(all_dfs, 9, "pop_tot")


# Aggregate the total population data: age 20 to age 64
no_of_sheets_to_manip <- 9
pop_tot_20_64 <- aggregate_data(pop_tot_dfs,pop_tot_dfs[[1]], 1, no_of_sheets_to_manip)

# drop dfs which are not needed anymore(the ones used for aggregation)
dfs_to_drop <- c()
indicator_to_drop <- "pop_tot"
for(i in 1:no_of_sheets_to_manip){
  dfs_to_drop <- c(dfs_to_drop, str_glue("{indicator_to_drop}_Sheet_{i}"))
}
all_dfs <- all_dfs[ !(names(all_dfs) %in% dfs_to_drop) ]

# Insert the new df into the original df at the desired position
position_to_insert <- length(all_dfs) - 1
all_dfs <- append(all_dfs, setNames(list(pop_tot_20_64), "pop_tot_20_64_Sheet_1"), after = position_to_insert)


# Remove the unwanted Sheets

  # delete from emp_tech
  dfs_to_drop <- c()
  vector_sheets_to_remove <- c(1:46)
  
  indicator_to_drop <- "emp_tech"
  
  sheets_to_remove <- vector_sheets_to_remove[-c(3,5,7,21,23,25,27,31,35,37,41,43,45)]
  
  for(i in sheets_to_remove){
    dfs_to_drop <- c(dfs_to_drop, str_glue("{indicator_to_drop}_Sheet_{i}"))
  }
  
  all_dfs <- all_dfs[ !(names(all_dfs) %in% dfs_to_drop) ]
  
    # remove_sheets(getwd(), vector_sheets_to_remove,"emp_tech")
  
  # delete from gdp
  dfs_to_drop <- c()
  
  vector_sheets_to_remove <- c(1:4)
  
  indicator_to_drop <- "gdp"
  
  sheets_to_remove <- vector_sheets_to_remove[-c(1)]
  
  for(i in sheets_to_remove){
    dfs_to_drop <- c(dfs_to_drop, str_glue("{indicator_to_drop}_Sheet_{i}"))
  }
  
  all_dfs <- all_dfs[ !(names(all_dfs) %in% dfs_to_drop) ]
  
    # remove_sheets(getwd(), vector_sheets_to_remove,"gdp")
  
  # delete from gfcf
  dfs_to_drop <- c()
  vector_sheets_to_remove <- c(1:8)
  indicator_to_drop <- "gfcf"
  sheets_to_remove <- vector_sheets_to_remove
  
  for(i in sheets_to_remove){
    dfs_to_drop <- c(dfs_to_drop, str_glue("{indicator_to_drop}_Sheet_{i}"))
  }
  
  all_dfs <- all_dfs[ !(names(all_dfs) %in% dfs_to_drop) ]
  
  
  
  names(all_dfs)
  
  
  
    # remove_sheets(getwd(), vector_sheets_to_remove,"gfcf")


# Transform data from a tabular version to a columnar version
df_names <- names(all_dfs) 
source("./code/functions.R")
columnar_dfs <- list()

for(i in 1:length(all_dfs)){
    all_dfs[[i]] <- rename_headers(all_dfs[[i]])
    
    df <- all_dfs[[i]]

    col_df <- tabular_to_columnar(df, df_names[i])
    columnar_dfs[[df_names[i]]] <- col_df

}


create_csvs(columnar_dfs, getwd(), TRUE)












