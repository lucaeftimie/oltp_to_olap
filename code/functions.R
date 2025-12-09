clean_files <- function(x, y){
  #Print file path
  print(x)

  # Names of the sheets and number of sheets in the current file
  sheet_names <- getSheetNames(x)
  no_of_sheets <- length(sheet_names)

  # Create custom name for the newly created csv files
  for(i in 1:no_of_sheets){
    sheet_names[i]  <- str_glue("{y}_Sheet_{i}")
  }

  # Create list of sheets to return
  dfs <- vector("list", no_of_sheets)
  names(dfs) <- sheet_names

  # 
  for (sh in 1:no_of_sheets){
    data_frame<- openxlsx::read.xlsx(x, sheet = sh, fillMergedCells = TRUE, colNames = FALSE, rowNames = FALSE);

    data_frame <- remove_meta_data(data_frame)
    data_frame <- remove_labels_and_rename_columns(data_frame)
    data_frame <- remove_duplicate_columns(data_frame)
    data_frame <- rename_columns(data_frame)
    data_frame <- data_frame[-c(2),]


    data_frame[data_frame == ":"] <- 0
    data_frame[is.na(data_frame)] <- 0
    data_frame <- data_frame[rowSums(data_frame == 0) == 0, ]
    data_frame[1,1] <- "region_id"


    dfs[[sheet_names[sh]]] <- data_frame  
  }
  return (dfs) 
}


remove_meta_data <- function(sheet){

  first_row <- sheet$X1 
  number_of_rows <- 0

    for(i in 1:length(sheet$X1)){
      if(first_row[i] != 'Special value')
        number_of_rows <- number_of_rows + 1
      if(first_row[i] == 'Special value')
        break
    }

  sheet <- sheet[1:number_of_rows, ]
    
  number_of_rows <- 0


    for (i in 1:length(sheet$X1)) {
      if(first_row[i] != 'TIME')
        number_of_rows <- number_of_rows + 1
      if(first_row[i]== 'TIME')
        break
    }

    number_of_rows <- number_of_rows + 1
    sheet <- sheet[number_of_rows:nrow(sheet), ]


  return (sheet)
}


remove_labels_and_rename_columns <- function(sheet){
  if (sheet$X2[2] == "GEO (Labels)" ){
    col_names  <-  names(sheet)
    col_names  <- col_names[-c(2)]
    sheet <-  subset(sheet, select = col_names)

    col_names <- c()
    for(i in 1:ncol(sheet))
      col_names <- c(col_names, str_glue("X{i}"))

    names(sheet) <- col_names
  }

  return(sheet)
}

remove_duplicate_columns <- function(sheet){
    for(i in 1:ncol(sheet)){
      if(i %% 2 == 1 && i != 1){
        column_to_drop <- str_glue("X{i}")
        col_names <- names(sheet)
        sheet <- sheet[, !col_names %in% column_to_drop]
      }
  }

  return (sheet)
}

rename_columns <- function(sheet){
    col_names <- c()

    for(i in 1:ncol(sheet)){
      col_names <- c(col_names, str_glue("X{i}"))
    }

    names(sheet) <- col_names

    return (sheet)
}

remove_sheets <- function(out, v, indicator){
  for(i in v){
   f_to_d <- str_glue("{out}/{indicator}_Sheet_{i}.csv")
   file.remove(f_to_d)
  }
}
merge_tables <- function(df1, df2){
  merged_1_partial <- data.frame(merge(x = df1, y = df2, by.x = "X1", by.y = "X1"))
  merged_1 <- merged_1_partial %>% mutate(
    X2 = as.numeric(X2.x) + as.numeric(X2.y),
    X3 = as.numeric(X3.x) + as.numeric(X3.y),
    X4 = as.numeric(X4.x) + as.numeric(X4.y),
    X5 = as.numeric(X5.x) + as.numeric(X5.y),
    X6 = as.numeric(X6.x) + as.numeric(X6.y),
    X7 = as.numeric(X7.x) + as.numeric(X7.y),
    X8 = as.numeric(X8.x) + as.numeric(X8.y),
    X9 = as.numeric(X9.x) + as.numeric(X9.y),
    X10 = as.numeric(X10.x) + as.numeric(X10.y),
    X11 = as.numeric(X11.x) + as.numeric(X11.y)
  ) %>% select(X1,X2, X3, X4, X5, X6, X7, X8, X9, X10, X11)

  return(merged_1)
}

rename_headers <- function(df){
  colnames(df) <- df[1,]
  df <- df[-c(1), ]
  return(df)
}


tabular_to_columnar = function(df, indicator_name){
  new_df <- data.frame(region_id = c('BE10'),indicator = c('emp_pers'),year = c(2014))
  new_df <- new_df[-c(1), ]


  # for(i in 1:nrow(df)){
  #   for(k in 1:12){
  #     cat(df[i,k])
  #     cat("  ")
  #     Sys.sleep(0.08)
  #   }

  cat("\n")

  for(i in 1:nrow(df)){
    for(j in 2:ncol(df)){ 
      new_row <- list(region_id= NA_character_, indicator = NA_character_, year = NA_integer_, value = NA_integer_)
      new_row[["region_id"]]   <- df[i, 1]
      new_row[["indicator"]] <- indicator_name
      new_row[["year"]] <- as.integer(colnames(df)[j])
      new_row[["value"]] <- df[i,j]
      new_df <- rbind(new_df, new_row)
    }
  }
 
  return (new_df)

}

create_csvs <- function(list, wd, add_col_names){
    # outputs csv directory
  output_dir <- paste(wd, "/outputs_csv" , sep = "")
  # check if directory exists
  exists_output_dir <- dir.exists(output_dir)

  # Write each data frame from all_dfs into a single csv file
  if(!exists_output_dir){
    dir.create(output_dir)
    setwd(output_dir)
    for(i in 1:length(list)){
      new_file_name <- str_glue("{names(list)[i]}.csv")
      write.table(list[[i]], file = new_file_name, row.names= FALSE, col.names = add_col_names, sep=",")
      # Sys.sleep(0.2)
      print(new_file_name)
    }
  }else {
    print("Directory exists")
  }
}


aggregate_data <- function(dfs, df, i, no_of_dfs){
  sum <- 0
  if(i == no_of_dfs)
    return (df)
  else 
    return(merge_tables(df , aggregate_data(dfs, dfs[[i+1]], i + 1, no_of_dfs)))
}


identify_sheets_to_agg <- function(dfs, no_of_dfs_to_agg, indicator ){
  dfs_to_agg <- list()
  for(i in 1:no_of_dfs_to_agg){
    sheet_name_to_agg <- str_glue("{indicator}_Sheet_{i}")
    dfs_to_agg[[i]] <- dfs[[sheet_name_to_agg]] 
  }
  return(dfs_to_agg)
}











