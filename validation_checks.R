library(RSQLite)
library(DBI)
library(readr)
library(dplyr)
library(sets)

#dbWriteTable(conn, name = "customer", value = CUSTOMER, overwrite=TRUE, row.names = FALSE)

# ----------------------------------------------------- Functions ------------------------------------------------------------
delete_table <- function(table_name) {
  query = "Delete from"
  query = paste(query, table_name)
  dbExecute(conn, query)
}

write_table <- function(table_name, data_values) {
  dbWriteTable(conn, name = table_name, value = data_values, overwrite=TRUE, row.names = FALSE)
}

append_table <- function(table_name, data_values) {
  dbAppendTable(conn, name = table_name, value = data_values)
}

# Checks if any value is updated in the excel file
is_modified <- function(df_excel, df_db, prim_key) {
  n_cols <- length(colnames(df_excel))
  n <- nrow(df_excel)
  res_df <- df_excel == df_db
  updated_df <- data.frame()
  non_updated_df <- data.frame()
  for (i in 1:n) {
    if (sum(res_df[i,]) != n_cols) {
      updated_df <- rbind(updated_df, df_excel[i,])
    }
    else {
      non_updated_df <- rbind(non_updated_df, df_excel[i,])
    }
  }
 return (list(updated = updated_df, non_updated = non_updated_df))
}

check_ref_integrity <- function(inp_df, list_of_values, key) {
  invalid_records <- data.frame()
  valid_records <- data.frame()
  for (i in 1:nrow(inp_df)) {
    if(!(inp_df[i, key] %in% list_of_values)) {
      invalid_records <- rbind(invalid_records, inp_df[i,])
    }
    else {
      valid_records <- rbind(valid_records, inp_df[i,])
    }
  }
  return (list(valid = valid_records, invalid = invalid_records))
}

# ---------------------------------------------------- Connect to database ---------------------------------------------------------
conn <- RSQLite::dbConnect(RSQLite::SQLite(), "project.db")
a <- 1:50
# ------------------------------------------------ Customer address data checks --------------------------------------------------------
df_customer_excel <- read_csv('Data/CUSTOMER_address1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from CUSTOMER_ADDRESS')
table_name <- 'CUSTOMER_ADDRESS'
prim_key = 'caddress_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of customer address id
caddress_id_list <- unique(df_customer_excel$caddress_id)

sanity_check_customer_address <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$caddress_id),]
  duplicated_df <- inp_df[duplicated(inp_df$caddress_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'caddress_id']) | is.na(inp_df[i, 'country']) | is.na(inp_df[i, 'city']) | (grepl("[0-9]", inp_df[i, 'city'])) | is.na(inp_df[i, 'street']) | is.na(inp_df[i, 'postcode']) | 
       (grepl("[0-9]", inp_df[i, 'country']))) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_customer_address(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$caddress_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$caddress_id),]
df_customer_db <- df_customer_db[order(df_customer_db$caddress_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$caddress_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
  append_table(table_name, insertion_df)
} 
# ------------------------------------------------- Customer Data checks -----------------------------------------------------------------
df_customer_excel <- read_csv('Data/CUSTOMER1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from CUSTOMER')
table_name <- 'CUSTOMER'
prim_key = 'customer_id'
foreign_key = 'caddress_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

sanity_check_customer <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$customer_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$customer_id),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df)
  }
  
  inp_df <- unique_df
  unique_df <- inp_df[!duplicated(inp_df$email),] 
  duplicated_df <- inp_df[duplicated(inp_df$email),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df )
  }
  
  inp_df <- unique_df
  unique_df <- inp_df[!duplicated(inp_df$mobile),] 
  duplicated_df <- inp_df[duplicated(inp_df$mobile),]
  
  if((nrow(duplicated_df) > 0) | (nrow(duplicated_data) > 0)) {
    duplicated_data <- rbind(duplicated_data, duplicated_df)
    print('Following records are duplicated \n')
    print(duplicated_data)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'customer_id']) | is.na(inp_df[i, 'caddress_id']) | is.na(inp_df[i, 'first_name']) | 
       is.na(inp_df[i, 'last_name']) | is.na(inp_df[i, 'password']) | is.na(inp_df[i, 'email']) | is.na(inp_df[i, 'mobile'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Invalid data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_customer(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$customer_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$customer_id),]
df_customer_db <- df_customer_db[order(df_customer_db$customer_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$customer_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, caddress_id_list, foreign_key)
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
# --------------------------------------------- Category Parent Checks --------------------------------------------------------
df_customer_excel <- read_csv('Data/CATEGORY_parent1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from category_parent')
table_name <- 'category_parent'
prim_key = 'parent_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of customer address id
parent_category_id_list <- unique(df_customer_excel$parent_id)

sanity_check_category_parent <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$parent_id),]
  duplicated_df <- inp_df[duplicated(inp_df$parent_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'parent_id']) | is.na(inp_df[i, 'parent_category'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(inp_df[i,])
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_category_parent(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$parent_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$parent_id),]
df_customer_db <- df_customer_db[order(df_customer_db$parent_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$parent_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)
non_modified_df <- data.frame()

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
  append_table(table_name, insertion_df)
} 
# ------------------------------------------------ Category data validation ---------------------------------------------------
df_customer_excel <- read_csv('Data/CATEGORY1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from category')
table_name <- 'category'
prim_key = 'category_id'
foreign_key = 'parent_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of category address id
category_id_list <- unique(df_customer_excel$category_id)

sanity_check_category <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$category_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$category_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'category_id']) | is.na(inp_df[i, 'category_name']) | is.na(inp_df[i, 'filter_by'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Invalid data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_category(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$category_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$category_id),]
df_customer_db <- df_customer_db[order(df_customer_db$category_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$category_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, parent_category_id_list, foreign_key)
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
# ----------------------------------------------------- Products -----------------------------------------------------------------
df_customer_excel <- read_csv('Data/PRODUCT1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from product')
table_name <- 'product'
prim_key = 'product_id'
foreign_key = 'category_id'

inner_join_df <- data.frame()
inner_join_db_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()
rem_records_excel_df <- data.frame()

df_customer_excel <- df_customer_excel[,c(1,2,3,4,5,6,7)]
df_customer_db <- df_customer_db[,c(1,2,3,4,5,6,7)]

# Unique values of product id
product_id_list <- unique(df_customer_excel$product_id)

sanity_check_product <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$product_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$product_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'product_id']) | is.na(inp_df[i, 'product_name']) | is.na(inp_df[i, 'unit_price'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Invalid data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_product(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$product_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$product_id),]
df_customer_db <- df_customer_db[order(df_customer_db$product_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$product_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, category_id_list, foreign_key)
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
# --------------------------------------------------- Supplier Address validation --------------------------------------------
df_customer_excel <- read_csv('Data/SUPPLIER_address1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from supplier_address')
table_name <- 'supplier_address'
prim_key = 'supaddress_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of customer address id
sup_address_id_list <- unique(df_customer_excel$supaddress_id)

sanity_check_supplier_address <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$supaddress_id),]
  duplicated_df <- inp_df[duplicated(inp_df$supaddress_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'country']) | is.na(inp_df[i, 'street']) | is.na(inp_df[i, 'city']) | is.na(inp_df[i, 'postcode']) |
       is.na(inp_df[i, 'supaddress_id']) | grepl("[0-9]", inp_df[i, 'country']) | grepl("[0-9]", inp_df[i, 'city'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(inp_df[i,])
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_supplier_address(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$supaddress_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$supaddress_id),]
df_customer_db <- df_customer_db[order(df_customer_db$supaddress_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$supaddress_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)
non_modified_df <- data.frame()

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
  append_table(table_name, insertion_df)
} 
# -------------------------------------------------- Supplier validation -------------------------------------------------------
df_customer_excel <- read_csv('Data/SUPPLIER1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from supplier')
table_name <- 'supplier'
prim_key = 'supplier_id'
foreign_key = 'supaddress_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of customer address id
supplier_id_list <- unique(df_customer_excel$supplier_id)

sanity_check_supplier_address <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$supplier_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$supplier_id),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df)
  }
  
  inp_df <- unique_df
  unique_df <- inp_df[!duplicated(inp_df$supplier_phone),] 
  duplicated_df <- inp_df[duplicated(inp_df$supplier_phone),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df )
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'supplier_id']) | is.na(inp_df[i, 'supplier_name']) | is.na(inp_df[i, 'supaddress_id']) | is.na(inp_df[i, 'supplier_phone']) |
       grepl("[a-zA-Z]", inp_df[i, 'supplier_phone'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(inp_df[i,])
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_supplier_address(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$supaddress_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$supaddress_id),]
df_customer_db <- df_customer_db[order(df_customer_db$supaddress_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$supaddress_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)
non_modified_df <- data.frame()

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, sup_address_id_list, foreign_key)
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
# ------------------------------------------------------ supply validation --------------------------------------------------------------
df_customer_excel <- read_csv('Data/SUPPLY1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from supply')
table_name <- 'supply'
prim_key = 'supply_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

# Unique values of customer address id
supply_id_list <- unique(df_customer_excel$supply_id)

sanity_check_supply <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$supply_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$supply_id),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'supplier_id']) | is.na(inp_df[i, 'supply_id']) | is.na(inp_df[i, 'product_id'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(inp_df[i,])
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_supply(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$supply_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$supply_id),]
df_customer_db <- df_customer_db[order(df_customer_db$supply_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$supply_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)
non_modified_df <- data.frame()

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, supplier_id_list, 'supplier_id')
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(valid_record) > 0) {
  records <- check_ref_integrity(valid_record, product_id_list, 'product_id')
  valid_record <- records$valid
  invalid_records <- rbind(invalid_records, records$invalid)
}


if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
# --------------------------------------------- Shipment address validation ----------------------------------------------------
df_customer_excel <- read_csv('Data/SHIPMENT_address1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from SHIPMENT_ADDRESS')
table_name <- 'SHIPMENT_ADDRESS'
prim_key = 'saddress_id'

df_customer_excel <- df_customer_excel[,c(1,2,3,4,5)]
df_customer_db <- df_customer_db[,c(1,2,3,4,5)]


inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()


# Unique values of shipment address id
shipment_address_id_list <- unique(df_customer_excel$saddress_id)

sanity_check_shipment_address <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$saddress_id),]
  duplicated_df <- inp_df[duplicated(inp_df$saddress_id),]
  
  if(nrow(duplicated_df) > 0) {
    print('Following records are duplicated \n')
    print(duplicated_df)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if((is.na(inp_df[i, 'saddress_id'])) | (is.na(inp_df[i, 'city'])) | (is.na(inp_df[i, 'street'])) | 
       (is.na(inp_df[i, 'postcode'])) | (is.na(inp_df[i, 'country'])) | (grepl("[0-9]", inp_df[i, 'city'])) | 
       (grepl("[0-9]", inp_df[i, 'country']))) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Bad quality data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = 'saddress_id')
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_shipment_address(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$saddress_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$saddress_id),]
df_customer_db <- df_customer_db[order(df_customer_db$saddress_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$saddress_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
  append_table(table_name, insertion_df)
} 
# ------------------------------------------------- Shipment validation ------------------------------------------------------------
df_customer_excel <- read_csv('Data/SHIPMENT1.csv')
df_customer_db <- RSQLite::dbGetQuery(conn, 'Select * from SHIPMENT')
table_name <- 'SHIPMENT'
prim_key = 'shipment_id'
foreign_key = 'saddress_id'

inner_join_df <- data.frame()
insertion_df <- data.frame()
modified_df <- data.frame()
final_data <- data.frame()
valid_record <- data.frame()
invalid_record <- data.frame()
non_modified_df <- data.frame()

sanity_check_customer <- function(inp_df) {
  validated_df <- data.frame()
  bad_df <- data.frame()
  duplicated_data <- data.frame()
  
  # Duplicate entries are removed
  unique_df <- inp_df[!duplicated(inp_df$shipment_id),] 
  duplicated_df <- inp_df[duplicated(inp_df$shipment_id),]
  
  if(nrow(duplicated_df) > 0) {
    duplicated_data <- rbind(duplicated_data, duplicated_df)
    print('Following records are duplicated \n')
    print(duplicated_data)
  }
  
  inp_df <- unique_df
  n_df <- nrow(inp_df)
  
  for (i in 1:n_df) {
    if(is.na(inp_df[i, 'shipment_id']) | is.na(inp_df[i, 'shipment_status']) | is.na(inp_df[i, 'shipment_method']) | 
       is.na(inp_df[i, 'saddress_id'])) {
      bad_df <- rbind(bad_df, inp_df[i,])
    }
  }
  
  if (nrow(bad_df) > 0) {
    print('Invalid data')
    print(bad_df)
    validated_df <- anti_join(inp_df, bad_df, by = prim_key)
    return (validated_df)
  }
  return (inp_df)
}

df_customer_excel <- sanity_check_customer(df_customer_excel)

n_excel <- nrow(df_customer_excel)
n_db <- nrow(df_customer_db)

# Check for insertion or modification
inner_join_df <- merge(x = df_customer_excel, y = df_customer_db, by = prim_key)
inner_join_db_df <- merge(x = df_customer_db, y = df_customer_excel, by = prim_key)

cols <- names(df_customer_excel)
n_cols <- length(cols)
col_vec <- c(head(a, n_cols))
inner_join_df <- inner_join_df[,col_vec]
inner_join_db_df <- inner_join_db_df[,col_vec]
names(inner_join_df) <- cols

inner_join_df <- inner_join_df[order(inner_join_df$shipment_id),]
inner_join_db_df <- inner_join_db_df[order(inner_join_db_df$shipment_id),]
df_customer_db <- df_customer_db[order(df_customer_db$shipment_id),]
df_customer_excel <- df_customer_excel[order(df_customer_excel$shipment_id),]

rem_records_excel_df <- anti_join(df_customer_excel, inner_join_df, by = prim_key)

if ((nrow(inner_join_df) > 0) & (nrow(inner_join_db_df) > 0)) {
  data <- is_modified(inner_join_df, inner_join_db_df, prim_key)
  modified_df <- data$updated
  non_modified_df <- data$non_updated
}

insertion_df <- rbind(modified_df, non_modified_df)
insertion_df <- rbind(insertion_df, rem_records_excel_df)

if (nrow(insertion_df) > 0) {
  delete_table(table_name)
} 

if (nrow(insertion_df) > 0) {
  records <- check_ref_integrity(insertion_df, shipment_address_id_list, foreign_key)
  valid_record <- records$valid
  invalid_records <- records$invalid
}

if (nrow(invalid_records) > 0) {
  print('These are the records which violates foreign key constraint')
  print(invalid_records)
}

if (nrow(valid_record) > 0) {
  append_table(table_name, valid_record)
}
