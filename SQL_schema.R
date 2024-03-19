# Load required libraries
library(readr)
library(RSQLite)
library(DBI)
library(tidyverse)

# List all files
all_files <- list.files("/cloud/project/Data/")

# Loop through files to display their dimensions
for (variable in all_files) {
  this_filepath <- paste0("/cloud/project/Data/", variable)
  this_file_contents <- read_csv(this_filepath)
  
  number_of_rows <- nrow(this_file_contents)
  number_of_columns <- ncol(this_file_contents)
  
  print(paste0("The file: ", variable,
               " has: ",
               format(number_of_rows, big.mark = ","),
               " rows and ",
               number_of_columns, " columns"))
}

# Check if the first column of each file is a primary key
for (variable in all_files) {
  this_filepath <- paste0("/cloud/project/Data/", variable)
  this_file_contents <- read_csv(this_filepath)
  number_of_rows <- nrow(this_file_contents)
  
  print(paste0("Checking for: ", variable))
  
  print(paste0(" is ", nrow(unique(this_file_contents[,1])) == number_of_rows))
}

# Read the data
for (variable in all_files) {
  this_filepath <- paste0("/cloud/project/Data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  frame_name <- (gsub("\\d+\\.csv$", "", variable))
  assign(frame_name, readr::read_csv(this_filepath))
}


# Setup the connection
project <- dbConnect(RSQLite::SQLite(), "project.db")

# Create the 'customer' table
dbExecute(project, "
CREATE TABLE 'customer' (
  'customer_id' VARCHAR(200) PRIMARY KEY,
  'caddress_id' VARCHAR(200) NOT NULL,
  'first_name' VARCHAR(200) NOT NULL,
  'last_name' VARCHAR(200) NOT NULL,
  'password' VARCHAR(200) NOT NULL,
  'email' VARCHAR(200) NOT NULL UNIQUE,
  'mobile' VARCHAR(20) NOT NULL UNIQUE,
  'membership' INT,
  'age' INT,
  'gender' VARCHAR(20),
  FOREIGN KEY ('caddress_id') REFERENCES customer_address('caddress_id')
)")

# Create the 'supplier' table
dbExecute(project, "
CREATE TABLE 'supplier' (
  'supplier_id' VARCHAR(200) PRIMARY KEY,
  'supplier_name' VARCHAR(200) NOT NULL,
  'supaddress_id' VARCHAR(200) NOT NULL,
  'supplier_phone' VARCHAR(15) NOT NULL UNIQUE,
  FOREIGN KEY ('supaddress_id') REFERENCES supplier_address('supaddress_id')
)")

# Create the 'supply' table
dbExecute(project, "
CREATE TABLE 'supply' (
  'supply_id' VARCHAR(200) PRIMARY KEY,
  'supplier_id' VARCHAR(200),
  'product_id' VARCHAR(200),
  FOREIGN KEY ('supplier_id') REFERENCES supplier('supplier_id'),
  FOREIGN KEY ('product_id') REFERENCES product('product_id')
)")

# Create the 'product' table
dbExecute(project, "
CREATE TABLE 'product' (
  'product_id' VARCHAR(200) PRIMARY KEY,
  'product_name' VARCHAR(200) NOT NULL,
  'unit_price' DECIMAL(10,2) NOT NULL,
  'brand_name' VARCHAR(200),
  'color' VARCHAR(200),
  'size' VARCHAR(200),
  'category_id' INT,
  FOREIGN KEY ('category_id') REFERENCES category('category_id')
)")

# Create the 'category' table
dbExecute(project, "
CREATE TABLE 'category' (
  'category_id' VARCHAR(200) PRIMARY KEY,
  'category_name' VARCHAR(200) NOT NULL,
  'filter_by' VARCHAR(200) NOT NULL,
  'parent_id' VARCHAR(200),
  FOREIGN KEY ('parent_id') REFERENCES category_parent('parent_id')
)")

# Create the 'category_parent' table
dbExecute(project, "
CREATE TABLE 'category_parent' (
  'parent_id' INT PRIMARY KEY,
  'parent_category' VARCHAR(200) NOT NULL
)")

# Create the 'orders' table
dbExecute(project, "
CREATE TABLE 'orders' (
  'order_id' VARCHAR(200) PRIMARY KEY,
  'order_date' DATE NOT NULL,
  'customer_id' VARCHAR(200),
  'product_id' VARCHAR(200),
  'quantity' INT NOT NULL,
  'transaction_status' TEXT NOT NULL,
  'transaction_method' TEXT NOT NULL,
  FOREIGN KEY ('customer_id') REFERENCES customer('customer_id'),
  FOREIGN KEY ('product_id') REFERENCES product('product_id')
)")

# Create the 'review' table
dbExecute(project, "
CREATE TABLE 'review' (
  'review_id' VARCHAR(200) PRIMARY KEY,
  'ratings' INT NOT NULL,
  'reviews' TEXT NOT NULL,
  'product_id' VARCHAR(200),
  'review_date' DATE,
  'order_id' VARCHAR(200),
  FOREIGN KEY ('product_id') REFERENCES product('product_id'),
  FOREIGN KEY ('order_id') REFERENCES orders('order_id')
)")

# Create the 'shipment' table
dbExecute(project, "
CREATE TABLE 'shipment' (
  'shipment_id' VARCHAR(200) PRIMARY KEY,
  'shipment_status' VARCHAR(200) NOT NULL,
  'shipment_method' VARCHAR(200) NOT NULL,
  'saddress_id' VARCHAR(200) NOT NULL,
  'order_id' VARCHAR(200) NOT NULL,
  FOREIGN KEY ('saddress_id') REFERENCES shipment_address('saddress_id'),
  FOREIGN KEY ('order_id') REFERENCES orders('order_id')
)")

# Create the 'shipment_address' table
dbExecute(project, "
CREATE TABLE 'shipment_address' (
  'saddress_id' VARCHAR(200) PRIMARY KEY,
  'country' VARCHAR (200) NOT NULL,
  'city' VARCHAR (200) NOT NULL,
  'street' VARCHAR (200) NOT NULL,
  'postcode' VARCHAR (200) NOT NULL
)")

# Create the 'supplier_address' table
dbExecute(project, "
CREATE TABLE 'supplier_address' (
  'supaddress_id' VARCHAR(200) PRIMARY KEY,
  'country' VARCHAR (200) NOT NULL,
  'city' VARCHAR (200) NOT NULL,
  'street' VARCHAR (200) NOT NULL,
  'postcode' VARCHAR (200) NOT NULL
)")

# Create the 'customer_address' table
dbExecute(project, "
CREATE TABLE 'customer_address' (
  'caddress_id' VARCHAR(200) PRIMARY KEY,
  'country' VARCHAR (200) NOT NULL,
  'city' VARCHAR (200) NOT NULL,
  'street' VARCHAR (200) NOT NULL,
  'postcode' VARCHAR (200) NOT NULL
)")

# Load files into SQLite database
for (variable in all_files) {
  this_filepath <- paste0("/cloud/project/Data/",variable)
  this_file_contents <- readr::read_csv(this_filepath)
  table_name <- tolower(gsub("\\d+\\.csv$", "", variable))
  RSQLite::dbWriteTable(project,table_name, this_file_contents, append=TRUE, row.names = FALSE)
}

# Update the order_date in the orders table
dbExecute(project, "
UPDATE orders
SET order_date = DATE('1970-01-01', order_date || ' day');
")

# Update the review_date in the review table
dbExecute(project, "
UPDATE review
SET review_date = DATE('1970-01-01', review_date || ' day');
")

# Get a list of tables from the database that we already created
dbListTables(project)

<<<<<<< HEAD
=======
# Update the order_date in the orders table
dbExecute(project, "
UPDATE orders
SET order_date = DATE('1970-01-01', order_date || ' day');
")

# Update the review_date in the review table
dbExecute(project, "
UPDATE review
SET review_date = DATE('1970-01-01', review_date || ' day');
")

# Get a list of tables from the database that we already created
dbListTables(project)



>>>>>>> f2a3ba7059a12de285e7516a8f5800c4f04bb08a
# Check email format
email_check <- dbGetQuery(project, "
    SELECT * 
    FROM customer
    WHERE email NOT LIKE '%@%.%'
")

# Check mobile format
mobile_check <- dbGetQuery(project, "
    SELECT * 
    FROM customer
    WHERE mobile NOT LIKE '___-___-____'
")

# Check duplicate entries
duplicate_email_mobile <- dbGetQuery(project, "
    SELECT email, mobile, COUNT(*)
    FROM customer
    GROUP BY email, mobile
    HAVING COUNT(*) > 1
")


duplicate_supplier <- dbGetQuery(project, "
    SELECT supplier_name, supplier_phone, COUNT(*)
    FROM supplier
    GROUP BY supplier_name, supplier_phone
    HAVING COUNT(*) > 1
")

duplicate_product_name <- dbGetQuery(project, "
    SELECT product_name, COUNT(*)
    FROM product 
    GROUP BY product_name 
    HAVING COUNT(*) > 1
")

duplicate_parent_category <- dbGetQuery(project, "
    SELECT parent_category, COUNT(*)
    FROM category_parent 
    GROUP BY parent_category 
    HAVING COUNT(*) > 1
")

# Check referential integrity
referential_integrity_customer_address <- dbGetQuery(project, "
    SELECT * 
    FROM customer 
    WHERE caddress_id NOT IN (SELECT caddress_id FROM customer_address)
")

referential_integrity_supplier_address <- dbGetQuery(project, "
    SELECT * 
    FROM supplier 
    WHERE supaddress_id NOT IN (SELECT supaddress_id FROM supplier_address)
")

referential_integrity_supply <- dbGetQuery(project, "
    SELECT s.*
    FROM supply s
    LEFT JOIN supplier sp ON s.supplier_id = sp.supplier_id
    LEFT JOIN product p ON s.product_id = p.product_id
    WHERE sp.supplier_id IS NULL OR p.product_id IS NULL
")

referential_integrity_product <- dbGetQuery(project, "
    SELECT * 
    FROM product 
    WHERE category_id NOT IN (SELECT category_id FROM category)
")

referential_integrity_category <- dbGetQuery(project, "
    SELECT * 
    FROM category 
    WHERE parent_id NOT IN (SELECT parent_id FROM category_parent)
")

referential_integrity_orders <- dbGetQuery(project, "
    SELECT o.*
    FROM orders o
    LEFT JOIN customer c ON o.customer_id = c.customer_id
    LEFT JOIN product p ON o.product_id = p.product_id
    WHERE c.customer_id IS NULL OR p.product_id IS NULL
")

referential_integrity_review <- dbGetQuery(project, "
    SELECT r.*
    FROM review r
    LEFT JOIN product p ON r.product_id = p.product_id
    LEFT JOIN orders o ON r.order_id = o.order_id
    WHERE p.product_id IS NULL OR o.order_id IS NULL
")

referential_integrity_shipment <- dbGetQuery(project, "
    SELECT * 
    FROM shipment
    WHERE saddress_id NOT IN (SELECT saddress_id FROM shipment_address)
")


# Close the database connection when done
dbDisconnect(project)
