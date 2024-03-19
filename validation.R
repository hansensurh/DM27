library(readr)
library(RSQLite)


### Customer Entity
CUSTOMER <- read_csv("data/customer_data.csv")
# Customer first and last name validation

# Define the validation function for names
validate_name <- function(name) {
  pattern <- "^[A-Za-z ]+$"  # Only letters and spaces allowed
  grepl(pattern, name)
}
valid_first_name <- sapply(CUSTOMER$first_name, validate_name)
valid_last_name <- sapply(CUSTOMER$last_name, validate_name)

# Customer mobile validation
# Define the validation function for mobile numbers
validate_mobile <- function(mobile) {
  pattern <- "^\\d{3}-\\d{3}-\\d{4}$"  # Pattern: XXX-XXX-XXXX
  grepl(pattern, mobile)
}
valid_customer_mobile <- sapply(CUSTOMER$mobile, validate_mobile)

# Customer membership validation
validate_membership <- function(membership) {
  # Check if the value is Yes or No
  is_valid <- membership == "Yes" | membership == "No"
  return(is_valid)
}

# Apply the validation function to each value in the CUSTOMER$membership column
valid_membership <- sapply(CUSTOMER$membership, validate_membership)

# Customer gender validation
validate_gender <- function(gender) {
  # Check if the value is Female or Male
  is_valid <- gender == "Female" | gender == "Male"
}
valid_gender <- sapply(CUSTOMER$gender, validate_gender)

# Customer age validation
validate_age <- function(age) {
  # Check if age is between 10 and 99 (inclusive)
  is_valid_age <- age >= 10 & age <= 99
}

# Apply the validation function to the age column
valid_ages <- sapply(CUSTOMER$age, validate_age)

# Check to see if customer_id is distinct
duplicated_cus_ids <- duplicated(CUSTOMER$customer_id) | duplicated(CUSTOMER$customer_id, fromLast = TRUE)

unique_cus_ids <- !duplicated_cus_ids

# Check to see if password is distinct
duplicated_pass <- duplicated(CUSTOMER$password) | duplicated(CUSTOMER$password, fromLast = TRUE)

unique_pass <- !duplicated_pass

# Check to see if email id is distinct and correct format
validate_email_format <- function(emails) {
  pattern <- "^[a-zA-Z0-9]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  grepl(pattern, emails)
}
valid_emails <- sapply(CUSTOMER$email, validate_email_format)

# Remove invalid data 
# Create a list of all your validation vectors
cus_validations_list <- list(valid_first_name, valid_last_name, valid_customer_mobile, 
                             valid_membership, valid_gender, valid_ages, 
                             unique_cus_ids, valid_emails, unique_pass)

# Use Reduce to apply the & operator across all elements of the list
cus_all_valid <- Reduce("&", cus_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(cus_all_valid) > 0) {
  # If there are valid rows, update the SUPPLIER dataframe to only include those rows
  CUSTOMER <- CUSTOMER[cus_all_valid, ]
} else {
  message("No valid rows found. The SUPPLIER dataframe will not be updated.")
}

### Supplier Entity
SUPPLIER <- read_csv("data/supplier_data.csv")

# Check to see if supplier_id is distinct
duplicated_sup_ids <- duplicated(SUPPLIER$supplier_id) | duplicated(SUPPLIER$supplier_id, fromLast = TRUE)

unique_sup_ids <- !duplicated_sup_ids

# Validation for supplier name
valid_supplier_name <- sapply(SUPPLIER$supplier_name, validate_name)

valid_sup_phone <- sapply(SUPPLIER$supplier_phone, validate_mobile)

# Check to see if supaddress_id is distinct
duplicated_sup_add_ids <- duplicated(SUPPLIER$supaddress_id) | duplicated(SUPPLIER$supadress_id, fromLast = TRUE)

unique_sup_add_ids <- !duplicated_sup_add_ids

# Create a list of all your validation vectors
sup_validations_list <- list(unique_sup_add_ids, valid_sup_phone, unique_sup_ids, valid_supplier_name)

# Use Reduce to apply the & operator across all elements of the list
sup_all_valid <- Reduce("&", sup_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(sup_all_valid) > 0) {
  # If there are valid rows, update the SUPPLIER dataframe to only include those rows
  SUPPLIER <- SUPPLIER[sup_all_valid, ]
} else {
  message("No valid rows found. The SUPPLIER dataframe will not be updated.")
}

# Category Entity
CATEGORY <- read_csv("data/category_data.csv")

# Check to see if category is distinct
duplicated_cat_ids <- duplicated(CATEGORY$category_id) | duplicated(CATEGORY$category_id, fromLast = TRUE)

unique_cat_ids <- !duplicated_cat_ids

# Define the allowed category names
allowed_categories <- c("Books", "Sports Equipment", "Beauty Products", 
                        "Home Decor", "Toys", "Pet Supplies", 
                        "Clothing", "Electronics", "Others")

# Define the validation function for category names
validate_category_name <- function(category_name) {
  # Check if category name is only alphabets (and spaces)
  pattern <- "^[A-Za-z ]+$"
  
  # Check if the category name matches the pattern and is in the allowed list
  is_valid_pattern <- grepl(pattern, category_name)
  is_valid_category <- category_name %in% allowed_categories
  
  # Both conditions must be TRUE
  return(is_valid_pattern & is_valid_category)
}

# Apply the validation function to each value in the CATEGORY$category_name column
valid_category_names <- sapply(CATEGORY$category_name, validate_category_name)

# Define the allowed filter_by 
allowed_filter <- c("Any", "Female", "Male")

# Define the validation function for filter_by
validate_filter_by <- function(filter_by) {
  # Check if the filter_by is in the allowed list
  is_valid <- filter_by %in% allowed_filter
  return(is_valid)
}

# Apply the validation function to each value in the CATEGORY$category_name column
valid_filter_by <- sapply(CATEGORY$filter_by, validate_filter_by)

# Define the allowed parent_id 
allowed_parent <- c("101", "102", "103", "104", "105")

# Define the validation function for parent_id
validate_parent <- function(parent_id) {
  # Check if the parent is in the allowed list
  is_valid <- parent_id %in% allowed_parent
  return(is_valid)
}

# Apply the validation function to each value in the CATEGORY$parent_id column
valid_parent <- sapply(CATEGORY$parent_id, validate_parent)

# Create a list of all your validation vectors
cat_validations_list <- list(unique_cat_ids, valid_parent, valid_filter_by, valid_category_names)

# Use Reduce to apply the & operator across all elements of the list
cat_all_valid <- Reduce("&", cat_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(cat_all_valid) > 0) {
  # If there are valid rows, update the CATEGORY dataframe to only include those rows
  CATEGORY <- CATEGORY[cat_all_valid, ]
} else {
  message("No valid rows found. The CATEGORY dataframe will not be updated.")
}

### Category Parent 
CATEGORY_PARENT <- read_csv("data/category_parent_data.csv")

# Check to see if parent_id is distinct
duplicated_cat_parent_ids <- duplicated(CATEGORY_PARENT$parent_id) | duplicated(CATEGORY_PARENT$parent_id, fromLast = TRUE)

unique_cat_parent_ids <- !duplicated_cat_parent_ids

# Define the allowed parent_category 
allowed_parent_cat <- c("Home & Lifestyle", "Sports & Outdoor Activities", "Fashion & Beauty", "Electronics & Technology", "Others")

# Define the validation function for parent_id
validate_parent_cat <- function(parent_category) {
  # Check if the parent is in the allowed list
  is_valid <- parent_category %in% allowed_parent_cat
  return(is_valid)
}

# Apply the validation function to each value in the CATEGORY$parent_id column
valid_parent_cat <- sapply(CATEGORY_PARENT$parent_category, validate_parent_cat)

# Create a list of all your validation vectors
cat_parent_validations_list <- list(valid_parent_cat, unique_cat_parent_ids)

# Use Reduce to apply the & operator across all elements of the list
cat_parent_all_valid <- Reduce("&", cat_parent_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(cat_parent_all_valid) > 0) {
  # If there are valid rows, update the CATEGORY_PARENT dataframe to only include those rows
  CATEGORY_PARENT <- CATEGORY_PARENT[cat_parent_all_valid, ]
} else {
  message("No valid rows found. The CATEGORY_PARENT dataframe will not be updated.")
}

### Address 
CUSTOMER_ADDRESS <- read_csv("data/customer_address_data.csv")
SUPPLIER_ADDRESS <- read_csv("data/supplier_address_data.csv")
SHIPPING_ADDRESS <- read_csv("data/shipment_address_data.csv")

# Check to see if all address is distinct
# Customer address
duplicated_cus_add_ids <- duplicated(CUSTOMER_ADDRESS$caddress_id) | duplicated(CUSTOMER_ADDRESS$caddress_id, fromLast = TRUE)

unique_cus_add_ids <- !duplicated_cus_add_ids

# Shipping address
duplicated_ship_add_ids <- duplicated(SHIPPING_ADDRESS$saddress_id) | duplicated(SHIPPING_ADDRESS$saddress_id, fromLast = TRUE)

unique_ship_add_ids <- !duplicated_ship_add_ids

# Supplier address
duplicated_sup_add_id <- duplicated(SUPPLIER_ADDRESS$supaddress_id) | duplicated(SUPPLIER_ADDRESS$supaddress_id, fromLast = TRUE)

unique_sup_add_id <- !duplicated_sup_add_id

# Validate country
valid_cus_country <- sapply(CUSTOMER_ADDRESS$country, validate_name)
valid_sup_country <- sapply(SUPPLIER_ADDRESS$country, validate_name)
valid_ship_country <- sapply(SHIPPING_ADDRESS$country, validate_name)

# Validate city
valid_cus_city <- sapply(CUSTOMER_ADDRESS$city, validate_name)
valid_sup_city <- sapply(SUPPLIER_ADDRESS$city, validate_name)
valid_ship_city <- sapply(SHIPPING_ADDRESS$city, validate_name)

# Validate postcode ??

# Validate street
valid_cus_street <- sapply(CUSTOMER_ADDRESS$street, validate_name)
valid_sup_street <- sapply(SUPPLIER_ADDRESS$street, validate_name)
valid_ship_street <- sapply(SHIPPING_ADDRESS$street, validate_name)

# Create a list of all your validation vectors
cus_add_validations_list <- list(valid_cus_street, valid_cus_city, valid_cus_country, unique_cus_add_ids)

# Use Reduce to apply the & operator across all elements of the list
cus_add_all_valid <- Reduce("&", cus_add_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(cus_add_all_valid) > 0) {
  # If there are valid rows, update the CUSTOMER_ADDRESS dataframe to only include those rows
  CUSTOMER_ADDRESS <- CUSTOMER_ADDRESS[cus_add_all_valid, ]
} else {
  message("No valid rows found. The CUSTOMER_ADDRESS dataframe will not be updated.")
}

# Create a list of all your validation vectors
sup_add_validations_list <- list(valid_sup_street, valid_sup_city, valid_sup_country, unique_sup_add_id)

# Use Reduce to apply the & operator across all elements of the list
sup_add_all_valid <- Reduce("&", sup_add_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(sup_add_all_valid) > 0) {
  # If there are valid rows, update the SUPPLIER_ADDRESS dataframe to only include those rows
  SUPPLIER_ADDRESS <- SUPPLIER_ADDRESS[sup_add_all_valid, ]
} else {
  message("No valid rows found. The SUPPLIER_ADDRESS dataframe will not be updated.")
}

# Create a list of all your validation vectors
ship_add_validations_list <- list(valid_ship_street, valid_ship_city, valid_ship_country, unique_ship_add_ids)

# Use Reduce to apply the & operator across all elements of the list
ship_add_all_valid <- Reduce("&", ship_add_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(ship_add_all_valid) > 0) {
  # If there are valid rows, update the SHIPPING_ADDRESS dataframe to only include those rows
  SHIPPING_ADDRESS <- SHIPPING_ADDRESS[ship_add_all_valid, ]
} else {
  message("No valid rows found. The SHIPPING_ADDRESS dataframe will not be updated.")
}

### Supply
SUPPLY <- read_csv("data/supply_data.csv")

# Supply id
duplicated_supply_ids <- duplicated(SUPPLY$supply_id) | duplicated(SUPPLY$supply_id, fromLast = TRUE)

unique_supply_ids <- !duplicated_supply_ids

# Create a list of all your validation vectors
supply_validations_list <- list(unique_supply_ids)

# Use Reduce to apply the & operator across all elements of the list
supply_all_valid <- Reduce("&", supply_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(supply_all_valid) > 0) {
  # If there are valid rows, update the SUPPLY dataframe to only include those rows
  SUPPLY <- SUPPLY[supply_all_valid, ]
} else {
  message("No valid rows found. The SUPPLY dataframe will not be updated.")
}

### Product Entity
PRODUCT <- read_csv("data/product_data.csv")

# product id
duplicated_prod_ids <- duplicated(PRODUCT$product_id) | duplicated(PRODUCT$product_id, fromLast = TRUE)

unique_prod_ids <- !duplicated_prod_ids

# Category id
# Define the category id
allowed_cat_id <- c("1001-1", "1002-1", "1003-1", "1003-2", "1003-3", "1004-1", "1005-1", "1006-1", "1007-1", "1007-2", "1008-1", "1009-1")

# Define the validation function for category id
validate_cat_id <- function(category_id){
  # Check if the id is in the allowed list
  is_valid <- category_id %in% allowed_cat_id
  return(is_valid)
}

# Apply the validation function to each value in the PRODUCT$category_id column
valid_cat_id <- sapply(PRODUCT$category_id, validate_cat_id)

# Product name
valid_prod_name <- sapply(PRODUCT$product_name, validate_name)

# Brand name
valid_brand_name <- sapply(PRODUCT$brand_name, validate_name)

# Product color
valid_color <- sapply(PRODUCT$color, validate_name)

# Product size
# Define the allowed size
allowed_prod_size <- c("S", "M", "XS", "L", "XL", "Free", "2XL", "3XL", "Default")

# Define the validation function for shipment method
validate_prod_size <- function(size){
  # Check if the size is in the allowed list
  is_valid <- size %in% allowed_prod_size
  return(is_valid)
}

# Apply the validation function to each value in the PRODUCT$size column
valid_prod_size <- sapply(PRODUCT$size, validate_prod_size)

# Define the validation function for unit_price
validate_unit_price <- function(unit_price) {
  # Regular expression pattern to match a number that may include a decimal point
  pattern <- "^\\d+(\\.\\d+)?$"
  return(grepl(pattern, unit_price))
}

# Apply the validation function to each value in the PRODUCT$unit_price column
valid_unit_prices <- sapply(PRODUCT$unit_price, validate_unit_price)

# Create a list of all your validation vectors
product_validations_list <- list(valid_unit_prices, valid_cat_id, valid_prod_size, valid_color, valid_brand_name, valid_prod_name, unique_prod_ids)

# Use Reduce to apply the & operator across all elements of the list
product_all_valid <- Reduce("&", product_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(product_all_valid) > 0) {
  # If there are valid rows, update the PRODUCT dataframe to only include those rows
  PRODUCT <- PRODUCT[product_all_valid, ]
} else {
  message("No valid rows found. The PRODUCT dataframe will not be updated.")
}

### Order 
ORDERS <- read_csv("data/orders_data.csv")

# Order id
duplicated_ord_ids <- duplicated(ORDERS$order_id) | duplicated(ORDERS$order_id, fromLast = TRUE)

unique_ord_ids <- !duplicated_ord_ids

# Quantity
# Define the validation function for quantity
validate_quantity <- function(quantity) {
  # Check if the value is numeric and a whole number
  is_valid <- is.numeric(quantity) && quantity == round(quantity)
  return(is_valid)
}

# Apply the validation function to each value in the ORDER$quantity column
valid_quantities <- sapply(ORDERS$quantity, validate_quantity)

# Order date
# Define the validation function for order_date in YYYY-MM-DD format
validate_order_date <- function(order_date) {
  # Regular expression pattern to match the "YYYY-MM-DD" format
  pattern <- "^\\d{4}-\\d{2}-\\d{2}$"
  
  # First, check if the order_date matches the pattern
  if(grepl(pattern, order_date)) {
    # Try to convert the string to a Date object to ensure it's a valid date
    parsed_date <- as.Date(order_date, format="%Y-%m-%d")
    
    # Check if the conversion was successful by comparing it to NA
    return(!is.na(parsed_date))
  } else {
    # If the pattern does not match, immediately return FALSE
    return(FALSE)
  }
}

# Apply the validation function to each value in the ORDER$order_date column
valid_order_date <- sapply(ORDERS$order_date, validate_order_date)

# Transaction status
# Define the allowed transaction status
allowed_trans_status <- c("cancelled", "completed", "in progress", "pending", "failed")

# Define the validation function for shipment method
validate_trans_status <- function(transaction_status) {
  # Check if the status is in the allowed list
  is_valid <- transaction_status %in% allowed_trans_status
  return(is_valid)
}

# Apply the validation function to each value in the ORDERS$transaction_status column
valid_trans_status <- sapply(ORDERS$transaction_status, validate_trans_status)

# Transaction method
# Define the allowed transaction status
allowed_trans_method <- c("debit card", "credit card", "Venmo", "Apple Pay", "PayPal", "Google Pay")

# Define the validation function for shipment method
validate_trans_method <- function(transaction_method) {
  # Check if the method is in the allowed list
  is_valid <- transaction_method %in% allowed_trans_method
  return(is_valid)
}

# Apply the validation function to each value in the ORDERS$transaction_method column
valid_trans_method <- sapply(ORDERS$transaction_method, validate_trans_method)

# Create a list of all your validation vectors
orders_validations_list <- list(valid_trans_method, valid_trans_status, valid_order_date, valid_quantities, unique_ord_ids)

# Use Reduce to apply the & operator across all elements of the list
orders_all_valid <- Reduce("&", orders_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(orders_all_valid) > 0) {
  # If there are valid rows, update the ORDERS dataframe to only include those rows
  ORDERS <- ORDERS[orders_all_valid, ]
} else {
  message("No valid rows found. The ORDERS dataframe will not be updated.")
}

### Shipment Entity
SHIPMENT <- read_csv("data/shipment_data.csv")

# Shipment id
duplicated_ship_ids <- duplicated(SHIPMENT$shipment_id) | duplicated(SHIPMENT$shipment_id, fromLast = TRUE)

unique_ship_ids <- !duplicated_ship_ids

# Shipment address id
duplicated_ship_add_id <- duplicated(SHIPMENT$saddress_id) | duplicated(SHIPMENT$saddress_id, fromLast = TRUE)

unique_ship_add_id <- !duplicated_ship_add_id

# Order id
duplicated_ord_id <- duplicated(SHIPMENT$order_id) | duplicated(SHIPMENT$order_id, fromLast = TRUE)

unique_ord_id <- !duplicated_ord_id

# Shipment method
# Define the allowed shipment method
allowed_ship_method <- c("parcel", "overnight", "truckload", "courier", "airmail", "priority", "standard", "express", "freight", "ground")

# Define the validation function for shipment method
validate_ship_method <- function(shipment_method) {
  # Check if the method is in the allowed list
  is_valid <- shipment_method %in% allowed_ship_method
  return(is_valid)
}

# Apply the validation function to each value in the SHIPMENT$shipment_method column
valid_ship_method <- sapply(SHIPMENT$shipment_method, validate_ship_method)

# Define the allowed shipment status
allowed_ship_status <- c("shipped", "returned", "delivered", "out for delivery", "delayed")

# Define the validation function for shipment_status
validate_ship_status <- function(shipment_status) {
  # Check if the status is in the allowed list
  is_valid <- shipment_status %in% allowed_ship_status
  return(is_valid)
}

# Apply the validation function to each value in the SHIPMENT$shipment_status column
valid_ship_status <- sapply(SHIPMENT$shipment_status, validate_ship_status)

# Create a list of all your validation vectors
shipment_validations_list <- list(valid_ship_status, valid_ship_method, unique_ord_id, unique_ship_add_id, unique_ship_ids)

# Use Reduce to apply the & operator across all elements of the list
shipment_all_valid <- Reduce("&", shipment_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(shipment_all_valid) > 0) {
  # If there are valid rows, update the SHIPMENT dataframe to only include those rows
  SHIPMENT <- SHIPMENT[shipment_all_valid, ]
} else {
  message("No valid rows found. The SHIPMENT dataframe will not be updated.")
}


### Review
REVIEW <- read_csv("data/review_data.csv")

# review id
duplicated_review_ids <- duplicated(REVIEW$review_id) | duplicated(REVIEW$review_id, fromLast = TRUE)

unique_review_ids <- !duplicated_review_ids

# order date
valid_review_dates <- sapply(REVIEW$review_date, validate_order_date)

# Ratings 
# Define the validation function for ratings
validate_ratings <- function(ratings) {
  # Check if the rating is between 1 and 5, inclusive
  is_valid <- ratings >= 1 & ratings <= 5
  return(is_valid)
}

# Apply the validation function to each value in the REVIEW$ratings column
valid_ratings <- sapply(REVIEW$ratings, validate_ratings)

# Create a list of all your validation vectors
review_validations_list <- list(valid_ratings, valid_review_dates, unique_review_ids)

# Use Reduce to apply the & operator across all elements of the list
review_all_valid <- Reduce("&", review_validations_list)

# Create a new dataframe excluding rows that fail any validation
if (length(review_all_valid) > 0) {
  # If there are valid rows, update the REVIEW dataframe to only include those rows
  REVIEW <- REVIEW[review_all_valid, ]
} else {
  message("No valid rows found. The REVIEW dataframe will not be updated.")
}


### Testing for review validation
new_row <- data.frame(
  review_id = "c4032ded-5bd9-4e67-923c-f7660d2d911a",
  review_date = "2024-10-02",
  reviews = "Excellent product",
  product_id = "7DR2E19WJ60",
  ratings = 6,
  order_id = "03718afe-c1bb-4b2b-a4cc-837087a6bf6s",
  stringsAsFactors = FALSE
)

# Add the new row to the REVIEW dataframe
REVIEW <- rbind(REVIEW, new_row)












