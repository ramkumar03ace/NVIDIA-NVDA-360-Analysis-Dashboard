  setwd("D:/VIT/VIT Sem 7/PDS/Project")
  
  # --- 0. Load all necessary libraries ---
  library(rvest)        # For web scraping
  library(dplyr)        # For data manipulation (select, bind_rows, filter)
  library(googlesheets4) # For writing to your Google Sheet
  
  # --- 1. Scrape (Improved Method) ---
  url <- "https://finviz.com/quote.ashx?t=NVDA"
  page <- read_html(url)
  
  # Use the specific CSS class ".snapshot-table2" to find the *exact* table.
  # This is much more robust than guessing the index or checking dimensions.
  finviz_table <- page %>% 
    html_node(".snapshot-table2") %>% 
    html_table()
  
  # Add an error check in case the class name ever changes
  if (is.null(finviz_table)) {
    stop("Error: Could not find the table with class '.snapshot-table2'. 
          The website layout may have changed. Please inspect the page.")
  }
  
  # --- FILLED GAP 2: Clean and Wrangle the Data ---
  # The table is in a 'wide' 6x2 format (6 pairs of Metric/Value columns)
  # We need to stack them into a 'long' 2-column format.
  # This logic remains correct.
  
  # Select the first pair of columns (1 and 2)
  df1 <- finviz_table %>% select(Metric = X1, Value = X2)
  
  # Select the second pair (3 and 4)
  df2 <- finviz_table %>% select(Metric = X3, Value = X4)
  
  # Select the third pair (5 and 6)
  df3 <- finviz_table %>% select(Metric = X5, Value = X6)
  
  # Select the fourth pair (7 and 8)
  df4 <- finviz_table %>% select(Metric = X7, Value = X8)
  
  # Select the fifth pair (9 and 10)
  df5 <- finviz_table %>% select(Metric = X9, Value = X10)
  
  # Select the sixth pair (11 and 12)
  df6 <- finviz_table %>% select(Metric = X11, Value = X12)
  
  # Bind all 6 data frames together into one long, clean data frame
  finviz_clean <- bind_rows(df1, df2, df3, df4, df5, df6) %>%
    filter(!is.na(Metric), Metric != "") # Clean up any empty rows just in case
  
  # --- 3. Write to Google Sheet ---
  
  # Let's look at our clean data first
  print("Scraping and cleaning successful. Here's the data:")
  print(finviz_clean)
  
  # IMPORTANT:
  # 1. Make sure you have created a Google Sheet named "NVIDIA Data Project".
  # 2. Make sure it has a tab (sheet) named "finviz_data".
  # 3. Make sure you have run gs4_auth() in your console to log in.
  
  # --- FIX FOR THE ERROR ---
  # The error "Invalid input: ✖ NVIDIA Data Project" means you MUST use
  # the Google Sheet ID, not its name.
  # 1. Go to your "NVIDIA Data Project" Google Sheet.
  # 2. Copy the ID from the URL.
  #    Example URL: https://docs.google.com/spreadsheets/d/1aBcD-eFgH_..._kLmNoP/edit
  #    The ID is the long part in the middle: "1aBcD-eFgH_..._kLmNoP"
  # 3. Paste that ID string below, replacing "PASTE_YOUR_SHEET_ID_HERE".
  
  # After you've done that, you can uncomment and run the line below:
  sheet_write(finviz_clean, # Use the CLEAN data, not the original
             ss = "1-5oHCZcxrLarT31dh6xhRMggpvTVnc8erqr6jqiNe7w",
             sheet = "finviz_data")
  
  print("Successfully wrote clean data to Google Sheets!")
  
  
  
