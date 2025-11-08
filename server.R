#setwd("D:/VIT/VIT Sem 7/PDS/Project")
# Authenticate using a cached token in the ".secrets" folder

# --- Seminar 3: server.R (The "Back-End") ---
# This file runs all the R code to power the app.

# --- 0. Load ALL Libraries ---
# (Install any you don't have yet)
# install.packages(c("shiny", "plotly", "DT", "rsconnect", "purrr"))

library(shiny)
library(ggplot2)
library(plotly)        # For renderPlotly()
library(DT)            # For renderDT()
library(googlesheets4) # For sheet_append() and range_read()
library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(tidyquant)
library(reshape2)      # For acast()
library(tidyr)         # For pivot_longer()

library(purrr)         # For map()
library(lubridate)     # For months()

library(shinyjs)       # <-- ADD THIS (for show/hide buttons)
library(uuid)          # <-- ADD THIS (for UUIDgenerate)

gs4_auth(cache = ".secrets", email = "ramkumar.arcot2022@vitstudent.ac.in")

# --- 1. Define Server Logic ---
server <- function(input, output, session) {
  
  # --- IMPORTANT: PASTE YOUR SHEET ID HERE ---
  YOUR_SHEET_ID <- "1-5oHCZcxrLarT31dh6xhRMggpvTVnc8erqr6jqiNe7w" 
  
  # --- 2. Reactive Data Loading & Analysis ---
  # We use reactive() so data is loaded once when the app starts
  
  # A. Load and process Reddit data
  reddit_data <- reactive({
    print("Loading and processing Reddit data...")
    reddit_df <- range_read(ss = YOUR_SHEET_ID, sheet = "reddit_comments")
    bing_sentiments <- get_sentiments("bing")
    
    reddit_words <- reddit_df %>%
      unnest_tokens(output = word, input = comment) %>%
      anti_join(stop_words)
    
    # Data for Sentiment Bar Chart
    sentiment_counts <- reddit_words %>%
      inner_join(bing_sentiments, by = "word") %>%
      count(sentiment)
    
    # Data for Word Cloud
    sentiment_word_counts <- reddit_words %>%
      inner_join(bing_sentiments) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    list(counts = sentiment_counts, word_counts = sentiment_word_counts)
  })
  
  # B. Load and process PDF data
  pdf_data <- reactive({
    print("Loading and processing PDF data...")
    pdf_df <- range_read(ss = YOUR_SHEET_ID, sheet = "pdf_text")
    
    keywords_to_find <- c("AI", "Data_Center", "Gaming", "Automotive", "Risk")
    
    count_keywords <- function(text) {
      data.frame(
        AI = str_count(text, regex("AI|Artificial Intelligence", ignore_case = TRUE)),
        Data_Center = str_count(text, regex("Data Center", ignore_case = TRUE)),
        Gaming = str_count(text, regex("Gaming|GeForce", ignore_case = TRUE)),
        Automotive = str_count(text, regex("Automotive", ignore_case = TRUE)),
        Risk = str_count(text, regex("Risk", ignore_case = TRUE))
      )
    }
    
    total_keyword_counts <- pdf_df %>%
      mutate(counts = purrr::map(text, count_keywords)) %>%
      unnest(counts) %>%
      summarise(across(all_of(keywords_to_find), sum)) %>%
      pivot_longer(cols = everything(), names_to = "Keyword", values_to = "Count")
    
    return(total_keyword_counts)
  })
  
  # C. Load and process Stock data (ONCE)
  # We load this data *once* when the app starts.
  print("Loading and processing Stock data... (This should only appear ONCE)")
  all_stock_data <- tq_get("NVDA", from = "1999-01-22", to = Sys.Date())
  
  # D. Load and process Finviz data
  finviz_data <- reactive({
    print("Loading Finviz data...")
    range_read(ss = YOUR_SHEET_ID, sheet = "finviz_data")
  })
  
  # --- 3. Render Visuals ---
  
  # --- Tab 1: Stock Performance ---
  output$stock_price_plot <- renderPlotly({
    
    # Get the full dataset (from the variable we created)
    all_data <- all_stock_data 
    today <- Sys.Date()
    
    # 1. Determine the start date based on the user's input
    start_date <- if (input$stock_time_range == "1 Year") {
      today - 365
    } else if (input$stock_time_range == "2 Years") {
      today - (365 * 2)
    } else if (input$stock_time_range == "3 Years") {
      today - (365 * 3)
    } else if (input$stock_time_range == "5 Years") {
      today - (365 * 5)
    } else {
      min(all_data$date) # "All Time" uses the earliest date we loaded
    }
    
    # 2. Filter the data based on the start date
    plot_data <- all_data %>%
      filter(date >= start_date)
    
    # 3. Create the plot using the newly filtered 'plot_data'
    p <- ggplot(plot_data, aes(x = date, y = adjusted)) +
      geom_line(color = "darkgreen") +
      labs(title = "NVIDIA (NVDA) Stock Price (Adjusted)", x = "Date", y = "Adjusted Close Price ($)") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar)
    
    ggplotly(p) # Make it interactive!
  })
  
  output$candlestick_plot <- renderPlotly({
    
    # 1. Filter the data (using the 180-day filter)
    #    This also uses the fast 'all_stock_data' variable
    recent_prices <- all_stock_data %>%
      filter(date >= (Sys.Date() - 180)) 
    
    # 2. Create the plot using plot_ly's native candlestick function
    p <- plot_ly(recent_prices, x = ~date, type="candlestick",
                 open = ~open, close = ~close, high = ~high, low = ~low,
                 increasing = list(line = list(color = 'darkgreen')),
                 decreasing = list(line = list(color = 'red'))) %>%
      
      layout(title = "NVIDIA (NVDA) 6-Month Candlestick Chart",
             yaxis = list(title = "Stock Price ($)", tickformat = "$.2f"),
             xaxis = list(title = "Date", rangeslider = list(visible = FALSE))) # Hides the small slider at the bottom
    
    return(p) # Return the plotly object directly
  })
  
  # --- Tab 2: Sentiment Analysis ---
  output$wordcloud_plot <- renderPlot({
    # Wordcloud uses a special plot function, not ggplot
    sentiment_word_counts <- reddit_data()$word_counts
    
    sentiment_word_counts %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(
        colors = c("red", "darkgreen"), # negative, positive
        max.words = 100,
        title.size = 2
      )
  })
  
  output$sentiment_bar_plot <- renderPlotly({
    sentiment_counts <- reddit_data()$counts
    
    p <- ggplot(sentiment_counts, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +
      labs(title = "Overall Sentiment Score", x = "Sentiment", y = "Total Word Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) # Make it interactive!
  })
  
  # --- Tab 3: Annual Report Insights ---
  output$keyword_plot <- renderPlotly({
    
    keyword_counts <- pdf_data()
    
    # Create the treemap using plot_ly's native function
    p <- plot_ly(
      data = keyword_counts,
      labels = ~Keyword,  # The text for each box
      parents = NA,       # No hierarchy, so all are at the top level
      values = ~Count,    # The size of each box
      type = 'treemap',
      # Show the label (e.g., "AI") and the value (e.g., "500")
      textinfo = "label+value", 
      # What to show on hover
      hoverinfo = "label+value+percent parent" 
    ) %>%
      layout(title = "Keyword Frequency in 2025 Annual Report") # Add a title
    
    return(p) # Return the plotly object directly
  })
  
  # --- Tab 4: Key Financials ---
  output$finviz_table <- renderDT({
    datatable(
      finviz_data(),
      rownames = FALSE,
      options = list(
        pageLength = 15,    # Show 15 rows at a time
        scrollY = "400px"   # Make the table scrollable
      )
    )
  })
  
  # --- 4. CRUD Functionality (Full CRUD) ---
  
  # --- 0. Load new libraries (add to top of server.R) ---
  # library(shinyjs)
  # library(uuid)
  
  # --- 1. Helper Function and Reactive Trigger ---
  
  # This reactiveVal is a "trigger" that tells the table to refresh
  load_ratings_trigger <- reactiveVal(runif(1))
  
  # A helper function to clear the input form
  clear_form <- function() {
    updateDateInput(session, "rating_date", value = Sys.Date())
    updateTextInput(session, "rating_source", value = "")
    updateSelectInput(session, "rating_action", selected = "Buy")
    updateNumericInput(session, "rating_target", value = 150)
  }
  
  # --- 2. R (Read): Read and Display Data ---
  
  # R (Read): Read the ratings from the Google Sheet
  ratings_data <- reactive({
    load_ratings_trigger() # Depend on the trigger
    print("Reading analyst ratings from Google Sheet...")
    range_read(ss = YOUR_SHEET_ID, sheet = "analyst_ratings", col_types = "Dccnc")
  })
  
  # Display the ratings in the table
  output$ratings_table <- DT::renderDataTable({
    df <- ratings_data()
    datatable(
      df,
      rownames = FALSE,
      selection = 'single', # Allow only one row to be selected
      options = list(
        order = list(list(0, 'desc')), # Show newest ratings first
        # Hide the 'rating_id' column (column index 4, since R is 0-indexed in JS)
        columnDefs = list(list(visible = FALSE, targets = c(4)))
      )
    )
  })
  
  # --- 3. C (Create): Submit a New Rating ---
  
  # Show the "Submit" form when "Add New" is clicked
  observeEvent(input$add_new_btn, {
    clear_form()
    shinyjs::show("submit_div")
    shinyjs::hide("update_div")
    shinyjs::hide("add_new_div")
  })
  
  # C (Create): When the submit button is pressed...
  observeEvent(input$submit_rating, {
    print("Submit button pressed. Appending to Google Sheet...")
    
    # Create a 1-row data frame with the new unique ID
    new_rating <- data.frame(
      date = input$rating_date,
      source = input$rating_source,
      rating = input$rating_action,
      price_target = input$rating_target,
      rating_id = UUIDgenerate() # Generate a unique ID
    )
    
    # Append this row to the 'analyst_ratings' tab
    sheet_append(ss = YOUR_SHEET_ID, data = new_rating, sheet = "analyst_ratings")
    
    # Pull the trigger to refresh the table
    load_ratings_trigger(runif(1))
    
    # Clear and hide the form
    clear_form()
    shinyjs::hide("submit_div")
    shinyjs::show("add_new_div")
  })
  
  # --- 4. U (Update): Load, Update, and Cancel ---
  
  # This stores the Google Sheet row number of the selected item
  selected_row_data <- reactiveVal(NULL)
  
  # U (Load): Load data into the form for editing
  observeEvent(input$load_for_edit, {
    row_index <- input$ratings_table_rows_selected
    if (is.null(row_index)) {
      showModal(modalDialog(title = "Error", "Please select a row to edit."))
      return()
    }
    
    # Get the data from the selected row
    selected_data <- ratings_data()[row_index, ]
    
    # Fill the form with this data
    updateDateInput(session, "rating_date", value = selected_data$date)
    updateTextInput(session, "rating_source", value = selected_data$source)
    updateSelectInput(session, "rating_action", selected = selected_data$rating)
    updateNumericInput(session, "rating_target", value = selected_data$price_target)
    
    # Store this data for the Update step
    selected_row_data(selected_data)
    
    # Show the "Update" buttons and hide "Submit"
    shinyjs::show("update_div")
    shinyjs::hide("submit_div")
    shinyjs::hide("add_new_div")
  })
  
  # U (Cancel): Cancel the edit
  observeEvent(input$cancel_edit, {
    clear_form()
    selected_row_data(NULL)
    shinyjs::hide("update_div")
    shinyjs::show("add_new_div")
  })
  
  # U (Update): When the "Update" button is pressed...
  observeEvent(input$update_rating, {
    # Get the unique ID of the row we are editing
    selected_id <- selected_row_data()$rating_id
    if (is.null(selected_id)) return()
    
    print(paste("Updating row with ID:", selected_id))
    
    # Read all sheet data to find the *Google Sheet row number*
    all_data <- range_read(ss = YOUR_SHEET_ID, sheet = "analyst_ratings", col_types = "Dccnc")
    row_to_update <- which(all_data$rating_id == selected_id)
    
    if (length(row_to_update) == 0) {
      showModal(modalDialog(title = "Error", "Could not find row to update. Please refresh."))
      return()
    }
    
    # Google Sheet rows are 1-indexed, +1 for the header
    sheet_row_number <- row_to_update + 1
    
    # Create the updated data frame (keeping the same ID)
    updated_data <- data.frame(
      date = input$rating_date,
      source = input$rating_source,
      rating = input$rating_action,
      price_target = input$rating_target,
      rating_id = selected_id
    )
    
    # Write OVER the old row using range_write
    range_write(ss = YOUR_SHEET_ID, 
                sheet = "analyst_ratings", 
                data = updated_data, 
                range = cell_rows(sheet_row_number), # e.g., "A5:E5"
                col_names = FALSE) # Don't re-write the header
    
    # Refresh, clear, and reset buttons
    load_ratings_trigger(runif(1))
    clear_form()
    selected_row_data(NULL)
    shinyjs::hide("update_div")
    shinyjs::show("add_new_div")
  })
  
  # --- 5. D (Delete): Delete a Rating ---
  
  # D (Delete): Show a confirmation modal
  observeEvent(input$delete_rating, {
    row_index <- input$ratings_table_rows_selected
    if (is.null(row_index)) {
      showModal(modalDialog(title = "Error", "Please select a row to delete."))
      return()
    }
    
    # Store the selected row's data
    selected_data <- ratings_data()[row_index, ]
    selected_row_data(selected_data)
    
    # Show confirmation
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete this rating?",
            "\nSource:", selected_data$source,
            "\nRating:", selected_data$rating),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Yes, Delete", class = "btn-danger")
      )
    ))
  })
  
  # D (Confirm Delete): This runs when "Yes, Delete" is clicked
  observeEvent(input$confirm_delete, {
    removeModal()
    
    # Get the unique ID of the row to delete
    selected_id <- selected_row_data()$rating_id
    if (is.null(selected_id)) return()
    
    print(paste("Deleting row with ID:", selected_id))
    
    # Read all sheet data to find the *Google Sheet row number*
    all_data <- range_read(ss = YOUR_SHEET_ID, sheet = "analyst_ratings", col_types = "Dccnc")
    row_to_delete <- which(all_data$rating_id == selected_id)
    
    if (length(row_to_delete) == 0) {
      showModal(modalDialog(title = "Error", "Could not find row to delete. Please refresh."))
      return()
    }
    
    # Google Sheet rows are 1-indexed, +1 for the header
    sheet_row_number <- row_to_delete + 1
    
    # Delete that specific row
    range_delete(ss = YOUR_SHEET_ID, 
                 sheet = "analyst_ratings", 
                 range = cell_rows(sheet_row_number),
                 shift = "up")
    
    # Refresh and clear
    load_ratings_trigger(runif(1))
    clear_form()
    selected_row_data(NULL)
  })
  # --- 5. CRUD Functionality (Notes) ---
  
  # This reactiveVal is a "trigger" that tells the table to refresh
  load_notes_trigger <- reactiveVal(runif(1))
  
  # R (Read): Read the notes from the Google Sheet
  notes_data <- reactive({
    load_notes_trigger() # Depend on the trigger
    print("Reading notes from Google Sheet...")
    range_read(ss = YOUR_SHEET_ID, sheet = "app_notes")
  })
  
  # Display the notes in the table
  output$notes_table <- renderDT({
    datatable(
      notes_data(),
      options = list(order = list(list(0, 'desc'))) # Show newest notes first
    )
  })
  
  # C (Create): When the submit button is pressed...
  observeEvent(input$submit_note, {
    print("Submit button pressed. Appending to Google Sheet...")
    # Create a 1-row data frame for the new note
    new_note <- data.frame(
      timestamp = as.character(Sys.time()),
      note = input$note_input
    )
    
    # Append this row to the 'app_notes' tab
    sheet_append(ss = YOUR_SHEET_ID, data = new_note, sheet = "app_notes")
    
    # Pull the trigger to refresh the table
    load_notes_trigger(runif(1))
    
    # Clear the text box for the user
    updateTextInput(session, "note_input", value = "")
  })
} # End of server