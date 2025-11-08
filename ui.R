#setwd("D:/VIT/VIT Sem 7/PDS/Project")
# --- Seminar 3: ui.R (The "Front-End") ---
# This file controls the layout and appearance of your app.

# 1. Load necessary libraries
library(shiny)
library(plotly) # For interactive plots
library(DT)     # For interactive tables

# 2. Define the User Interface (UI)
ui <- fluidPage(
  
  # --- PASTE THE NEW CODE BLOCK HERE ---
  tags$head(
    tags$style(HTML("
      /* Style the main tab bar */
      .nav-tabs {
        background-color: #2c3e50; /* A professional dark blue background */
        border-bottom: 2px solid #34495e;
      }
      
      /* Style the inactive tab links */
      .nav-tabs > li > a {
        color: #ecf0f1; /* Light text color */
        border: 1px solid transparent;
      }
      
      /* Style the inactive tab links on hover */
      .nav-tabs > li > a:hover {
        background-color: #34495e; /* Slightly lighter on hover */
        border-color: #34495e;
        color: #fff;
      }
      
      /* Style the active tab link */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #000; /* Black text for active tab */
        background-color: #fff; /* White background for active tab */
        border: 1px solid #ddd;
        border-bottom-color: transparent;
      }
    "))
  ),
  
  shinytoastr::useToastr(),
  # -------------------------------------------
  # App title
  titlePanel("NVIDIA (NVDA) 360° Analysis Dashboard"),
  
  # Layout with multiple tabs
  tabsetPanel(
    
    # --- Tab 1: Stock Performance ---
    tabPanel(
      "Stock Performance",
      br(), # Add a line break for spacing
      h3("Historical Stock Price"),
      p("This chart shows the adjusted closing price for NVDA stock from 2023 to the present."),
      
      # --- ADD THIS BLOCK ---
      radioButtons("stock_time_range", 
                   "Select Time Range:",
                   choices = c("1 Year", "2 Years", "3 Years", "5 Years", "All Time"),
                   selected = "All Time",
                   inline = TRUE), # 'inline = TRUE' makes the buttons horizontal
      # ---------------------
      
      plotlyOutput("stock_price_plot"), # Interactive plot
      hr(),
      h3("Recent 6-Month Candlestick Chart"),
      p("This chart shows the daily open, high, low, and close prices for the last 6 months."),
      plotlyOutput("candlestick_plot") # Interactive plot
    ),
    
    # --- Tab 2: Sentiment Analysis ---
    tabPanel(
      "Sentiment Analysis (Reddit)",
      br(),
      h3("Reddit Sentiment Word Cloud"),
      p("This cloud shows the most frequent positive (green) and negative (red) words from r/NVDA_Stock comments."),
      plotOutput("wordcloud_plot", height = "600px"), # Standard plot for wordcloud
      hr(),
      h3("Overall Sentiment Score"),
      p("This chart shows the total count of positive vs. negative words found in the comments."),
      plotlyOutput("sentiment_bar_plot") # Interactive plot
    ),
    
    # --- Tab 3: Annual Report Insights ---
    tabPanel(
      "Annual Report Insights",
      br(),
      h3("Keyword Frequency in 2025 Annual Report (10-K)"),
      p("This chart shows the total number of times key business segments and 'Risk' were mentioned in the official report."),
      plotlyOutput("keyword_plot") # Interactive plot
    ),
    
    # --- Tab 4: Live Project Notes (CRUD) ---
    tabPanel(
      "Live Project Notes",
      br(),
      h3("Add a New Note"),
      p("This data is written live to the 'app_notes' tab of your Google Sheet."),
      
      # Input for new note
      textInput("note_input", "Enter your note:", ""),
      
      # Submit button
      actionButton("submit_note", "Submit Note"),
      
      hr(),
      h3("All Project Notes"),
      
      # Output table
      DTOutput("notes_table")
    ),
    
    # --- Tab 5: Key Financials (Finviz) ---
    tabPanel(
      "Key Financials (Finviz)",
      br(),
      h3("Key Financial & Trading Metrics"),
      p("This table shows fundamental data (like P/E Ratio) and technical data (like RSI) scraped from Finviz. 
        It provides a snapshot of the company's valuation and recent trading momentum."),
      
      # We will display the data in a clean, searchable table
      DTOutput("finviz_table"), 
      
      hr(),
      
      # --- This is the new part for "normal people" ---
      h3("What Does This Data Mean?"),
      p("These metrics help investors decide if a stock is a good buy. Here are a few of the most important ones:"),
      
      tags$ul(
        tags$li(tags$strong("Market Cap:"), " This is the total value of all of the company's stock. It's a quick way to see how 'big' a company is (e.g., NVIDIA is a 'Mega-Cap' company)."),
        tags$li(tags$strong("P/E (Price-to-Earnings) Ratio:"), " A simple ratio that compares the stock price to the company's earnings. A high P/E (like NVIDIA's) can mean investors expect *high future growth* or that the stock might be *expensive*."),
        tags$li(tags$strong("EPS (Earnings Per Share):"), " The company's total profit divided by all its shares. A higher EPS is generally better, as it means the company is more profitable per share."),
        tags$li(tags$strong("Dividend Yield:"), " How much the company pays you in dividends each year as a percentage of the stock price. A 0.5% yield means you get $0.50 per year for every $100 of stock you own. NVIDIA is known for growth, not high dividends."),
        tags$li(tags$strong("RSI (Relative Strength Index):"), " This is a momentum score from 0-100. A score over 70 is often considered 'overbought' (and may be due for a pullback), while a score under 30 is 'oversold' (and may be due for a bounce).")
      )
    ),
    # --- Tab 6: Analyst Ratings Tracker (CRUD) ---
    tabPanel(
      "Analyst Ratings Tracker",
      
      # 1. ADD THIS: This line initializes shinyjs
      shinyjs::useShinyjs(), 
      
      br(),
      h3("Log a New Analyst Rating"),
      p("Add new 'Buy'/'Sell'/'Hold' ratings as they are announced. 
        Select a row in the table below and click 'Load for Edit' or 'Delete'."),
      
      # We need a more complex input form now
      fluidRow(
        column(4,
               dateInput("rating_date", "Date:", value = Sys.Date())
        ),
        column(4,
               textInput("rating_source", "Source (e.g., J.P. Morgan):", "")
        )
      ),
      fluidRow(
        column(4,
               selectInput("rating_action", "Rating:", 
                           choices = c("Buy", "Hold", "Sell", "Upgrade", "Downgrade"))
        ),
        column(4,
               numericInput("rating_target", "Price Target ($):", value = 150, min = 0)
        )
      ),
      
      # --- 2. MODIFY THIS: Wrap buttons in divs ---
      # The "Submit" button
      shinyjs::hidden(
        div(id = "submit_div",
            actionButton("submit_rating", "Submit New Rating")
        )
      ),
      
      # The "Update" button (will be hidden at first)
      shinyjs::hidden(
        div(id = "update_div",
            actionButton("update_rating", "Update Selected Rating", class = "btn-primary"),
            actionButton("cancel_edit", "Cancel", class = "btn-warning")
        )
      ),
      
      # We will show a simple "Add" button to start
      div(id = "add_new_div",
          actionButton("add_new_btn", "Add New Rating")
      ),
      
      
      hr(),
      h3("All Logged Analyst Ratings"),
      
      # --- 3. ADD THIS: New buttons for U and D ---
      div(
        actionButton("load_for_edit", "Load Selected Rating for Edit"),
        actionButton("delete_rating", "Delete Selected Rating", class = "btn-danger"),
        style = "margin-bottom: 15px;" # Add some space
      ),
      
      # --- 4. MODIFY THIS: Use DT::dataTableOutput ---
      # --- 4. MODIFY THIS: Use DT::dataTableOutput ---
      DT::dataTableOutput("ratings_table")
    ) # <-- This is the end of the last tabPanel
    
  ), # <-- This is the end of the tabsetPanel
  
  # --- PASTE THE NEW FOOTER CODE HERE ---
  
  hr(), # Add a horizontal line
  tags$div(
    style = "text-align: center; padding: 20px; font-size: 0.9em; color: #888;",
    "PDS Project Team:",
    tags$br(),
    tags$strong("22BCE2819"), " - MOHAMMED OMAR SHAIKH",
    tags$br(),
    tags$strong("22BCE2898"), " - ROHITH THIYAGRAJAN",
    tags$br(),
    tags$strong("22BCE3741"), " - RAMKUMAR ARCOT DHARMALINGAM"
  )
  
  # -------------------------------------
  
) # <-- This is the final closing parenthesis for fluidPage