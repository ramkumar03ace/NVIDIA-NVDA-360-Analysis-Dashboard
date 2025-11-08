setwd("D:/VIT/VIT Sem 7/PDS/Project")
# --- 0. Install and Load New Libraries ---
# (You only need to run the install.packages lines once)
# install.packages("ggplot2")       # For plotting
# install.packages("tidytext")      # For sentiment analysis
# install.packages("stringr")       # For text manipulation
# install.packages("wordcloud")     # For word clouds
# install.packages("tidyquant")     # For stock prices & candlestick charts
# install.packages("reshape2")      # For 'acast' function
# install.packages("tidyr")         # *** NEW: ADDED FOR 'pivot_longer' FUNCTION ***

library(googlesheets4)
library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(wordcloud)
library(tidyquant) # For tq_get() and geom_candlestick()
library(reshape2)      # For 'acast' function
library(tidyr)         # *** NEW: ADDED FOR 'pivot_longer' FUNCTION ***

# --- 1. Read ALL Data From Your Google Sheet "Database" ---

# IMPORTANT: Paste the same Google Sheet ID here

YOUR_SHEET_ID <- "1-5oHCZcxrLarT31dh6xhRMggpvTVnc8erqr6jqiNe7w" 

print("Reading data from Google Sheets...")

# Read the Reddit comments
reddit_df <- range_read(ss = YOUR_SHEET_ID, sheet = "reddit_comments")

# Read the PDF text
pdf_df <- range_read(ss = YOUR_SHEET_ID, sheet = "pdf_text")

print("All data successfully read from Google Sheets.")

# --- 2. Analysis 1: Reddit Sentiment ---

print("Starting Reddit sentiment analysis...")

# Get the "bing" sentiment lexicon (positive/negative words)
bing_sentiments <- get_sentiments("bing")

# Tokenize: Break comments into individual words
reddit_words <- reddit_df %>%
  unnest_tokens(output = word, input = comment)

# Remove "stop words" (common words like "the", "is", "a")
reddit_words <- reddit_words %>%
  anti_join(stop_words)

# Perform the sentiment analysis
reddit_sentiment <- reddit_words %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(sentiment) # Count total positive vs. negative words

print("--- Reddit Sentiment Results ---")
print(reddit_sentiment)

# --- VISUAL 1: Sentiment Bar Chart ---
plot1 <- ggplot(reddit_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() + # Use geom_col() for a bar chart
  geom_text(aes(label = n), vjust = -0.5) + # Add count labels
  labs(
    title = "NVIDIA Sentiment on r/NVDA_Stock",
    subtitle = "Based on 801 comments from the past month",
    x = "Sentiment",
    y = "Total Word Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend

# Save the plot to your project folder
ggsave("1_sentiment_bar_chart.png", plot1)
print("Saved 1_sentiment_bar_chart.png")

# --- VISUAL 2: Sentiment Word Cloud ---
# We need to count words, join sentiment, and spread them
sentiment_word_counts <- reddit_words %>%
  inner_join(bing_sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Create a PNG file for the word cloud
png("2_sentiment_word_cloud.png", width = 800, height = 800)

sentiment_word_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(
    colors = c("red", "darkgreen"), # negative, positive
    max.words = 100,
    title.size = 2
  )

dev.off() # Finish saving the PNG
print("Saved 2_sentiment_word_cloud.png")


# --- 3. Analysis 2: PDF Keyword Frequency ---

print("Starting PDF keyword analysis...")

# Define the keywords we care about (case-insensitive)
# These are central to NVIDIA's business
keywords_to_find <- c("AI", "Data_Center", "Gaming", "Automotive", "Risk")

# Create a function to count keyword occurrences on each page
count_keywords <- function(text) {
  # str_count is case-insensitive by default with regex(..., ignore_case=TRUE)
  data.frame(
    AI = str_count(text, regex("AI|Artificial Intelligence", ignore_case = TRUE)),
    Data_Center = str_count(text, regex("Data Center", ignore_case = TRUE)),
    Gaming = str_count(text, regex("Gaming|GeForce", ignore_case = TRUE)),
    Automotive = str_count(text, regex("Automotive", ignore_case = TRUE)),
    Risk = str_count(text, regex("Risk", ignore_case = TRUE))
  )
}

# Run the function on all pages and sum the totals
total_keyword_counts <- pdf_df %>%
  mutate(counts = purrr::map(text, count_keywords)) %>%
  unnest(counts) %>%
  summarise(across(all_of(keywords_to_find), sum)) %>%
  pivot_longer(cols = everything(), names_to = "Keyword", values_to = "Count")

print("--- PDF Keyword Results ---")
print(total_keyword_counts)

# --- VISUAL 3: Keyword Frequency Bar Chart ---
plot3 <- ggplot(total_keyword_counts, aes(x = reorder(Keyword, -Count), y = Count, fill = Keyword)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(
    title = "Keyword Frequency in NVIDIA 2025 Annual Report",
    x = "Keyword",
    y = "Total Mentions"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("3_keyword_frequency_chart.png", plot3)
print("Saved 3_keyword_frequency_chart.png")


# --- 4. Analysis 3: Historical Stock Price ---

print("Downloading historical stock data for NVDA...")

# Use tidyquant to get stock prices
# This is our new, fourth data source
nvda_prices <- tq_get("NVDA", from = "2023-01-01", to = Sys.Date())

print("--- Sample of Stock Price Data ---")
print(head(nvda_prices))

# --- VISUAL 4: Stock Price Line Chart ---
plot4 <- ggplot(nvda_prices, aes(x = date, y = adjusted)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "NVIDIA (NVDA) Stock Price (Adjusted)",
    subtitle = "2023 - Present",
    x = "Date",
    y = "Adjusted Close Price ($)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar) # Format y-axis as dollars

ggsave("4_stock_price_line_chart.png", plot4)
print("Saved 4_stock_price_line_chart.png")

# --- VISUAL 5: Candlestick Chart (Recent) ---
# Filter for the last 6 months of data
recent_prices <- nvda_prices %>%
  filter(date >= (Sys.Date() - months(6)))

plot5 <- ggplot(recent_prices, aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(
    title = "NVIDIA (NVDA) 6-Month Candlestick Chart",
    x = "Date",
    y = "Stock Price ($)"
  ) +
  theme_tq() + # A nice theme from the tidyquant package
  scale_y_continuous(labels = scales::dollar)

ggsave("5_stock_candlestick_chart.png", plot5, width = 10, height = 6)
print("Saved 5_stock_candlestick_chart.png")

print("--- SEMINAR 2 COMPLETE ---")
print("All analysis is done and 5 plots have been saved to your project folder.")

