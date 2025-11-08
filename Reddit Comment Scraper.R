setwd("D:/VIT/VIT Sem 7/PDS/Project")

# --- 0. Load all necessary libraries ---
# install.packages("RedditExtractoR") # Uncomment this if you haven't installed it
library(RedditExtractoR)
library(dplyr)
library(googlesheets4)

# --- 1. Find relevant threads ---
# We will search the r/NVDA_Stock subreddit, which is perfect for this.
# We'll search for "NVDA" or "Nvidia" and get the most commented threads
# from the past month.
print("Finding recent, high-comment threads on r/NVDA_Stock...")

# You can change keywords, sort_by ("comments", "new", "top"), or period ("all", "year", "month")
thread_urls <- find_thread_urls(
  subreddit = "NVDA_Stock",
  keywords = "NVDA, Nvidia",
  sort_by = "comments",
  period = "month"
)

# Check if we found any threads
if (nrow(thread_urls) == 0) {
  stop("No threads found with those keywords. Try different keywords or a longer 'period'.")
}

# Let's limit our scrape to the top 10 most-commented threads to be fast
# You can increase this number if you want more data
top_urls <- head(thread_urls$url, 10)

print(paste("Found", length(top_urls), "threads. Now scraping all comments..."))
print("(This may take a minute or two)...")

# --- 2. Get all comments from those threads ---
# This function visits each URL and scrapes its content.
# It returns a list containing two data frames: $threads and $comments
reddit_content <- get_thread_content(top_urls)

# We only care about the comments data frame
reddit_comments_df <- reddit_content$comments

# Check if we got any comments
if (nrow(reddit_comments_df) == 0) {
  stop("Found threads, but could not scrape any comments from them.")
}

print(paste("Successfully scraped", nrow(reddit_comments_df), "comments!"))

# --- DEBUGGING STEP ---
# Let's print the names of all columns to see what's available
print("--- Available Column Names ---")
print(names(reddit_comments_df))

# --- 3. Clean the Data ---
# The data is already pretty clean, but let's select the most important columns
#
# *** FIX APPLIED HERE ***
# The error "column `comment_author` doesn't exist" means the real name is just `author`.
# We've changed `comment_author = author` to `author`
# (Or, if we wanted to rename it, we'd use: `comment_author = author`)

clean_comments_df <- reddit_comments_df %>%
  select(
    thread_url = url,
    comment_id,
    author, # This is the fix
    comment,
    timestamp,
    score
  )

print("--- Sample of scraped comments (Cleaned) ---")
print(head(clean_comments_df))

# --- 4. Write to Google Sheet ---

# IMPORTANT:
# 1. Make sure your Google Sheet has a tab (sheet) named "reddit_comments".
# 2. Paste the *same* Sheet ID you used in the previous steps.

YOUR_SHEET_ID <- "1-5oHCZcxrLarT31dh6xhRMggpvTVnc8erqr6jqiNe7w" 

# After you've pasted your ID, you can uncomment and run the line below:
sheet_write(
 clean_comments_df,
 ss = YOUR_SHEET_ID,
 sheet = "reddit_comments"
)

print("Successfully wrote all Reddit comments to Google Sheets tab 'reddit_comments'!")

