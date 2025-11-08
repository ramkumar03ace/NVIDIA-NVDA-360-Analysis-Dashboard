setwd("D:/VIT/VIT Sem 7/PDS/Project")

# --- 0. Load all necessary libraries ---
# install.packages("pdftools") # Uncomment this if you haven't installed it
library(pdftools)
library(dplyr)
library(googlesheets4)

# --- 1. Define the PDF URL ---
# This is the direct link to NVIDIA's official 2025 Annual Report (Form 10-K)
pdf_url <- "https://s201.q4cdn.com/141608511/files/doc_financials/2025/q4/177440d5-3b32-4185-8cc8-95500a9dc783.pdf"

print("Starting PDF text extraction from URL...")
print("This may take 10-20 seconds as it reads the entire report...")

# --- 2. Extract Text Directly from URL ---
# pdf_text() reads the PDF and returns a character vector,
# where each item in the vector is the text from one page.
all_pages_text <- pdf_text(pdf_url)

print(paste("Successfully extracted text from", length(all_pages_text), "pages."))

# --- 3. Clean into a Data Frame ---
# We want to store this in our database (Google Sheets) in a tidy format.
pdf_df <- data.frame(
  page_number = 1:length(all_pages_text),
  text = all_pages_text
)

# Let's look at the text from one of the "Risk Factors" pages (e.g., page 16)
print("--- Sample: Text from Page 16 (Risk Factors) ---")
print(substr(pdf_df[16, "text"], 1, 500))
print("...")

# --- 4. Write to Google Sheet ---

# IMPORTANT:
# 1. Make sure your Google Sheet has a tab (sheet) named "pdf_text".
# 2. Paste the *same* Sheet ID you used in the previous step.

YOUR_SHEET_ID <- "1-5oHCZcxrLarT31dh6xhRMggpvTVnc8erqr6jqiNe7w" 

# After you've pasted your ID, you can uncomment and run the line below:
sheet_write(pdf_df,
            ss = YOUR_SHEET_ID,
           sheet = "pdf_text")

print("Successfully wrote all PDF text to Google Sheets tab 'pdf_text'!")
