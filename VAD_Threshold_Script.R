# Load necessary libraries
library(tm)          # Text mining
library(stringr)     # String manipulation
library(tokenizers)  # Tokenization
library(lexicon)     # Lexicon for sentiment analysis
library(wordcloud)   # Word cloud generation

# Load NRC VAD lexicon
VAD <- NRC_VAD

# Filter words with high and low valence
Valence_Pos_75_Data <- subset(VAD[1:2], VAD$Valence >= 0.75, select = c(Word))
Valence_Pos_75_Data <- unlist(Valence_Pos_75_Data)

Valence_Neg_25_Data <- subset(VAD[1:2], VAD$Valence <= 0.25, select = c(Word))
Valence_Neg_25_Data <- unlist(Valence_Neg_25_Data)

# Initialize an empty list to store results
df1 <- list()

# Define the folder containing subject files
folder_path <- "study_study_folder"

# Get a list of all files in the folder
file_list <- list.files(path = folder_path, full.names = TRUE)

# Loop through each file in the folder
for (file_path in file_list) {
  # Read and preprocess the text
  file_content <- readLines(file_path)
  text_content <- paste(file_content, collapse = " ") # Combine lines into a single string

  # Remove URLs
  text_content <- gsub("http[s]?://\\S+|www\\.\\S+", " ", text_content)

  # Remove emojis (Unicode emoji range and other special symbols)
  text_content <- gsub("[\\p{So}\\p{Cn}]", " ", text_content, perl = TRUE)

  # Remove punctuation, symbols, and other non-alphanumeric characters
  text_content <- gsub("\\W", " ", text_content)

  # Remove digits
  text_content <- gsub("\\d", " ", text_content)

  # Convert to lowercase
  text_content <- tolower(text_content)

  # Remove stopwords
  text_content <- removeWords(text_content, stopwords())

  # Remove single letters
  text_content <- gsub("\\b[A-z]\\b{1}", " ", text_content)

  # Remove extra whitespace
  text_content <- stripWhitespace(text_content)

  # Tokenize the text
  tokens <- unlist(str_split(text_content, "\\s+"))

  # Calculate word counts
  total_words <- length(tokens)
  positive_words <- sum(!is.na(match(tokens, Valence_Pos_75_Data)))
  negative_words <- sum(!is.na(match(tokens, Valence_Neg_25_Data)))

  # Compute sentiment scores
  pos_div_words <- ifelse(total_words > 0, positive_words / total_words, 0)
  neg_div_words <- ifelse(total_words > 0, negative_words / total_words, 0)

  # Store results in a data frame
  file_results <- data.frame(
    File = basename(file_path),
    Positive_Sentiment = pos_div_words,
    Negative_Sentiment = neg_div_words
  )

  # Append results to the main list
  df1 <- rbind(df1, file_results)
}

# Combine all results into a single data frame
df1 <- do.call(rbind, df1)

# Save the results to a CSV file
output_file <- "sentiment_analysis_results.csv"
write.csv(df1, output_file, row.names = FALSE)

# Print completion message
cat("Sentiment analysis completed. Results saved to", output_file, "\n")
