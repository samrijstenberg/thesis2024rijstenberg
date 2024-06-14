# Load packages
library(vivainsights)
library(ggplot2)

# Clean environment
rm(list=ls())

# Load data for naming
df_names <- read.csv("your directory")
str(df_names)

df_names <- df_names[df_names$ActualHT <100 ,]


df_names |>
  ggplot(aes(x = ActualHT)) +
  geom_histogram(bins = 100) +
  ylab("Frequency") +
  xlab("Man hours / ton") +
  theme_bw(base_size = 22)

# Remove instances outside range 15-75
df_names <- df_names[df_names$ActualHT >15 ,]
df_names <- df_names[df_names$ActualHT <75 ,]
df_names = df_names[(which(nchar(df_names$Section_Type) > 2)),]

# Fix name of a building location
df_names$Building_Location[df_names$Building_Location == "x & y"] <- "x"


df_names |>
  ggplot(aes(x = ActualHT)) +
  geom_histogram(bins = 60) +
  ylab("Frequency") +
  xlab("Man hours / ton") +
  theme_bw(base_size = 22)


# Creating keyword lists for each category
AList <- c("a", "aa")
BList <- c("b", "bb")
# ...  25 more times

# Make copy 1/0 instead of Y/N
library(plyr)
df_names$Copy <- revalue(df_names$Copy, c("Y"=1))
df_names$Copy <- revalue(df_names$Copy, c("N"=0))
df_names$Copy <- as.integer(df_names$Copy)

# Function to check if any keyword is present in a description
contains_keywords <- function(description, keywords) {
  # Check if any of the keywords are in the description
  any(sapply(keywords, function(kw) grepl(kw, description, ignore.case = TRUE)))
}



# Function to extract common words excluding specific keywords
extract_common_words <- function(reviews, num_words=100) {
  # Flatten the list of reviews into one string
  all_reviews <- paste(reviews, collapse=" ")
  
  # Count the frequency of each word
  words <- unlist(strsplit(all_reviews, "\\s+"))
  word_table <- sort(table(words), decreasing = TRUE)
  
  # Return the top N words
  return(head(word_table, num_words))
}


#Analyze strings
results <- df_names %>%
  group_by(`Type`) %>%
  summarise(CommonWords = list(extract_common_words(Section_Type)))

# View the results
print(results)

for (i in 1:nrow(results)) {
  Type <- results$`Type`[i]
  common_words_table <- results$CommonWords[[i]]
  
  # Check if the common words table is not NULL
  if (!is.null(common_words_table)) {
    cat("\nType:", Type, "\n")
    print(common_words_table)
  } else {
    cat("\nType:", Type, "has no common words.\n")
  }
}


# Creating columns for each category equal to 1 or 0
df_names$isA <- as.integer(sapply(df_names$Section_Type, contains_keywords, AList))
sum(df_names$isA)

df_names$isB <- as.integer(sapply(df_names$Section_Type, contains_keywords, Blist))
sum(df_names$isB)

# 25 more times





# Remove unnecessary columns
df_names_complete <- df_names[-c(1:3)]
df_names_complete <- df_names_complete[-c(2)]
df_names_complete <- df_names_complete[-c(8)]
df_names_complete <- df_names_complete[-c(9:12)]

# Fill empty cells with NA
df_names_complete[df_names_complete == ''] <- NA


write.csv(df_names_complete, "your directory", row.names=FALSE)



# Make anonymous dataframe
df_names_encrypted <- df_names_complete

# Encrypt Type
reptype <- c("A", "B", "C", "D", "E", "F", "G")
df_names_encrypted$Type <- anonymize(df_names_encrypted$Type, scramble = FALSE, replacement = reptype)

# Encrypt Building Location
reploc <- c("A", "B", "C", "D", "E", "F")
df_names_encrypted$Building_Location <- anonymize(df_names_encrypted$Building_Location, scramble = FALSE, replacement = reploc)

# Encrypt section types
colnames(df_names_encrypted) <- c("ActualHT", "Type", "Copy", "Installed_Power", "Length", "Width", "Depth", "Building_Location", "isSectionA", "isSectionB", "isSectionC", "isSectionD", "isSectionE", "isSectionF", "isSectionG", "isSectionH", "isSectionI", "isSectionJ", "isSectionK", "isSectionL", "isSectionM", "isSectionN", "isSectionO", "isSectionP", "isSectionQ", "isSectionR", "isSectionS", "isSectionT", "isSectionU", "isSectionV", "isSectionW", "isSectionX", "isSectionY", "isSectionZ", "isSectionAA")

write.csv(df_names_encrypted, "your directory", row.names=FALSE)

