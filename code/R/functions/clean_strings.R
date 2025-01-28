clean_strings <- function(strings) {
  cleaned_strings <- lapply(strings, function(string) {
    # Remove punctuation
    no_punc <- gsub("[[:punct:]]", "", string)
    
    # Convert to lowercase and remove accents/other characters
    no_accents <- iconv(no_punc, to = "ASCII//TRANSLIT")
    lower <- tolower(no_accents)
    
    # Remove leading/trailing whitespace
    trimmed <- trimws(lower)
    
    return(trimmed)
  })
  
  return(cleaned_strings)
}
