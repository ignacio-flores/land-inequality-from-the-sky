library(stringdist)
library(tictoc)

match_strings <- function(list1, list2, nr = 3) {
  
  tic("match_strings ")
  
    print(paste("Original (list1) length: ", length(list1)))
    print(paste("Similars (list2) length: ", length(list2)))
    
    # Initialize an empty data frame to store the matches
    matches <- data.frame(
      string1 = character(), 
      string2 = character(), 
      distance = numeric(), 
      stringsAsFactors = FALSE
    )
    
    # Loop over each string in the first list
    for (string1 in list1) {
      # Initialize an empty data frame to store the distances for this string
      distances <- data.frame(string2 = character(), distance = numeric(), stringsAsFactors = FALSE)
      
      # Loop over each string in the second list
      for (string2 in list2) {
        # Compute the Levenshtein distance between the two strings
        distance <- stringdist(string1, string2)
        
        # Add the distance to the data frame
        distances <- rbind(distances, data.frame(string2 = string2, distance = distance))
      }
      
      # Sort the distances in ascending order
      distances <- distances[order(distances$distance),]
      
      # Keep only the top three distances
      distances <- head(distances, nr)
      
      # Check if any of the top three distances are a perfect match
      perfect_match <- distances$distance == 0
      
      # If there is a perfect match, add it to the matches data frame and remove it from list2
      if (any(perfect_match)) {
        perfect_match_index <- which(perfect_match)[1]
        matches <- rbind(matches, data.frame(string1 = string1, string2 = distances[perfect_match_index, "string2"], distance = 0))
        list2 <- list2[-which(list2 == distances[perfect_match_index, "string2"])]
      }
      # If there is no perfect match, add the top three matches to the matches data frame
      else {
        matches <- rbind(matches, data.frame(string1 = string1, string2 = distances$string2, distance = distances$distance))
      }
    }
    
    #rename 
    matches <- rename(matches, original = `string1`, similar = `string2`)
    
    #count perfect matches 
    pfct <- filter(matches, distance == 0)
    lpfct <- unique(pfct$original)
    print(paste0("Perfect matches ", length(lpfct)))
    print(lpfct)
    
  toc()
  
  return(matches)
}
