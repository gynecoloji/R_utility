library(pheatmap)
library(stringr)

# Function to calculate gap positions for heatmap
calculate_gaps <- function(categories) {
  # Get frequency table
  freq_table <- table(categories)
  
  # Calculate positions
  positions <- numeric(length(freq_table))
  running_sum <- 0
  
  for(i in 1:length(freq_table)) {
    running_sum <- running_sum + freq_table[i]
    positions[i] <- running_sum
  }
  
  return(positions)
}


