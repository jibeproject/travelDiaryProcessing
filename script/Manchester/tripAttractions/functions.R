library(lavaan)
library(ggplot2)
library(patchwork)
library(reshape2)  # For reshaping the data

# To combine two R tables
# Define the function
combine_table_counts <- function(table1, table2) {
  # Extract labels and counts from table1
  labels_table1 <- names(table1)
  counts_table1 <- as.vector(table1)
  
  # Extract labels and counts from table2
  labels_table2 <- names(table2)
  counts_table2 <- as.vector(table2)
  
  # Create a data frame for table1 counts
  df_table1 <- data.frame(
    Label = labels_table1,
    Count_V1 = counts_table1
  )
  
  # Merge with table2 counts (matching on the Label)
  df_combined <- merge(df_table1, data.frame(Label = labels_table2, Count_V2 = counts_table2), 
                       by = "Label", all.x = TRUE)
  
  return(df_combined)
}



# base R
plot_histogram<-function(x, p){
  x <- x^p
  hist(x, probability = TRUE, breaks = 50)
  abline(v = mean(x), col='red', lwd = 3)
  lines(density(x), col = 'green', lwd = 3)
}

# ggplot
plot_histogram_ggplot <- function(x, p) {
  # Apply the transformation
  x <- x^p
  
  # Create a data frame for ggplot
  df <- data.frame(x = x)
  
  # Plot using ggplot2
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "lightblue", color = "black") +
    geom_vline(aes(xintercept = mean(x)), color = "red", linetype = "solid", linewidth = 1.5) +
    geom_density(color = "green", linewidth = 1.5) +
    theme_minimal() +
    labs(title = paste("Histogram and Density for p =", p),
         x = "Transformed Variable",
         y = "Density")
}

# ggplot - compare two variables
plot_density_ggplot <- function(x, p) {
  x1<-x
  x2<-x^p
  
  # Create a data frame in long format
  df <- data.frame(value = c(x1, x2),
                   variable = rep(c("x1", "x2"), each = length(x1)))
  
  # Plot using ggplot2
  ggplot(df, aes(x = value, color = variable)) +
    geom_density(size = 1.5) +                        # Add density plots for both x1 and x2
    theme_minimal() +
    labs(title = paste("Density Plot for p =", p),
         x = "Transformed Variable",
         y = "Density") +
    scale_color_manual(values = c("x1" = "blue", "x2" = "green"))  # Set custom colors for x1 and x2
}



