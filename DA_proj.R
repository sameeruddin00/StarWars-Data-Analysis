install.packages("dplyr")
install.packages("reshape2")


library(dplyr)
library(ggplot2)
library(reshape2)

data("starwars")


# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to calculate summary statistics
summary_stats <- function(x) {
  c(
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    average = mean(x, na.rm = TRUE),
    mode = as.character(get_mode(x))
  )
}

# Calculate summary statistics for each attribute
stats <- sapply(starwars, function(col) {
  if(is.numeric(col)) {
    summary_stats(col)
  } else {
    c(min = "", max = "", average = "", mode = as.character(get_mode(col)))
  }
})

# Print the results
print(stats)











#2question

# Filter out numeric attributes
numeric_attributes <- select_if(starwars, is.numeric)

# Melt the data for plotting
melted_data <- melt(numeric_attributes)

# Create boxplot
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Attribute", y = "Value") +
  ggtitle("Boxplot of Numeric Attributes in starwars Dataset") +
  theme_minimal()







#2Question revised



# Create boxplot for each numeric attribute
for (col in names(starwars)) {
  if (is.numeric(starwars[[col]])) {
    ggplot(starwars, aes(x = NULL, y = .data[[col]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", col),
           x = "", y = col) +
      theme_minimal()
  }
}




#3Question

# Load the required packages
library(dplyr)
library(ggplot2)

# Function to check for missing or infinite values
check_data_quality <- function(data) {
  missing_values <- sum(is.na(data))
  infinite_values <- sum(!is.finite(as.matrix(data)))
  return(list(missing_values = missing_values, infinite_values = infinite_values))
}

# Load the starwars dataset
data(starwars)

# Convert non-numeric columns to NA
starwars_numeric <- starwars %>% 
  mutate_if(is.character, as.numeric)

# Remove non-numeric columns
starwars_numeric <- select_if(starwars_numeric, is.numeric)

# Check for missing or infinite values
data_quality <- check_data_quality(starwars_numeric)
missing_values <- data_quality$missing_values
infinite_values <- data_quality$infinite_values

# If there are missing or infinite values, clean the data
if (missing_values > 0 || infinite_values > 0) {
  # Handle missing values by imputing with mean or median, or removing them
  # For simplicity, we'll remove rows with missing values
  starwars_clean <- na.omit(starwars_numeric)
  
  # Check again for missing or infinite values
  data_quality_clean <- check_data_quality(starwars_clean)
  missing_values_clean <- data_quality_clean$missing_values
  infinite_values_clean <- data_quality_clean$infinite_values
  
  # If there are still missing or infinite values, print a message
  if (missing_values_clean > 0 || infinite_values_clean > 0) {
    cat("There are still missing or infinite values in the cleaned dataset. Please handle them before proceeding.")
  } else {
    cat("Data cleaned successfully. Proceeding with K-means clustering analysis.")
    
    # Perform K-means clustering for K = 2, 3, 4, and 5
    k_values <- 2:5
    cluster_results <- lapply(k_values, function(k) {
      kmeans_result <- kmeans(starwars_clean, centers = k, nstart = 10)
      return(list(data = starwars_clean, clusters = kmeans_result$cluster))
    })
    
    # Calculate within-cluster sum of squares (WCSS) for each K
    wcss <- sapply(cluster_results, function(result) {
      sum(result$clusters$cluster_centers[result$clusters$cluster, ] - result$clusters$data)^2
    })
    
    # Plot the elbow method to find the optimal number of clusters
    elbow_df <- data.frame(K = k_values, WCSS = wcss)
    ggplot(elbow_df, aes(x = K, y = WCSS)) +
      geom_line() +
      geom_point(color = "red") +
      labs(title = "Elbow Method for Optimal Number of Clusters",
           x = "Number of Clusters (K)", y = "Within-Cluster Sum of Squares (WCSS)") +
      theme_minimal()
    
    # Find the optimal number of clusters using the elbow method
    optimal_k <- elbow_df$K[which.min(wcss)]
    
    # Print the optimal number of clusters
    cat("Optimal number of clusters:", optimal_k, "\n")
    
    # Present the result of cluster analysis for the optimal number of clusters
    cluster_result_optimal <- cluster_results[[which(k_values == optimal_k)]]
    print(cluster_result_optimal)
  }
} else {
  cat("No missing or infinite values found in the dataset. Proceeding with K-means clustering analysis.")
}



# Define the value of k for clustering
k <- 3  # You can change this to the desired number of clusters

# Check the number of distinct data points and the number of clusters
n_distinct <- nrow(unique(starwars_clean))
n_clusters <- k

cat("Number of distinct data points:", n_distinct, "\n")
cat("Number of clusters:", n_clusters, "\n")


read.csv(starwars)
getwd()

data("starwars")

 
saveRDS(starwars, file = "starwars.txt")

