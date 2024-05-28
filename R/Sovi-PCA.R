library(readr)
urlfile = "https://raw.githubusercontent.com/dedenistiawan/Dataset/main/sovi_data_kab_514.csv"
Data<-read.csv(url(urlfile), row.names = "KABUPATEN")

#library(scales)
library(dplyr)
library(ggplot2)

# List of indicators to be normalized
indicators <- c('CHILDREN', 'FEMALE', 'ELDERLY', 'FHEAD', 'FAMILYSIZE', 'NOELECTRIC', 
                'LOWEDU', 'GROWTH', 'POVERTY', 'ILLITERATE', 'NOTRAINING', 'DPRONE', 
                'RENTED', 'NOSEWER', 'TAPWATER', 'NO WORK', 'CACAT', 'P.KRONIS', 'JAMKES')


# Normalize the data Normalize the indicators 0-1
sovi_data_normalized <- data %>%
  mutate(across(all_of(indicators), ~ rescale(.)))

# Normalize the data Normalize the indicators z-score
sovi_data_normalized <- data %>% mutate_at(c(indicators), ~(scale(.) %>% as.vector))

#normaliasai z-score
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data_z <- data %>%
  mutate(across(where(is.numeric), standardize))

#SOVI Z-Score
data_z_sovi <- data_z %>%
  rowwise() %>%
  mutate(SVI = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

# Categorize the SVI
data_z_soVi_cat <- data_z_sovi %>%
  mutate(SVI_category = case_when(
    SVI >= 2 ~ "Very High",
    SVI >= 1 ~ "High",
    SVI >= 0 ~ "Moderate",
    SVI >= -1 ~ "Low",
    TRUE ~ "Very Low"
  ))

# Histogram of SoVI
ggplot(data_z_soVi_cat, aes(x = SVI)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Social Vulnerability Index (SoVI)",
       x = "SoVI",
       y = "Frequency")

# Bar plot of top 10 most vulnerable regions
data_z_soVi_cat %>%
  arrange(desc(SVI)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(KABUPATEN, SVI), y = SVI)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 10 Most Vulnerable Regions",
       x = "Region",
       y = "SoVI")



#SOVI II
# Define equal weights for each indicator
weights <- rep(1 / length(indicators), length(indicators))

# Calculate SoVI
sovi_data_normalized <- sovi_data_normalized %>%
  rowwise() %>%
  mutate(SoVI = sum(c_across(all_of(indicators)) * weights)) %>%
  ungroup()

# Display the final data with SoVI
sovi_data_normalized %>%
  select(KABUPATEN, SoVI) %>%
  arrange(desc(SoVI))

# Histogram of SoVI
ggplot(sovi_data_normalized, aes(x = SoVI)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Social Vulnerability Index (SoVI)",
       x = "SoVI",
       y = "Frequency")

# Bar plot of top 10 most vulnerable regions
sovi_data_normalized %>%
  arrange(desc(SoVI)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(KABUPATEN, SoVI), y = SoVI)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 10 Most Vulnerable Regions",
       x = "Region",
       y = "SoVI")

#==================================================================#
#SOVI-PCA#
#normaliasai z-score
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

data_z <- data %>%
  mutate(across(where(is.numeric), standardize))

# Perform PCA
# Exclude the first column (KABUPATEN)
pca_result <- prcomp(data_z[,-1], scale. = TRUE)  

# Summary of PCA results
summary(pca_result)

# Extract the scores (principal components)
scores <- as.data.frame(pca_result$x)

# Calculate the Social Vulnerability Index (SoVI) using the first principal component
sovi_pca <- scores[,1]

# Add the SoVI to the original data
data_z <- data_z %>%
  mutate(SoVI_PCA = sovi_pca)

# Rank the kabupaten based on the SoVI
sovi_ranking <- data_z %>%
  select(KABUPATEN, SoVI_PCA) %>%
  arrange(desc(SoVI_PCA))

# Display the SoVI ranking
print(sovi_ranking)

# Save the result to a CSV file
write.csv(sovi_ranking, "sovi_ranking_pca.csv", row.names = FALSE)

#========================================================#
#SOVI-PCA Cutter#

# Normalize the data using Z-score normalization
normalize <- function(x) {
  return ((x - mean(x)) / sd(x))
}

# Apply normalization to the relevant columns
normalized_data <- sovi_data %>%
  mutate(across(where(is.numeric), normalize))

# Perform PCA
pca_result <- prcomp(normalized_data[,-1], scale. = TRUE)  # Exclude the first column (KABUPATEN)

# Summary of PCA results
summary(pca_result)

# Determine the number of components that explain a significant amount of variance
explained_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
n_components <- which(explained_variance >= 0.8)[1]  # Select components that explain at least 80% of the variance

# Extract the scores (principal components)
scores <- as.data.frame(pca_result$x)

# Calculate the Social Vulnerability Index (SoVI) using the selected principal components
sovi_pca <- rowSums(scores[, 1:n_components])

# Add the SoVI to the original data
sovi_data <- sovi_data %>%
  mutate(SoVI_Cutter = sovi_pca)

# Rank the kabupaten based on the SoVI
sovi_ranking <- sovi_data %>%
  select(KABUPATEN, SoVI_Cutter) %>%
  arrange(desc(SoVI_Cutter))

# Display the SoVI ranking
print(sovi_ranking)

# Save the result to a CSV file
write.csv(sovi_ranking, "sovi_ranking_cutter.csv", row.names = FALSE)

# Visualize the results
library(ggplot2)
ggplot(sovi_ranking, aes(x = reorder(KABUPATEN, SoVI_Cutter), y = SoVI_Cutter)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Social Vulnerability Index (SoVI) by Kabupaten",
       x = "Kabupaten",
       y = "SoVI Score") +
  theme_minimal()

#==============================================================
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(scales)

# Load the data
file_path <- "path/to/your/sovi_data_kab_514.xlsx"
data <- read_excel(file_path)

# View the first few rows of the data
head(data)

# Data Cleaning: Remove non-numeric columns and handle missing values
sovi_data <- data %>% 
  select(-KABUPATEN) %>% 
  na.omit()

# Data Normalization: Scale the data
sovi_data_scaled <- scale(sovi_data)

# Perform Principal Component Analysis (PCA)
pca_result <- PCA(sovi_data_scaled, graph = FALSE)

# Extract PCA results
pca_var <- get_pca_var(pca_result)

# Scree plot to determine the number of components
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Contribution of variables to PC1 and PC2
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)

# Construct SVI using the first few principal components
num_components <- 3  # Select number of components based on scree plot
svi_scores <- as.data.frame(pca_result$ind$coord[, 1:num_components])

# Calculate SVI as a weighted sum of selected principal components
svi_scores$SVI <- rowSums(svi_scores)

# Add SVI scores to the original data
data$SVI <- svi_scores$SVI

# View the final data with SVI scores
head(data)

# Save the final data to a new Excel file
write.xlsx(data, "path/to/your/final_sovi_data.xlsx")

