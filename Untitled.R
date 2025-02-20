


pca_data <- site_data[, c("NH4", "NO3", "Ortho_P_mg_kg","weight", "fire.frequency", "FESM_Fire.Severity.Category")]  # Replace with your actual column names
pca_data_scaled <- scale(pca_data)
pca_result <- rda(pca_data_scaled)
summary(pca_result)

pca_result$sdev^2 / sum(pca_result$sdev^2)  # Explained variance ratio

# Plot the PCA results (scree plot)
screeplot(pca_result, main = "Scree Plot")

# Biplot (projection of the data on the first two components)
biplot(pca_result, main = "PCA Biplot")


# Standardize the data (optional but recommended)
data_scaled <- scale(site_data)  # This scales the data to have a mean of 0 and standard deviation of 1

# Perform PCA using rda() from vegan
pca_result <- rda(data_scaled)

# View the PCA result
summary(pca_result)

# Accessing the PCA components (eigenvectors)
pca_result$rotation  # Principal components (eigenvectors)

# Accessing the eigenvalues (importance of components)
pca_result$CA$eig  # Eigenvalues (variance explained by each principal component)

# Accessing the scores (coordinates of the data points in the new PCA space)
pca_result$sites  # PCA scores for each sample

