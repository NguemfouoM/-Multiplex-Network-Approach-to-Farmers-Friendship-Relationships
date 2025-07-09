# Load required packages
library(igraph)      # For network creation and analysis
library(ggplot2)     # For plotting adjacency matrices and degree distributions
library(tidyr)       # For reshaping data
library(dplyr)       # For data manipulation
library(corrplot)    # For correlation heatmap

# Set seed for reproducibility
set.seed(123)

# Simulate example networks using sample_gnm
layers <- list(
  FADIR = sample_gnm(45, 72),    # 72 edges
  FAINT = sample_gnm(45, 35),    # 35 edges
  FAMU = sample_gnm(45, 26),     # 26 edges
  NOFADIR = sample_gnm(45, 33),  # 33 edges
  NOFAINT = sample_gnm(45, 27),  # 27 edges
  NOFAMU = sample_gnm(45, 23)    # 23 edges
)

# --- Figure S1: Adjacency Matrices ---
# Function to convert adjacency matrix to long format for ggplot2
adj_to_df <- function(graph, layer_name) {
  adj <- as_adjacency_matrix(graph, sparse = FALSE)
  adj_df <- as.data.frame(adj)
  adj_df$Node1 <- 1:45
  adj_long <- pivot_longer(adj_df, cols = -Node1, names_to = "Node2", values_to = "Edge")
  adj_long$Node2 <- as.integer(gsub("V", "", adj_long$Node2))
  adj_long$Layer <- layer_name
  return(adj_long)
}

# Combine all layers into one dataframe
adj_data <- do.call(rbind, mapply(adj_to_df, layers, names(layers), SIMPLIFY = FALSE))

# Plot heatmaps
p1 <- ggplot(adj_data, aes(x = Node1, y = Node2, fill = Edge)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, 1)) +
  facet_wrap(~ Layer, ncol = 3) +
  labs(title = "Figure S1: Adjacency Matrices for All Six Layers",
       x = "Node", y = "Node",
       caption = "Heatmaps showing connectivity patterns among 45 farmers.") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Save as PDF
ggsave("adjacency_matrices.pdf", p1, width = 10, height = 6)

# --- Figure S2: Degree Distributions (Scatter Plots) ---
# Calculate degrees and prepare data, including zeros for completeness
degree_data <- lapply(names(layers), function(layer) {
  deg <- degree(layers[[layer]])
  deg_table <- table(deg)  # Count nodes per degree
  data.frame(Degree = as.numeric(names(deg_table)),
             Count = as.numeric(deg_table),
             Layer = layer)
})
degree_df <- do.call(rbind, degree_data)

# Plot log-log scatter plots
p2 <- ggplot(degree_df, aes(x = Degree + 1, y = Count)) +  # Add 1 to shift zeros
  geom_point(color = "blue", alpha = 0.7, size = 2) +
  facet_wrap(~ Layer, ncol = 3, scales = "free_y") +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Figure S2: Degree Distributions (Log-Log) for All Six Layers",
       x = "Degree", y = "Count of Nodes",
       caption = "Log-log scatter plots (Degree + 1 to include zeros) showing degree frequencies.") +
  theme_minimal()

# Save as PDF
ggsave("degree_distributions.pdf", p2, width = 10, height = 6)

# --- Figure S3: Correlation Heatmap ---
# Correlation matrix from Table \ref{tab:correlation}
corr_matrix <- matrix(c(
  1.00, 0.04, 0.43, 0.76, 0.33, -0.08,
  0.04, 1.00, 0.17, -0.01, 0.30, 0.00,
  0.43, 0.17, 1.00, 0.47, 0.83, 0.09,
  0.76, -0.01, 0.47, 1.00, 0.24, 0.03,
  0.33, 0.30, 0.83, 0.24, 1.00, 0.12,
  -0.08, 0.00, 0.09, 0.03, 0.12, 1.00
), nrow = 6, byrow = TRUE)
rownames(corr_matrix) <- colnames(corr_matrix) <- c("FADIR", "NOFAINT", "FAMU", "NOFADIR", "NOFAMU", "FAINT")

# Create heatmap
pdf("correlation_heatmap.pdf", width = 6, height = 6)
corrplot(corr_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.srt = 45,
         title = "Figure S3: Heatmap of Pearson Correlations",
         mar = c(0, 0, 2, 0))
dev.off()

# Notify completion
cat("Figures generated: adjacency_matrices.pdf, degree_distributions.pdf, correlation_heatmap.pdf\n")