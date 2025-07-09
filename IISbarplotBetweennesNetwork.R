# Load packages
library(igraph)
library(ggplot2)

# Simulate multiplex network data with 45 nodes explicitly
set.seed(123)
layers <- c("FADIR", "FAINT", "FAMU", "NOFADIR", "NOFAINT", "NOFAMU")
edge_counts <- c(72, 35, 26, 33, 27, 23)
g_list <- list()

for (i in 1:6) {
  g <- make_empty_graph(n = 45, directed = FALSE)
  V(g)$name <- 1:45
  edges <- sample(1:45, edge_counts[i] * 2, replace = TRUE)
  edges <- matrix(edges, ncol = 2)
  g <- add_edges(g, edges)
  g_list[[layers[i]]] <- g
}

# Compute degree centrality for each layer
degrees <- lapply(g_list, degree)

# Compute overlapping edges (w_{\alpha\beta})
overlap <- matrix(0, 6, 6, dimnames = list(layers, layers))
for (i in 1:5) {
  for (j in (i+1):6) {
    e1 <- as_edgelist(g_list[[layers[i]]])
    e2 <- as_edgelist(g_list[[layers[j]]])
    overlap[i,j] <- length(intersect(paste(e1[,1], e1[,2]), paste(e2[,1], e2[,2]))) /
      min(nrow(e1), nrow(e2))
    overlap[j,i] <- overlap[i,j]
  }
}

# Compute Interlayer Influence Score (IIS)
iis <- rep(0, 45)
for (i in 1:45) {
  score <- 0
  for (a in 1:5) {
    for (b in (a+1):6) {
      if (degrees[[layers[b]]][i] > 0) {
        score <- score + overlap[a,b] * degrees[[layers[a]]][i] / degrees[[layers[b]]][i]
      }
    }
  }
  iis[i] <- score
}

# Figure 2: IIS Bar Plot
print("Generating Figure 1: IIS Bar Plot")
iis_df <- data.frame(Node = 1:45, IIS = iis)
top5_iis <- head(iis_df[order(-iis_df$IIS),], 5)
ggplot(top5_iis, aes(x = factor(Node), y = IIS)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Farmer (Node ID)", y = "Interlayer Influence Score (IIS)",
       title = "Top 5 Farmers by Interlayer Influence Score") +
  theme_minimal()
ggsave("iis_barplot.png", width = 5, height = 3, dpi = 300)
print("Figure 1 saved as iis_barplot.png")

# Figure 3: Betweenness Centrality Bar Plot (FADIR Layer)
print("Generating Figure 2: Betweenness Centrality Bar Plot")
fadir_g <- g_list[["FADIR"]]
betweenness_scores <- betweenness(fadir_g)
betweenness_df <- data.frame(Node = 1:45, Betweenness = betweenness_scores)
top5_betweenness <- head(betweenness_df[order(-betweenness_df$Betweenness),], 5)
ggplot(top5_betweenness, aes(x = factor(Node), y = Betweenness)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Farmer (Node ID)", y = "Betweenness Centrality",
       title = "Top 5 Farmers by Betweenness Centrality in FADIR Layer") +
  theme_minimal()
ggsave("betweenness_barplot.png", width = 5, height = 3, dpi = 300)
print("Figure 2 saved as betweenness_barplot.png")

# Figure 6: Network with IIS-Sized Nodes (FADIR Layer)
print("Generating Figure 3: Network Plot")
V(fadir_g)$size <- pmin(iis * 10, 50)
png("iis_network.png", width = 5, height = 5, units = "in", res = 300)
plot(fadir_g, vertex.label = NA, vertex.size = V(fadir_g)$size,
     edge.color = "gray", layout = layout_with_fr(fadir_g),
     main = "FADIR Layer with Nodes Sized by IIS")
dev.off()
print("Figure 3 saved as iis_network.png")