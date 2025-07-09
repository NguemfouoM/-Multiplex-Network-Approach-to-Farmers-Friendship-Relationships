install.packages("ggraph") # Install the ggraph package if you don't have it
library(ggraph) # Load the package
library(igraph)
library(ggraph)
library(ggplot2)

# 1. Create mock intervention network
set.seed(42)
nodes <- data.frame(
  name = c(paste0("FADIR_", 1:5), paste0("FAMU_", 1:10)),
  type = rep(c("FADIR Hub", "FAMU Group"), times = c(5,10))
)

edges <- data.frame(
  from = sample(nodes$name[nodes$type=="FADIR Hub"], 15, replace = TRUE),
  to = sample(nodes$name[nodes$type=="FAMU Group"], 15, replace = TRUE)
)

# 2. Generate the plot (Figure 5)
g <- graph_from_data_frame(edges, vertices = nodes)

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.5, width = 0.7) +
  geom_node_point(aes(color = type, shape = type), size = 8) +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  labs(title = "Proposed Intervention: Pairing FADIR Hubs with FAMU Groups",
       subtitle = "Dashed lines indicate planned extension program connections",
       color = "Node Type", shape = "Node Type") +
  scale_color_manual(values = c("FADIR Hub" = "#E41A1C", "FAMU Group" = "#377EB8")) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "bottom")
# Save figure
ggsave("Intervention_validation.png", combined_plot, width = 8, height = 6, dpi = 300)