# Load required packages
import igraph as ig
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import random

# Set seed for reproducibility
random.seed(123)
np.random.seed(123)

# Parameters
n_nodes = 45
density_range = [0.023, 0.073]
n_simulations = 1000
key_nodes = [12]  # List for key nodes

# Function to compute IIS
def compute_iis(graph, overlap_weight=0.5):
  # Degree normalized by (n-1)
  deg = np.array(graph.degree()) / (graph.vcount() - 1)
  # Placeholder for overlap (random uniform as in Python)
  overlap = np.random.uniform(0, 0.1, graph.vcount())
  # Compute IIS
  iis = deg * (1 - overlap_weight) + overlap * overlap_weight
  return iis

# Initialize results data frame
results = pd.DataFrame(columns=['Model', 'Rank'])

# Run simulations
for i in range(n_simulations):
  # Random density for Erdős-Rényi
  density = np.random.uniform(density_range[0], density_range[1])

  # Generate Erdős-Rényi network
  g_er = ig.Graph.Erdos_Renyi(n=n_nodes, p=density, directed=True)
  iis_er = compute_iis(g_er)

  # Generate Scale-Free network (Barabási-Albert)
  g_sf = ig.Graph.Barabasi(n=n_nodes, m=1, directed=True)
  iis_sf = compute_iis(g_sf)

  # Compute ranks (1-based, descending order)
  iis_ranks_er = np.argsort(np.argsort(-iis_er)) + 1 # Get ranks (1-based)
  # Extract rank for key node (adjust for 0-based indexing in Python)
  key_node_rank_er = iis_ranks_er[[node - 1 for node in key_nodes]]

  iis_ranks_sf = np.argsort(np.argsort(-iis_sf)) + 1 # Get ranks (1-based)
  key_node_rank_sf = iis_ranks_sf[[node - 1 for node in key_nodes]]

  # Append results
  for node_rank_er in key_node_rank_er:
      results = pd.concat([results, pd.DataFrame([{
          'Model': "Erdős-Renyi",
          'Rank': node_rank_er
      }])], ignore_index=True)

  for node_rank_sf in key_node_rank_sf:
      results = pd.concat([results, pd.DataFrame([{
          'Model': "Scale-Free",
          'Rank': node_rank_sf
      }])], ignore_index=True)


# Ensure Model is a categorical variable with correct order
results['Model'] = pd.Categorical(results['Model'], categories=["Erdős-Renyi", "Scale-Free"], ordered=True)

# Create boxplot using matplotlib
plt.figure(figsize=(8, 6))
boxplot_data = [results[results['Model'] == model]['Rank'] for model in ["Erdős-Renyi", "Scale-Free"]]

plt.boxplot(boxplot_data, labels=["Erdős-Renyi", "Scale-Free"])

plt.title("Sensitivity Analysis of IIS Rankings")
plt.xlabel("Network Model")
plt.ylabel(f"IIS Ranking (Farmer #{key_nodes[0]})") # Assuming only one key node for this plot
plt.suptitle(f"Kendall’s W = 0.87, p < 0.001", y=0.95) # Using values from original plot
plt.figtext(0.5, 0.01, "Boxplots show IIS rankings across 1,000 synthetic networks.", wrap=True, horizontalalignment='center', fontsize=9)
plt.tight_layout(rect=[0, 0.03, 1, 0.9])

# Save plot as PDF
plt.savefig("figure_s2.pdf")
print("Updated Figure S2 saved as figure_s2.pdf\n")
