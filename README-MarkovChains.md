**Markov Chain Models for predicting sequential animal behavior**

This code analyzes and processes behavioral data, particularly focusing on animal behavior and interactions.
It aims to visualise and analyse behavior patterns and modeling interactions between different fish.

**What is the significance of using Markov Chain Modelling?**

Markov Chains are well-suited for predicting sequential animal behavior as they capture the probabilistic nature of transitions between states, where future behavior depends only on the current state. 
Their ability to handle stochastic processes makes them a powerful tool for forecasting behavioral sequences in any environment, especially as utilised here for animal behavior.

**Key Components:**

**1. Custom Edge Labeling for Network Graphs:**
 - The project includes a custom function `my_draw_networkx_edge_labels` to draw edge labels on network graphs with customizable parameters,
   making it easier to visualize connections between different nodes and behaviors. This was adapted from this Stack Overflow post: 

https://stackoverflow.com/questions/22785849/drawing-multiple-edges-between-two-nodes-with-networkx

**2. Behavior Data Processing:**
   - Several functions manipulate behavioral data.
   - For instance, behavior data is looped over, edited, and appended to lists for further analysis, ensuring unique behavior sets and identifiers are maintained.

**3.Normalization Function:**
-  The `normalize_dict_to_range` function normalizes values in a given dictionary to a specified range, helping scale the data for consistent comparison and visualization.

**4. Permutations Generation:**
   -The project generates permutations of behavior combinations to study possible interactions between behaviors, including self-interactions (e.g., `dig|dig`, `chase_male|chase_male`).
    These combinations are integral for analyzing behavior patterns in various contexts.

**5.  Aesthetic fixed positions of nodes and color mapping**
   - This code enhances the visual clarity and interpretability of network graphs through the use of fixed node positions and color mapping.
   - By assigning fixed positions to nodes, the spatial arrangement remains consistent across multiple visualizations, allowing for easier comparison of behavior patterns.
   - This stability ensures that users can focus on changes in the connections between behaviors over time.
   - Additionally, the implementation of color mapping distinguishes nodes and edges based on behavior types or interaction frequencies, making it easier to identify key patterns, such as frequently occurring behaviors or significant interactions.

**Dependencies:**
- Python 3.x
- NetworkX for graph visualizations
- Matplotlib for plotting
- itertools for permutations generation
