# 📈 Marketing-Attribution-Modelling-with-Markov-Chains-in-R

*   **Purpose**: This R code implements Markov Chain models for marketing attribution, helping to understand the impact of different marketing channels on conversions. It also compares Markov Chain attribution with rule-based models like First Touch, Last Touch, and Linear Touch.

*   **Libraries Used**: The code uses several R libraries:
    *   `dplyr` and `tidyr` for data manipulation.
    *   `purrr` for functional programming.
    *   `gt` for creating tables.
    *   `plotly` for interactive plots.
    *   `stringr` for string manipulation.
    *   `ChannelAttribution` for attribution modeling.

*   **Data Simulation**:
    *   The code begins by defining a theoretical transition matrix (`M`) representing the probabilities of moving between different states (Non-Conversion, Display, TV, Search, Conversion).
    *   The `simulate_path` function simulates user journeys through these states based on the transition matrix. This function randomly selects a starting touchpoint and then steps through the Markov chain until a conversion or non-conversion event occurs.
    *   The simulation generates a dataset of 10,000 paths and conversion outcomes.

*   **Data Preparation**:
    *   The simulated paths are converted into sequences of channel interactions.
    *   These sequences are then aggregated to determine the number of converters and non-converters for each unique journey.

*   **Transition Matrix Estimation**: The code estimates the transition matrix from the simulated data using the `transition_matrix` function. This allows for comparison between the known transition matrix used for simulation and the one estimated from the data.

*   **Markov Model Attribution**: The `markov_model` function from the `ChannelAttribution` package is used to calculate channel attribution based on the Markov Chain model. The removal effects and relative removal effects are calculated to understand the impact of each channel.

*   **Comparison with Rule-Based Models**: The results from the Markov Chain model are compared with those from First Touch, Last Touch, and Linear Touch attribution models using the `heuristic_models` function. This comparison highlights the differences in channel valuation between the models.

*   **Visualization**: The code includes visualizations to understand the simulated paths and compare the results of different attribution models.

*   **Key Insights**: The example in the code demonstrates how Markov Chain attribution can correct for biases inherent in simpler models like Last Touch attribution. For instance, Last Touch might underestimate the importance of channels like Display and TV compared to the Markov Chain model.
