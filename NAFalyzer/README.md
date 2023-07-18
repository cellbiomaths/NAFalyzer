# NAF Gradient Data Evaluation

This R Shiny app is designed to assist with data evaluation of NAF gradients. 
Specifically, it provides two methods for calculating the relative distribution
of metabolites in different compartments.


## Monte Carlo Simulation

The first method is based on the fact that metabolite distributions are made from
a linear combination of the compartment-specific gradients:


B = x1 * A1 + x2 * A2 + x3 * A3

The app uses a bounded linear model to determine the optimal weights x of each
compartment, which reflects the relative distribution of the metabolite B in the
compartments A. However, experimental data has shown that an error of 10% cannot
be excluded due to the nature of splitting gradients.

To account for this, the app adds random noise to the measured distribution
following a normal distribution with a standard deviation of 5%. This leads to
more robust results, such negating infeasible relative distribution such as
x = c(0%, 0%, 100%).


## Getting Started

To use this app, simply run it in RStudio or your preferred R environment. The 
app provides a user-friendly interface for uploading and analyzing your NAF gradient
data.


## Prerequisites

This app requires R and several R packages, including shiny, dplyr, and ggplot2. These
packages can be installed using install.packages() if not already present in your R
environment.


## Input Data

The app expects a csv as input. The file should contain columns for each
compartment-specific gradient, as well as the measured distribution of the metabolite B.
The first row should contain column headers, and subsequent rows should contain data.
