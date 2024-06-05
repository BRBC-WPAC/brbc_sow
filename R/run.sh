#! /usr/bin/env bash

set -e # Exit on error
set -x # Print commands

# Run the R scripts

# 1: Build the consolidated CSV and daily flow files from the source data
Rscript compile_and_consolidate_data.R

# 2: Populate duckdb database from CSV files
Rscript populate_database.R

# 3: Generate the individual image files for each station and variable
Rscript generate_plots.R

# 4: Compile report