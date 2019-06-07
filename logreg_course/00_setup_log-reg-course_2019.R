# Install / load packages and load data

# This script loads required packages
# Downloading first any that are not already
# downloaded.

# It also loads and previews the example dataset 
# And the 'cattle' dataset


install_if_missing <- function(pkg){
  if (!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

install_if_missing("tidyverse")
install_if_missing("boot")
install_if_missing("broom")
install_if_missing("skimr")
install_if_missing("sjPlot")
install_if_missing("finalfit")

dat <- read_csv("logreg_data_01_20190530.csv")

skim(dat)

cattle <- read_csv("cattle_data.csv")

skim(cattle)
