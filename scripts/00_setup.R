rm(list = ls())

# Packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Creating the exact dataset
set.seed(34674896)

VCData <- read.csv("data/raw/WVSExtract.csv")

VC <- VCData[sample(1:nrow(VCData), 100000 , replace = FALSE), ]
VC <- VC[, c(
    1:3,
    sort(sample(4:50, 25, replace = FALSE)),
    sort(sample(51:65, 8, replace = FALSE))
)]

write.csv(VC, "data/processed/FIT3152A1Data_AlinMerchant.csv", row.names = FALSE)

# Adding Focus-Country group
VC$Group <- ifelse(VC$Country == "NLD", "Netherlands", "OtherCountries")
VC$Group <- as.factor(VC$Group)

# Defining Variables
confidence_vars <- c(
    "CChurches",
    "CArmedForces",
    "CPolice",
    "CParliament",
    "CTelevision",
    "CWomensMvt",
    "CCourts",
    "CEU"
)

predictor_vars <- setdiff(
    names(VC_clean),
    c("Wave", "Country", "Year", "Group", confidence_vars)
)
setdiff(predictor_vars, names(VC_clean))
# Clean coded missing values
VC_clean <- VC

num_cols <- names(VC_clean)[sapply(VC_clean, is.numeric)]

for (col in num_cols) {
    VC_clean[[col]][VC_clean[[col]] %in% c(-1, -2 , -4 , -5)] <- NA
}

# Useful subsets
nld <- VC_clean %>% filter(Country == "NLD")
others <- VC_clean %>% filter(Country != "NLD")

# Creating output folders
if (!dir.exists("outputs")) dir.create("outputs")
if (!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# Quick Checks
cat("Rows, columns:", dim(VC_clean), "\n")
cat("Netherlands rows:", nrow(nld), "\n")
cat("Other Countries rows:", nrow(others), "\n")
cat("Waves in NLD:", unique(sort(nld$Wave)), "\n")