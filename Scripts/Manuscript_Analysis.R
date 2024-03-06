# Analysis for SoilHet2 Manuscript ---------------------------------------------------------------------------
# This README provides an overview of the R code used for the primary analysis in the Article: "Spatial pattern of 
# seed arrival  has a greater effect on plant diversity than does soil heterogeneity in a grassland ecosystem" by 
# Kjaer et al. (2024) The code is organized into sections for better understanding and readability.

# NOTE: Due to issues with the "Matrix" package, this code does not work in R v4.3.2 (specifically the code that
# calculates linear and generalized linear models). It does work in v4.1.2 when the code was previously written.

# Libraries
# The following R libraries are loaded for the analysis:
 
# Rmisc: Used for summary statistics.
# tidyverse: Provides various data manipulation and visualization tools.
# vegan: Utilized for species accumulation curves.
# codyn: Used for diversity and richness metrics.
# lme4: Necessary for running Generalized Linear Mixed Models (GLMMs).
# lmerTest: Provides p-values for GLMMs.
# car: Used for ANOVA for GLMMs.
# pwr: Assesses statistical power and Type II Error.
# emmeans: Used for making pairwise comparisons.
# cowplot: Utilized to combine figures into one figure.

# Importing Data
# Data is imported from two sources:

# SoilHet2_PlotSppComp_2020.csv: Contains data related to species cover, year, and various factors.
# SoilHet2PlotFactors.txt: Contains information about plot factors such as SoilHet, Seeding, and Scale.

# Data Wrangling
# Data columns are appropriately converted to factors and manipulated as needed.
# Relative cover values are calculated.
# The dataset is filtered for specific conditions.

# Community Structure Analysis
# Community structure analysis is performed on the dataset.
# Evenness values for the year 2020 are selected, and experimental factors are added.
# Overdispersion in the model is checked.
# Type I and Type II Error Rate Testing functions are defined to calculate Type I and Type II error rates for the 
# GLMM model, specifically for each factor and interaction.
# These functions include typeI.test, effsize, and overdisp_fun.

# Richness Analysis

# Total Richness
# A GLMM model is fitted to assess total richness.
# The summary, diagnostic plots, and overdispersion are checked.
# Analysis of variance is performed.
# Least squares means and confidence intervals are calculated.
# Type I and Type II error rates are calculated for each factor and interaction.

# Sown Species Richness
# A GLMM model is fitted to assess sown species richness.
# The summary and analysis of variance are checked.
# Least squares means and confidence intervals are calculated.

# Non-Sown Species Richness
# A GLMM model is fitted to assess non-sown species richness.
# The summary and analysis of variance are checked.
# Least squares means and confidence intervals are calculated.

# Richness Graphs
# Graphs are created to visualize richness data, including total richness, sown species richness, and non-sown
# species richness. These graphs provide insights into the effects of SoilHet, Seeding, and Scale on richness.
 
# Saving Graphs
# The final combined richness graph is saved as a PNG file named Fig3.png.
 
# Please note that this README provides a high-level overview of the analysis performed using the provided R code.
# Additional details, such as specific results and interpretations, would typically be included in the manuscript
# or supplementary materials.

# Using This Code
# Before running the script, ensure that the required R packages are installed (and that the Matrix package is no
# longer causing issues in R v4.3.2). The script uses the publiclly available data found in the SoilHet2 Plot Species
# composition Excel file. Please note that the data used in this script and in this GitHub directory only contain data
# from 2018 through 2020. Additional years may be availble on the SoilHet2 figshare directory. 
# (https://figshare.com/projects/SoilHet2/117219)
# For questions or issues, please contact Esben Kjaer.
 
# The code was last updated on 6Mar24. 

# For any questions or issues related to this script, please refer to the script's author/maintainer.
# Author and Maintainer
# Author: Esben Kjaer
# Date: 6Mar24
# Email: elkjaer1745@gmail.com; esben.kjaer@ndsu.edu

# Libraries ---------------------------------------------------------------------------

library(Rmisc)                                         # loads Rmisc -- used for summary stats
library(tidyverse)                                     # loads tidyverse -- used for general accesibility
library(readxl)                                        # loads readxl -- used for importing excel sheets
library(vegan)                                         # loads vegan -- used for species accumulation curves
library(codyn)                                         # loads the codyn package -- used for diversity and richness metrics
library(lme4)                                          # loads lme4 -- used to run GLMMs
library(lmerTest)                                      # loads lmerTest -- used to provide p-values for GLMMs
library(car)                                           # loads car -- used for Anova (ANOVA for GLMMs)
library(pwr)                                           # loads pwr -- used to asses statistical power and Type II Error
library(emmeans)                                       # loads emmeans -- used to make pairwise comparisons
library(cowplot)                                       # loads cowplot -- used to combine figures into one figure


# Importing Data ---------------------------------------------------------------------------------------------

Sh2Comb <- read_excel(                                 
  "Data/SoilHet2_PlotSppComp_2020.xlsx",
  col_names = T,
  sheet = 1) |>
  as_tibble()
Sh2Comb$Spp <- as_factor(Sh2Comb$Spp)
head(Sh2Comb)
#Date - the date that the data were recorded
#Plot - the plot where data were recorded
#TxCode - combination of SoilHet, Seeding, and Scale factors -- see below
#SoilHet - Soil type in each plot: Het = Heterogenous, Hom = Homogenous, Non = not manipulated
#Seeding - sowing practice in each plot: Agg = aggregated, Uni = Uniform, Non = not sown
#Scale - the patch size: Lg = large (.4m x .4m), Sm = small (.2m x .2m), No = no patch size
#Spp - species codes - first 3 letter of the genus and the first 3 letters of the epithet
#Cover - percent cover
#Year - year the data was collected

Sh2Factors <- read_table2(                            # importing Plot factors; need to use "read_table2" because it
  # more readily accepts tab-delimited files
  "Other/SoilHet2PlotFactors.txt")                    # path to file
#Plot - the plot where data were recorded
#TxCode - combination of SoilHet, Seeding, and Scale factors -- see below
#SoilHet - Soil type in each plot: Het = Heterogenous, Hom = Homogenous, Non = not manipulated
#Seeding - sowing practice in each plot: Agg = aggregated, Uni = Uniform, Non = not sown
#Scale - the patch size: Lg = large (.4m x .4m), Sm = small (.2m x .2m), No = no patch size


# Data Wrangling ---------------------------------------------------------------------------------------------

Sh2Factors$Plot <-
  as.factor(Sh2Factors$Plot)                           # changes Plot to be a factor, that way it can be graphed
# correctly

#Calculate Relative Cover
Sh2Comb2 <-
  Sh2Comb |>                                           # create a new data frame called Sh2Comb2 and then
  group_by(Plot, Year) |>                              # group by plot and then
  dplyr::summarise(TotalVegCover = sum(Cover))         # sum Cover into a new colum "TotalVegCover

Sh2Comb3 <-
  left_join(                                           # create a new data frame called Sh2Comb3 and then
    Sh2Comb,                                           # add column to Sh2Comb based on matching "Plot
    Sh2Comb2, by = c("Plot","Year")) |>                # data in Sh2Comb and then
  mutate(RelCover =                                    # calculate Relative cover by dividing
           (Cover / TotalVegCover) * 100)              # cover by relcover and multiplying by 100

# Create a new data frame called Sh220 that filters Sh2Comb3 for the year 2020 and soil heterogeneity that is not "Non"
Sh220 <- Sh2Comb3 |>  
  filter(Year == 2020) |>                              # include only rows where the `Year` column is equal to 2020
  filter(SoilHet != "Non")                             # include only rows where `SoilHet` is not equal to "Non"

# Ungroup Sh220 and select only the Cover and TotalVegCover columns
Sh2Mat20 <- Sh220 |>  
  ungroup() |>                                         # Remove any grouping structure from the data frame
  select(-Cover,                                       # Remove the `Cover` column from the data frame
              -TotalVegCover)                          # Remove the `TotalVegCover` column from the data frame

# Pivot Sh2Mat20 wider, with the species names as the column names and the relative cover values as the values. Fill missing values with 0.
Sh2Mat20 <- Sh2Mat20 |> 
  pivot_wider(names_from = "Spp",                      # Use the `Spp` column for the column names
              values_from = "RelCover",                # Use the `RelCover` column for the values
              values_fill = 0)                         # Fill missing values with 0

# Creating a variable that lists whether or not a species was sown or unsown.
# the code conditionally fills the column, if the species is listed below (all sown species) it is assigned the
# Sown tag, all other species are listed as unsown
Sh2Comb3$Sown <-
  ifelse(
    Sh2Comb3$Spp %in% "ACHMIL" |
      Sh2Comb3$Spp %in% "AMOCAN" |
      Sh2Comb3$Spp %in% "ANDGER" |
      Sh2Comb3$Spp %in% "ASCSPE" |
      Sh2Comb3$Spp %in% "ASCTUB" |
      Sh2Comb3$Spp %in% "BAPAUS" |
      Sh2Comb3$Spp %in% "BOUCUR" |
      Sh2Comb3$Spp %in% "DALPUR" |
      Sh2Comb3$Spp %in% "DESCAN" |
      Sh2Comb3$Spp %in% "DESILL" |
      Sh2Comb3$Spp %in% "ECHANG" | 
      Sh2Comb3$Spp %in% "ELYCAN" |
      Sh2Comb3$Spp %in% "EUPPER" |
      Sh2Comb3$Spp %in% "PSEOBT" | 
      Sh2Comb3$Spp %in% "HELHEL" |
      Sh2Comb3$Spp %in% "HELMAX" |
      Sh2Comb3$Spp %in% "HYPPUN" | 
      Sh2Comb3$Spp %in% "LESCAP" |
      Sh2Comb3$Spp %in% "LESVIR" |
      Sh2Comb3$Spp %in% "LIAPYC" | 
      Sh2Comb3$Spp %in% "MIMQUA" |
      Sh2Comb3$Spp %in% "MONFIS" |
      Sh2Comb3$Spp %in% "OENMAC" | 
      Sh2Comb3$Spp %in% "PANVIR" |
      Sh2Comb3$Spp %in% "PENCOB" |
      Sh2Comb3$Spp %in% "RATCOL" | 
      Sh2Comb3$Spp %in% "RATPIN" |
      Sh2Comb3$Spp %in% "RUDHIR" |
      Sh2Comb3$Spp %in% "SALAZU" |
      Sh2Comb3$Spp %in% "SCHSCO" |
      Sh2Comb3$Spp %in% "SENMAR" |
      Sh2Comb3$Spp %in% "SILLAC" | 
      Sh2Comb3$Spp %in% "SOLRIG" |
      Sh2Comb3$Spp %in% "SORNUT" |
      Sh2Comb3$Spp %in% "SPOCOM" | 
      Sh2Comb3$Spp %in% "SYMOBL" |
      Sh2Comb3$Spp %in% "TEUCAN" |
      Sh2Comb3$Spp %in% "TRAOHI" | 
      Sh2Comb3$Spp %in% "TRIFLA" |
      Sh2Comb3$Spp %in% "VERHAS",
    "Sown",
    "Unsown")

PlotE <- community_structure(                         # assess the community structure
  Sh2Comb3,                                           # uses the sh2 dataset
  time.var="Year",                                    # no time variable
  abundance.var ="RelCover",                          # abundance is based on cover
  replicate.var="Plot",                               # eveness will be separated based on scale
  metric = "Evar")                                    # uses the Evar evenness metric



# Select evenness values from 2020 only and adds in the experimental factors
PlotE <- PlotE |> 
  filter(Year == "2020") |>                           # Selects only the year 2020
  mutate(Plot = as.factor(Plot)) |>                   # Converts the Plot column to a factor
  left_join(Sh2Factors,                                # Joins the Sh2Factors data frame on the Plot column
            by = "Plot")

# Removes the Year and Evar columns
PlotE <- PlotE |> 
  filter(Year == 2020) |>                              # Removes all rows where the Year column is not equal to 2020
  select(-Year, -Evar)                                 # Removes the Year and Evar columns


Sh2Mat20$Plot <- as.factor(Sh2Mat20$Plot)              # Converts the Plot column in Sh2Mat20 to a factor


Sh2Mat20 <- Sh2Mat20 |>                                # Filters Sh2Mat20 to only include rows where the Seeding 
  filter(Seeding != "Non")                             # column is not equal to "Non"


Sh2Mat20 <- left_join(Sh2Mat20, PlotE)                 # Performs a left join on Sh2Mat20 and PlotE

Sh2Mat20 <-  separate(                                 # creates two new columns from "Plot" called "Block" and remove
  Sh2Mat20,                                            # these data are filled with split data from Plot, with the first
  Plot,                                                # character from the plot column going to the "Block" column, and
  c("Block","remove"),                                 # everything else goes into the "remove" column, all while
  sep=1,                                               # retaining the "Plot" column
  remove=F)
Sh2Mat20$remove <- NULL                          
Sh2Mat20$Block <- as.factor(Sh2Mat20$Block)

# Function to test Type I error rate
# Arguments:
# - mu0: The hypothesized population mean
# - se: The standard error of the population mean
# - n: The sample size
# - alpha: The significance level for the test
# - iterations: The number of iterations to run in the simulation (default is 10,000)
typeI.test <- function(mu0, se, n, alpha, iterations = 10000) {
  pvals <- replicate(iterations, {                     # Create an empty vector to store p-values from each iteration
    temporary.sample <- rnorm(n = n, mean = mu0, sd = (se * n)) # Simulate a data set with the specified mean, sample size, and standard deviation
    pval <- 1 - pt((mean(temporary.sample) - mu0) / (sd(temporary.sample) / sqrt(n)), df = n - 1) # Calculate the p-value for a one-sample t-test comparing the sample mean to mu0
    pval                                               # Return the calculated p-value for this iteration
  })
  mean(pvals < alpha)                                  # Calculate the proportion of p-values less than alpha
}

# Function to calculate effect size
# Arguments:
# - t: The t-statistic from a hypothesis test
# - df: The degrees of freedom associated with the test
effsize <- function(t, df) {
  d <- (2 * t) / sqrt(df)                              # Calculate Cohen's d
  r <- sqrt((t^2) / ((t^2) + df))                      # Calculate the effect size
  cat("Cohen's d:", d, "\n")                           # Print Cohen's d and 
  cat("Effect size:", r, "\n")                         # effect size
}

# Function to check for overdispersion in a model
# Arguments:
# - model: The statistical model object to check for overdispersion
overdisp_fun <- function(model) {
  vpars <- function(m) nrow(m) * (nrow(m) + 1) / 2     # Function to calculate the number of variance parameters
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))   # Calculate the model degrees of freedom
  rdf <- nrow(model.frame(model)) - model.df           # Calculate the residual degrees of freedom
  rp <- residuals(model, type = "pearson")             # Extract Pearson residuals
  Pearson.chisq <- sum(rp^2)                           # Calculate Pearson Chi-square
  prat <- Pearson.chisq / rdf                          # Calculate the ratio of Pearson Chi-square to residual degrees of freedom
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE) # Calculate the p-value based on Chi-square distribution
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval) # Return a list of results including Chi-square value, ratio, residual degrees of freedom, and p-value
}


Sh2Mat20 <- Sh2Mat20 |>                                # Arranges the data frame by SoilHet, Seeding, and Scale
  arrange(SoilHet, desc(Seeding), desc(Scale))

options(contrasts = c("contr.sum", "contr.poly"))      # Sets the contrasts to sum and polynomial

Sh2Comb20S <- Sh2Comb3 |>                              # Filters the data frame to only include 
  filter(Year == "2020") |>                            # the year 2020, 
  filter(SoilHet != "Non") |>                          # soil heterogeneity that is not "Non", 
  filter(Sown == "Sown")                               # and sowing that is "Sown"

PlotEvarS <- community_structure(Sh2Comb20S,           # Calculates evenness for Sh2Comb20S
                                 time.var = NULL,      # The time variable is set to NULL
                                 abundance.var = "RelCover", # The abundance variable is set to "RelCover"
                                 replicate.var = "Plot", # The replicate variable is set to "Plot"
                                 metric = "Evar")      # The metric is set to "Evar"

Sown20 <- left_join(Sh2Comb20S,                        # Performs a left join on Sh2Comb20S and PlotEvarS,
                    PlotEvarS,                         #  joining on the Plot column
                    by = "Plot")

Sown20 <-  separate(Sown20,                            # Creates two new columns from "Plot" called "Block" and remove
                    Plot,                              # These data are filled with split data from Plot, with the first
                    c("Block", "remove"),              # character from the plot column going to the "Block" column, and
                    sep = 1,                           # everything else goes into the "remove" column, all while
                    remove = F)                        # retaining the "Plot" column

Sown20$remove <- NULL                                  # Removes the "remove" column

Sown20$Block <- as.factor(Sown20$Block)                # Converts the "Block" column to a factor

Sown20 <- Sown20 |> 
  ungroup() |>                                         # Ungroups the data frame
  dplyr::select(Plot:Scale,                            # and selects Plot through Block, Scale, and richness columns
                Block,
                richness) |> 
  unique()

Sh2Comb20U <- Sh2Comb3 |>                              # Filters the data frame 
  filter(Year == "2020") |>                            # to only include the year 2020,
  filter(SoilHet != "Non") |>                          #  soil heterogeneity that is not "Non", 
  filter(Sown == "Unsown")                             # and sowing that is "Unsown"

PlotEvarU <- community_structure(Sh2Comb20U,           # Calculates evenness for Sh2Comb20U
                                 time.var = NULL,      # The time variable is set to NULL
                                 abundance.var = "RelCover", # The abundance variable is set to "RelCover"
                                 replicate.var = "Plot", # The replicate variable is set to "Plot"
                                 metric = "Evar")      # The metric is set to "Evar"

PlotEvarU$Evar <- PlotEvarU$Evar |> 
  replace_na(0)                                        # Replaces any missing values in the Evar column with 0

Unsown20 <- left_join(Sh2Comb20U,                      # Performs a left join on Sh2Comb20U and 
                      PlotEvarU,                       # PlotEvarU, 
                      by = "Plot")                     # joining on the Plot column

Unsown20 <-  separate(Unsown20,                        # Creates two new columns from "Plot" called "Block" and remove
                      Plot,                            # These data are filled with split data from Plot, with the first
                      c("Block", "remove"),            # character from the plot column going to the "Block" column, and
                      sep = 1,                         # everything else goes into the "remove" column, all while
                      remove = F)                      # retaining the "Plot" column
Unsown20$remove <- NULL                          
Unsown20$Block <- as.factor(Unsown20$Block)

Unsown20 <- Unsown20 |> 
  ungroup() |>                                         # Ungroups the data frame
  dplyr::select(Plot:Scale,                            # and selects only Plot thorugh Block, Scale, and richness columns
                Block,
                richness) |> 
  unique()

Sowing.labs <-
  c("Uni" = "Uniform Sowing",                          # sets up a vector that contains equivallency between Scale
    "Agg" = "Aggregated Sowing")                       # abbreviations and the full word -- used for labelling facets
# ggplot

Scale.labs <-
  c("Sm" = "Small Patches",                            # sets up a vector that contains equivallency between Scale
    "Lg" = "Large Patches")                            # abbreviations and the full word -- used for labelling facets

# Richness Analysis ------------------------------------------------------------------------------------------

## Total Richness --------------------------------------------------------------------------------------------


GLMRPi <- glmer(richness ~ SoilHet * Seeding * Scale + (1 | Block), # Fits a generalized linear mixed model with random effect
                data = Sh2Mat20,                       # The data is the Sh2Mat20 data frame
                family = poisson(link = "identity"))   # The family is set to poisson with identity link function

summary(GLMRPi)                                        # Prints the summary of the GLMM model

plot(GLMRPi)                                           # Plots the diagnostic plots for the GLMM model

overdisp_fun(GLMRPi)                                   # Checks for overdispersion in the GLMM model

Anova(GLMRPi,                                          # Performs an analysis of variance on the GLMM model
      type = 3)                                        # Type 3 sums of squares are used

cld.a <- multcomp::cld(emmeans(GLMRPi,                 # Calculates the least squares means and confidence intervals for the GLMM model
                               list(pairwise ~ SoilHet * Seeding * Scale),
                               adjust = "tukey"))      # Tukey's HSD method is used for multiple comparisons

emmeans(GLMRPi,                                        # Calculates the least squares means and confidence intervals for the GLMM model
        list(pairwise ~ Scale | Seeding),              # Pairwise comparisons are performed for the Scale and Seeding factors
        adjust = "tukey")                              # Tukey's HSD method is used for multiple comparisons


a <-                                                   # Create a vector that contains each factor and interaction
  c(                                                   # allows for us to assign each type-I and type-II errorrates
    "Factor",                                          # to it's specific factor or factor interaction
    "SoilHet",                                         # follows the same order as reported on in the GLMM summary
    "Seeding",
    "Scale",
    "SoilHetxSeeding",
    "SoilHetxScale",
    "SeedingxScale",
    "SoilHetxSeedingxScale")

b <- c(                                                # creating a vector that contains the type-I error rates
  "Type-I Error",                                      # follows the same order as reported on in the GLMM summary
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 32,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 32,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 32,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 16,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 16,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 16,                                            # sample size of each treatment
    alpha = 0.05),                                     # alpha or p-value being used to indicate significance
  
  typeI.test(                                          # calculate type-I error for SoilHet predictors
    mu0 = 32.47222,                                    # average richness across all treatments
    se = 0.67093,                                      # standard error - taken from GLMM summary
    n = 8,                                             # sample size of each treatment
    alpha = 0.05))                                     # alpha or p-value being used to indicate significance

soilhet <- as.numeric(effsize(t = -0.246, df = 1)[2])  # calculates effect size, t value take from model 
seeding <- as.numeric(effsize(t = 10.950, df = 1)[2])  # calculates effect size, t value take from model
scale <- as.numeric(effsize(t = 3.155, df = 1)[2])     # calculates effect size, t value take from model
soilseed <- as.numeric(effsize(t = 0.422, df = 1)[2])  # calculates effect size, t value take from model
soilsc <- as.numeric(effsize(t = -0.996, df = 1)[2])   # calculates effect size, t value take from model
seedsc <- as.numeric(effsize(t = 2.639, df = 1)[2])    # calculates effect size, t value take from model
soiseesc <- as.numeric(effsize(t = 2.682, df = 1)[2])  # calculates effect size, t value take from model


c <- c(                                                # create a vector that contains the type-II error for each factor 
  "Type-II Error",                                     # and interactions follows the same pattern as GLMM summary
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = soilhet,                                     # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = seeding,                                     # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                       # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = scale,                                       # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = soilseed,                                    # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = soilsc,                                      # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = seedsc,                                      # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]),                               # calculate power (power is unknown or NULL) - pull power value
                                                       # for type-II error rate calculation
  
  1 - as.numeric(                                      # type-II error is equal to 1-power
    pwr.chisq.test(                                    # function to calculate power for Chi square tests
      w = soiseesc,                                    # effect size
      N = 64,                                          # total sample size
      df = 1,                                          # degrees of freedom
      sig.level = 0.05,                                # alpha or p-value being used for significance
      power = NULL)[5]))

d <- cbind(a, b, c)                                    # combine the three vectors into one table that contains the
                                                       # Type-I and Type-II error rates for all fixed factors and 
                                                       # their interactions 
d


## Sown Species Richness -------------------------------------------------------------------------------------

SownGLM.p <- glmer(richness ~ SoilHet * Seeding * Scale + (1 | Block), # Fits a generalized linear mixed model with Poisson error distribution and log link function
                   data = Sown20,                      # The data is the Sown20 data frame
                   family = poisson(link = "log"))     # The family is set to poisson with log link function

summary(SownGLM.p)                                     # Prints the summary of the GLMM model

Anova(SownGLM.p,                                       # Performs an analysis of variance on the GLMM model
      type = 3)                                        # Type 3 sums of squares are used

multcomp::cld(emmeans(SownGLM.p,                       # Calculates the least squares means and confidence intervals for the GLMM model
                      list(pairwise ~ SoilHet * Seeding * Scale),
                      adjust = "tukey"))               # Tukey's HSD method is used for multiple comparisons

## Non-Sown Species Richness ---------------------------------------------------------------------------------

UnsownLM <- glmer(richness ~ SoilHet * Seeding * Scale + (1 | Block), # Fits a generalized linear mixed model with Poisson error distribution and identity link function
                  family = poisson(link = "identity"),
                  data = Unsown20)

summary(UnsownLM)                                      # Prints the summary of the GLMM model

Anova(UnsownLM,                                        # Performs an analysis of variance on the GLMM model
      type = 3)                                        # Type 3 sums of squares are used

emmeans(UnsownLM,                                      # Calculates the least squares means and confidence intervals for the GLMM model
        list(pairwise ~ Seeding | Scale | SoilHet),    # Pairwise comparisons are performed for the Seeding, Scale, and SoilHet factors
        adjust = "tukey")                              # Tukey's HSD method is used for multiple comparisons

emmeans(UnsownLM,                                      # Calculates the least squares means and confidence intervals for the GLMM model
        list(pairwise ~ Scale | Seeding | SoilHet),    # Pairwise comparisons are performed for the Scale, Seeding, and SoilHet factors
        adjust = "tukey")

emmeans(UnsownLM,                                      # Calculates the least squares means and confidence intervals for the GLMM model
        list(pairwise ~ SoilHet | Seeding | Scale),    # Pairwise comparisons are performed for the SoilHet, Seeding, and Scale factors
        adjust = "tukey")

multcomp::cld(emmeans(UnsownLM,                        # Calculates the least squares means and confidence intervals for the GLMM model
                      list(pairwise ~ SoilHet * Seeding * Scale),
                      adjust = "tukey"))               # Tukey's HSD method is used for multiple comparisons

# Richness Graph Making -----------------------------------------------------------------------------------------------

Sh2Mat20 <- Sh2Mat20 |>
  dplyr::arrange(Scale) |>                              # Arranges the data frame by Scale in descending order
  mutate(Scale.f = factor(Scale, levels = c("Sm","Lg")))
Sown20 <- Sown20 |>
  arrange(desc(Scale)) |>                              # Arranges the data frame by Scale in descending order
  mutate(Scale.f = factor(Scale, levels = c("Sm","Lg")))
Unsown20 <- Unsown20 |>
  arrange(desc(Scale)) |>                              # Arranges the data frame by Scale in descending order
  mutate(Scale.f = factor(Scale, levels = c("Sm","Lg")))

txcode <- unique(Sh2Mat20$TxCode)

clds <- data.frame(TxCode = c(replicate(3,c("HomUniSm",
                                        "HetUniSm",
                                        "HomAggSm",
                                        "HetAggSm",
                                        "HomUniLg",
                                        "HetUniLg",
                                        "HomAggLg",
                                        "HetAggLg"))),
                   Sown = c(replicate(8,"Total"),replicate(8,"Sown"),replicate(8,"Unsown")))

clds <- clds |> 
  separate(TxCode,
           into = c("SoilHet","Extra"),
           sep = 3,
           remove = F) |> 
  separate(Extra,
           into = c("Seeding","Scale"),
           sep = 3,
           remove = T) |> 
  mutate(CLD = case_when(Sown == "Total" & TxCode == "HomUniSm" ~ "A",
                         Sown == "Total" & TxCode == "HetUniSm" ~ "AB",
                         Sown == "Total" & TxCode == "HomAggSm" ~ "ABC",
                         Sown == "Total" & TxCode == "HetAggSm" ~ "ABC",
                         Sown == "Total" & TxCode == "HomUniLg" ~ "ABC",
                         Sown == "Total" & TxCode == "HetUniLg" ~ "A",
                         Sown == "Total" & TxCode == "HomAggLg" ~ "BC",
                         Sown == "Total" & TxCode == "HetAggLg" ~ "C",
                         Sown == "Sown" & TxCode == "HomUniSm" ~ "A",
                         Sown == "Sown" & TxCode == "HetUniSm" ~ "A",
                         Sown == "Sown" & TxCode == "HomAggSm" ~ "B",
                         Sown == "Sown" & TxCode == "HetAggSm" ~ "B",
                         Sown == "Sown" & TxCode == "HomUniLg" ~ "A",
                         Sown == "Sown" & TxCode == "HetUniLg" ~ "A",
                         Sown == "Sown" & TxCode == "HomAggLg" ~ "B",
                         Sown == "Sown" & TxCode == "HetAggLg" ~ "B",
                         Sown == "Unsown" & TxCode == "HomUniSm" ~ "A",
                         Sown == "Unsown" & TxCode == "HetUniSm" ~ "ABC",
                         Sown == "Unsown" & TxCode == "HomAggSm" ~ "ABC",
                         Sown == "Unsown" & TxCode == "HetAggSm" ~ "ABC",
                         Sown == "Unsown" & TxCode == "HomUniLg" ~ "AB",
                         Sown == "Unsown" & TxCode == "HetUniLg" ~ "A",
                         Sown == "Unsown" & TxCode == "HomAggLg" ~ "BC",
                         Sown == "Unsown" & TxCode == "HetAggLg" ~ "C"))

clds <- clds |> 
  mutate(Scale.f = factor(Scale, levels = c("Sm","Lg")))
  

total <- ggplot(Sh2Mat20,                              # Create a graph from the Sh2Mat20 data frame
                aes(
                  x = Seeding,                         # SoilHet on the x-axis
                  y = richness,                        # Richness on the y-axis
                  group = SoilHet,                     # group, fill, and colour the graph by year
                  fill = SoilHet)) +
  stat_summary(geom = 'bar',                           # Add a bar plot with the mean richness 
               fun = mean,
               position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                          reverse = T))+
  stat_summary(geom = 'errorbar',                      # Add error bars for the mean richness
               fun.data = mean_cl_boot,                # The error bars are calculated using bootstrapping
               position = position_dodge(width = -0.9),
               width = 0.1,
               size = 0.3)+
  geom_text(data = clds |> 
              filter(Sown == "Total"),
            mapping = aes(x = Seeding, y = 42.5,
                          group = SoilHet),
            size = 2.5,
            position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                       reverse = T,
                                       width = 0.89),
            label = clds$CLD[clds$Sown == "Total"])+
  facet_grid(~ Scale.f,                                  # Create a facet grid with the Scale variable
             labeller = labeller(Scale.f = Scale.labs))+
  labs(x = "Seed Arrival",                             # labels the x-axis as Soil Heterogeneity
       y = "Species Richness") +                       # labels the legend "Sowing
  ggtitle("(a) Total Richness") +
  theme_classic() +                                    # sets the theme
  scale_x_discrete(name = "Seed Arrival",              # Changes the labels on the x-axis
                   limits = c("Uni", "Agg"),
                   labels = c("Uniform", "Aggregated"))+
  scale_fill_manual(values = c("tan",                  # Changes the colors of the bars in the bar plot
                               "saddlebrown",
                               "gray56"),
                    name = "Soil Heterogeneity",      # affects the legend
                    labels = c(                       # changes the labels on the legend
                      "Heterogeneous",
                      "Homogeneous",
                      "Unmanipulated")) +
  scale_y_continuous(limits = c(0, 45),                # makes the y-axis go from 0-45
                     breaks = c(0,5,10,15,20,25,30,35,40),
                     expand = c(0, 0)) +               # remove the floating 0 on the y-axis
  theme(                                               # affects the theme of the plot
    axis.text.x =                                      # changes the text on the x-axis
      element_text(                                    # changes the elements of the text
        angle = 0,                                     # sets the angle of the text to 0
        hjust = 0.5,                                   # adjusts the text 0.5 units horizontally
        vjust = 0.3)) +                                # vertically adjusts the text .3 unit
  theme(axis.title = element_text(size = 13)) +        # Change font size of axes titles
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(size = 8)) +        # Change font size of axes tick mark labels
  theme(plot.title = element_text(size = 13,           # Change font size of title
                                  hjust = 0.5),
        axis.title.x = element_text(color = 'white'),
        legend.position = "none") +                    # Change font size of axes tick mark labels
  theme(plot.subtitle = element_text(size = 10,        # Change font size of title
                                     hjust = 0.5))     # centers title"

total

sownR <- ggplot(Sown20,                                # Create a graph from the Sown20 data frame
                aes(
                  x = Seeding,                         # SoilHet on the x-axis
                  y = richness,                        # Richness on the y-axis
                  group = SoilHet,                     # group, fill, and colour the graph by year
                  fill = SoilHet)) +
  stat_summary(geom = 'bar',                           # Add a bar plot with the mean richness
               fun = mean,
               position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                          reverse = T))+
  stat_summary(geom = 'errorbar',                      # Add error bars for the mean richness
               fun.data = mean_cl_boot,                # The error bars are calculated using bootstrapping
               position = position_dodge(width = -0.9),
               width = 0.1,
               size = 0.3)+
  geom_text(data = clds |> 
              filter(Sown == "Sown"),
            mapping = aes(x = Seeding, y = 42.5,
                          group = SoilHet),
            size = 2.5,
            position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                       reverse = T,
                                       width = 0.89),
            label = clds$CLD[clds$Sown == "Sown"])+
  facet_grid(~ Scale.f,                                  # Create a facet grid with the Scale variable
             labeller = labeller(Scale.f = Scale.labs))+
  labs(x = "Seed Arrival",                             # labels the x-axis as Soil Heterogeneity
       y = "Average Richness") +                       # labels the legend "Sowing
  ggtitle("(b) Sown Species Richness") +
  theme_classic() +                                    # sets the theme
  scale_x_discrete(name = "Seed Arrival",              # Changes the labels on the x-axis
                   limits = c("Uni", "Agg"),
                   labels = c("Uniform", "Aggregated"))+
  scale_fill_manual(values = c("tan",
                               "saddlebrown",
                               "gray56"),
                    name = "Soil Heterogeneity",       # affects the legend
                    labels = c(                        # changes the labels on the legend
                      "Heterogeneous",
                      "Homogeneous",
                      "Unmanipulated")) +
  scale_y_continuous(limits = c(0, 45),                # makes the y-axis go from 0-45
                     breaks = c(0,5,10,15,20,25,30,35,40),
                     expand = c(0, 0)) +               # remove the floating 0 on the y-axis
  theme(                                               # affects the theme of the plot
    axis.text.x =                                      # changes the text on the x-axis
      element_text(                                    # changes the elements of the text
        angle = 0,                                     # sets the angle of the text to 0
        hjust = 0.5,                                   # adjusts the text 0.5 units horizontally
        vjust = 0.3)) +                                # vertically adjusts the text .3 unit
  theme(axis.title = element_text(size = 13)) +        # Change font size of axes titles
  theme(axis.text = element_text(size = 8)) +          # Change font size of axes tick mark labels
  theme(plot.title = element_text(size = 13,           # Change font size of title
                                  hjust = 0.5),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +                    # Change font size of axes tick mark labels
  theme(plot.subtitle = element_text(size = 10,        # Change font size of title
                                     hjust = 0.5))     # centers title
sownR

unsownR <- ggplot(Unsown20,                            # Create a graph from the Unsown20 data frame
                  aes(
                    x = Seeding,                       # SoilHet on the x-axis
                    y = richness,                      # Richness on the y-axis
                    group = SoilHet,                   # group, fill, and colour the graph by year
                    fill = SoilHet)) +
  stat_summary(geom = 'bar',                           # Add a bar plot with the mean richness
               fun = mean,
               position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                          reverse = T))+
  stat_summary(geom = 'errorbar',                      # Add error bars for the mean richness
               fun.data = mean_cl_normal,              # The error bars are calculated using bootstrapping
               position = position_dodge(width = -0.9),
               width = 0.1,
               size = 0.3)+
  geom_text(data = clds |> 
              filter(Sown == "Unsown"),
            mapping = aes(x = Seeding, y = 42.5,
                          group = SoilHet),
            size = 2.5,
            position = position_dodge2(padding = 0, # The bars are dodges to avoid overlap
                                       reverse = T,
                                       width = 0.89),
            label = clds$CLD[clds$Sown == "Unsown"])+
  facet_grid(~ Scale.f,                                  # Create a facet grid with the Scale variable
             labeller = labeller(Scale.f = Scale.labs))+
  labs(x = "Seed Arrival",                             # labels the x-axis as Soil Heterogeneity
       y = "Average Richness") +                       # labels the legend "Sowing
  ggtitle("(c) Non-sown Species Richness") +
  theme_classic() +                                    # sets the theme
  scale_x_discrete(name = "Seed Arrival",              # Changes the labels on the x-axis
                   limits = c("Uni", "Agg"),
                   labels = c("Uniform", "Aggregated"))+
  scale_fill_manual(values = c("tan",
                               "saddlebrown",
                               "gray56"),
                    name = "Soil Heterogeneity",       # affects the legend
                    labels = c(                        # changes the labels on the legend
                      "Heterogeneous",
                      "Homogeneous",
                      "Unmanipulated")) +
  scale_y_continuous(limits = c(0, 45),                # makes the y-axis go from 0-45
                     breaks = c(0,5,10,15,20,25,30,35,40),
                     expand = c(0, 0)) +               # remove the floating 0 on the y-axis
  theme(                                               # affects the theme of the plot
    axis.text.x =                                      # changes the text on the x-axis
      element_text(                                    # changes the elements of the text
        angle = 0,                                     # sets the angle of the text to 0
        hjust = 0.5,                                   # adjusts the text 0.5 units horizontally
        vjust = 0.3)) +                                # vertically adjusts the text .3 unit
  theme(axis.title = element_text(size = 13)) +        # Change font size of axes titles
  theme(axis.text = element_text(size = 8)) +          # Change font size of axes tick mark labels
  theme(plot.title = element_text(size = 13,           # Change font size of title
                                  hjust = 0.5),
        axis.title.x = element_text(color ="white"),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank()) +               # Change font size of axes tick mark labels
  theme(plot.subtitle = element_text(size = 10,        # Change font size of title
                                     hjust = 0.5))     # centers title



unsownR

# Create a plot grid with the three plots of richness
combinedR <- plot_grid(total, sownR, unsownR, ncol = 3, rel_widths = c(1.1,0.95,1.5)) # This code creates a plot grid with the three plots of richness, one for each combination of Seeding and SoilHet. The plots are arranged in three columns, with the total richness plot on the left, the sown richness plot in the middle, and the unsown richness plot on the right. The relative widths of the columns are set to 1.1, 0.95, and 1.5, respectively.

combinedR                                              # This code prints the plot to the console.

ggsave('Combined_Richness_SoilHet2_MS.png',           # This code saves the plot to a tiff file named `Combined_Richness_SoilHet2_MS.tiff`.
       plot = combinedR,
       width = 10,                                     # The width of the plot is set to 10 inches,
       height = 5.25,                                  # the height is set to 5.25 inches,
       dpi = 600)                                      # and the dpi is set to 600.
