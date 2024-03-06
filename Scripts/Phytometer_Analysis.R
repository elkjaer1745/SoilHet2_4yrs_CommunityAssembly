# Phytometer Analysis ----------------------------------------------------------------------------------------
# This README provides an overview of the R code used for the phytometer  analysis in the Article: "Spatial pattern of 
# seed arrival  has a greater effect on plant diversity than does soil heterogeneity in a grassland ecosystem" by 
# Kjaer et al. (2024) The code is organized into sections for better understanding and readability.

# NOTE: Due to issues with the "Matrix" package, this code does not work in R v4.3.2 (specifically the code that
# calculates linear and generalized linear models). It does work in v4.1.2 when the code was previously written.

# Packages Used
# - `emmeans`: For estimated marginal means
# - `car`: For regression diagnostics
# - `MASS`: For fitting GLMs
# - `lme4`: For fitting linear mixed-effects models
# - `lmerTest`: For obtaining p-values for mixed models
# - `Rmisc`: For summary statistics
# - `tidyverse`: For data manipulation and visualization
# - `readxl`: For reading Excel files
# - `multcomp`: For multiple comparisons
# 
# ## Data Import
# The script reads an Excel file containing phytometer data.
# 
# ## Data Wrangling
# It selects the relevant columns for analysis, calculates total biomass, removes missing values, and filters the data
# for specific plant species. Soil type and shoot biomass are converted to appropriate data types.
# 
# ## Total Response
# The script fits GLMs for total biomass vs. soil type for each plant species. It includes summary statistics, 
# estimated marginal means, ANOVA, and visualizations for each species.
# 
# ## Shoot Response
# Similar to total response, the script analyzes shoot biomass specifically for each plant species.
# 
# ## Root Response
# Similarly, the script analyzes root biomass specifically for each plant species.
#
# Please note that this README provides a high-level overview of the analysis performed using the provided R code.
# Additional details, such as specific results and interpretations, would typically be included in the manuscript
# or supplementary materials.
#
# Using This Code
# Before running the script, ensure that the required R packages are installed (and that the Matrix package is no
# longer causing issues in R v4.3.2). The script uses the publiclly available data found in the phytometer Excel
# file. Please note that the data used in this script and in this GitHub directory only contain data
# from the first round of greenhouse experiments for this project. Additional data may be availble on the SoilHet2
# figshare directory. (https://figshare.com/projects/SoilHet2/117219)
# For questions or issues, please contact Esben Kjaer.
# 
# The code, comments, and README were updated on 6Mar24. 
#
# For any questions or issues related to this script, please refer to the script's author/maintainer.
# Author and Maintainer
# Author: Esben Kjaer
# Date: 6Mar24
# Email: elkjaer1745@gmail.com; esben.kjaer@ndsu.edu

# Load Packages ----------------------------------------------------------------------------------------------

library(emmeans)                                       # Load the emmeans package for estimated marginal means
library(car)                                           # Load the car package for regression diagnostics
library(MASS)                                          # Load the MASS package for generalized linear models
library(lme4)                                          # Load the lme4 package for fitting linear mixed-effects models
library(lmerTest)                                      # Load the lmerTest package for p-values for mixed models
library(Rmisc)                                         # Load the Rmisc package for summarySE function
library(tidyverse)                                     # Load the tidyverse package for data manipulation and visualization
library(readxl)                                        # Load the readxl package for reading Excel files
library(multcomp)                                      # Load the multcomp package for multiple comparisons

# Data Import ------------------------------------------------------------------------------------------------

psf <- read_excel("Data/SoilHet2_Phytometer.xlsx",     # Read Excel file
                  sheet = 1,                           # Specify the sheet number
                  col_names = T)                       # Include column names
#CondSpp - Conditioning species -- single species sown into monoculture in the field portion of the phytometer assay
#CondFG - Conditioning species functional group -- functional group of the species in the CondSpp column, could be:
# forb, legume, or grass
#Rep - replication number
#RespSpp - Response species -- species sown into a pot in the greenhouse
#Cult_Pot - cultivation pot -- unique identifier for a specific pot in the greenhouse
#SoilType - the soil type or strata that were in each pot. 1 = Upper strata, 2 = Middle strata, 3 = Lower Strata, 4 = Mixed soil
#Shoot - shoot biomass in grams
#Root - root biomass in grams

# Data Wrangling ---------------------------------------------------------------------------------------------

psf <- psf %>%                                         # Select only columns needed for analysis
  ungroup() %>%                                        # Ungroup columns
  select(CondSpp:Root)                                 # Select columns from CondSpp to Root

psf$total <- as.numeric(psf$Shoot)+psf$Root            # Calculate total biomass

psf <- psf %>% remove_missing() %>%                    # Remove missing values
  filter(RespSpp %in% c("DESCAN","SCHSCO",
                        "HELMAX","RATPIN","SORNUT"))   # Filter to only species of interest

psf$SoilType <- psf$SoilType %>%                       # Convert SoilType to factor
  as.factor()

psf$Shoot <- psf$Shoot %>%                             # Convert Shoot to numeric
  as.numeric()


# Total Response ---------------------------------------------------------------------------------------------

# DESCAN -----------------------------------------------------------------------------------------------------

psfD <- psf %>%                                        # Filter data for Desmodium canadense (DESCAN)
  filter(RespSpp == "DESCAN")

desg <- glm(total~SoilType,                            # Fit GLM for total biomass vs. SoilType
            data = psfD,
            family = Gamma(link = "inverse"))          # use Gamma family with inverse link
summary(desg)                                          # Summary of the model

emmeans(desg, pairwise~SoilType)                       # Estimated marginal means

Anova(desg,                                            # ANOVA for the model
      type = 3)

psfDa <- summarySE(psfD,                               # Summary statistics for Desmodium canadense
                   measurevar = 'total',
                   groupvars = "SoilType")

ggplot(psfDa,                                         # Plot for Desmodium canadense
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Desmodium canadense") +                     # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# HELMAX -----------------------------------------------------------------------------------------------------

psfH <- psf %>%                                        # Filter data for Helianthus maximiliani (HELMAX)
  filter(RespSpp == "HELMAX")

helg <- glm(total~SoilType,                            # Fit GLM for total biomass vs. SoilType
            data = psfH,
            family = Gamma(link = "inverse"))          # use Gamma family with inverse link
summary(helg)                                          # Summary of the model

emmeans(helg, pairwise~SoilType)                       # Estimated marginal means

Anova(helg,                                            # ANOVA for the model
      type = 3)

psfHa <- summarySE(psfH,                               # Summary statistics for Helianthus maximiliani
                   measurevar = 'total',
                   groupvars = "SoilType")

ggplot(psfHa,                                          # Plot for Helianthus maximiliani
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Helianthus maximiliani") +                  # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# RATPIN -----------------------------------------------------------------------------------------------------

psfR <- psf %>%                                        # Filter data for Ratibida pinnata (RATPIN)
  filter(RespSpp == "RATPIN")

ratg <- glm(total~SoilType,                            # Fit GLM for total biomass vs. SoilType
            data = psfR,
            family = Gamma(link = "inverse"))          # use Gamma family with inverse link
summary(ratg)                                          # Summary of the model

emmeans(ratg, pairwise~SoilType)                       # Estimated marginal means

Anova(ratg,                                            # ANOVA for the model
      type = 3)

psfRa <- summarySE(psfR,                               # Summary statistics for Ratibida pinnata
                   measurevar = 'total',
                   groupvars = "SoilType")

ggplot(psfRa,                                          # Plot for Ratibida pinnata
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Ratibida pinnata") +                        # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# SCHSCO -----------------------------------------------------------------------------------------------------

psfS <- psf %>%                                        # Filter data for Schizachyrium scoparium (SCHSCO)
  filter(RespSpp == "SCHSCO")

schg <- glm(total~SoilType,                            # Fit GLM for total biomass vs. SoilType
            data = psfS,
            family = Gamma(link = "inverse"))          # use Gamma family with inverse link
summary(schg)                                          # Summary of the model

emmeans(schg, pairwise~SoilType)                       # Estimated marginal means

Anova(schg,                                            # ANOVA for the model
      type = 3)

psfSa <- summarySE(psfS,                               # Summary statistics for Schizachyrium scoparium
                   measurevar = 'total',
                   groupvars = "SoilType")

ggplot(psfSa,                                          # Plot for Schizachyrium scoparium
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Schizachyrium scoparium") +                 # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# SORNUT -----------------------------------------------------------------------------------------------------

psfSo <- psf %>%                                       # Filter data for Sorghastrum nutans (SORNUT)
  filter(RespSpp == "SORNUT")

sorG <- glm(total~SoilType,                             # Fit GLM for total biomass vs. SoilType
            data = psfSo,
            family = Gamma(link = "inverse"))          # use Gamma family with inverse link
summary(sorG)                                          # Summary of the model

emmeans(sorG, pairwise~SoilType)                       # Estimated marginal means

Anova(sorG,                                            # ANOVA for the model
      type = 3)

psfSoa <- summarySE(psfSo,                             # Summary statistics for Sorghastrum nutans
                    measurevar = 'total',
                    groupvars = "SoilType")

ggplot(psfSoa,                                         # Plot for Sorghastrum nutans
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Sorghastrum nutans") +                      # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# Total ------------------------------------------------------------------------------------------------------

totalg <- glm(total~SoilType,                          # Fit GLM for total biomass vs. SoilType
              data = psf,
              family = Gamma(link = "identity"))       # use Gamma family with inverse link
summary(totalg)                                        # Summary of the model

emmeans(totalg, pairwise~SoilType)                     # Estimated marginal means

Anova(totalg,                                          # ANOVA for the model
      type = 3)

psfa <- summarySE(psf,                                 # Summary statistics for total biomass
                  measurevar = 'total',
                  groupvars = "SoilType")

ggplot(psfa,                                           # Plot for total biomass
       aes(x = as.factor(SoilType),
           y = total))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = total - se,
                    ymax = total + se),
                width = .1)+
  theme_classic()+
  labs(x = "Soil Strata",                              # X-axis label
       y = "Plant Biomass (g)") +                      # Y-axis label
  ggtitle("Total Plant Biomass") +                     # Plot title
  theme_classic() +                                    # Classic theme
  scale_x_discrete(name = "Soil Strata",               # X-axis scale
                   labels = c("1" = "Upper",
                              "2" = "Middle",
                              "3" = "Lower",
                              "4" = "Mixed"))+
  scale_y_continuous(expand = c(0, 0)) +               # Y-axis limits
  theme(                                               # Theme adjustments
    axis.text.x =                                      # X-axis text adjustments
      element_text(
        angle = 0,                                     # Text angle
        hjust = 0.5,                                   # Horizontal adjustment
        vjust = 0.3)) +                                # Vertical adjustment
  theme(axis.title = element_text(size = 13)) +        # Axis title font size
  theme(axis.text = element_text(size = 10)) +         # Axis text font size
  theme(plot.title = element_text(size = 13,           # Plot title font size
                                  hjust = 0.5)) +      # Plot title horizontal adjustment
  theme(plot.subtitle = element_text(size = 10,        # Plot subtitle font size
                                     hjust = 0.5))     # Plot subtitle horizontal adjustment

# Shoot Response ---------------------------------------------------------------------------------------------

# DESCAN -----------------------------------------------------------------------------------------------------

psfD <- psf %>%                                        # Filter data for Desmodium canadense
  filter(RespSpp == "DESCAN")

desg <- glm(Shoot~SoilType,                            # Fit GLM for shoot biomass vs. SoilType
            data = psfD,
            family = Gamma(link = "inverse"))
summary(desg)                                          # Summary of the model

summary(glht(desg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# HELMAX -----------------------------------------------------------------------------------------------------

psfH <- psf %>%                                        # Filter data for Helianthus maximiliani
  filter(RespSpp == "HELMAX")

helg <- glm(Shoot~SoilType,                            # Fit GLM for shoot biomass vs. SoilType
            data = psfH,
            family = Gamma(link = "inverse"))
summary(helg)                                          # Summary of the model

summary(glht(helg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# RATPIN -----------------------------------------------------------------------------------------------------

psfR <- psf %>%                                        # Filter data for Ratibida pinnata
  filter(RespSpp == "RATPIN")

ratg <- glm(Shoot~SoilType,                            # Fit GLM for shoot biomass vs. SoilType
            data = psfR,
            family = Gamma(link = "inverse"))
summary(ratg)                                          # Summary of the model

summary(glht(ratg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# SCHSCO -----------------------------------------------------------------------------------------------------

psfS <- psf %>%                                        # Filter data for Schizachyrium scoparium
  filter(RespSpp == "SCHSCO")

schg <- glm(Shoot~SoilType,                            # Fit GLM for shoot biomass vs. SoilType
            data = psfS,
            family = Gamma(link = "inverse"))
summary(schg)                                          # Summary of the model

summary(glht(schg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# SORNUT -----------------------------------------------------------------------------------------------------

psfSo <- psf %>%                                       # Filter data for Sorghastrum nutans
  filter(RespSpp == "SORNUT")

sorG <- glm(Shoot~SoilType,                            # Fit GLM for shoot biomass vs. SoilType
            data = psfSo,
            family = Gamma(link = "inverse"))
summary(sorG)                                          # Summary of the model

summary(glht(sorG, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# Total Shoots -----------------------------------------------------------------------------------------------

Shootg <- glm(Shoot~SoilType,                          # Fit GLM for shoot biomass vs. SoilType
              data = psf,
              family = Gamma(link = "identity"))
summary(Shootg)                                        # Summary of the model

Anova(Shootg,                                          # ANOVA for the model
      type = 3)

summary(glht(Shootg, mcp(SoilType = "Tukey")))         # Post hoc comparisons

# Root Response ----------------------------------------------------------------------------------------------

# DESCAN -----------------------------------------------------------------------------------------------------

psfD <- psf %>%                                        # Filter data for Desmodium canadense
  filter(RespSpp == "DESCAN")

desg <- glm(Root~SoilType,                             # Fit GLM for root biomass vs. SoilType
            data = psfD,
            family = Gamma(link = "inverse"))
summary(desg)                                          # Summary of the model

summary(glht(desg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# HELMAX -----------------------------------------------------------------------------------------------------

psfH <- psf %>%                                        # Filter data for Helianthus maximiliani
  filter(RespSpp == "HELMAX")

helg <- glm(Root~SoilType,                             # Fit GLM for root biomass vs. SoilType
            data = psfH,
            family = Gamma(link = "inverse"))
summary(helg)                                          # Summary of the model

summary(glht(helg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# RATPIN -----------------------------------------------------------------------------------------------------

psfR <- psf %>%                                        # Filter data for Ratibida pinnata
  filter(RespSpp == "RATPIN")

ratg <- glm(Root~SoilType,                             # Fit GLM for root biomass vs. SoilType
            data = psfR,
            family = Gamma(link = "inverse"))
summary(ratg)                                          # Summary of the model

summary(glht(ratg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# SCHSCO -----------------------------------------------------------------------------------------------------

psfS <- psf %>%                                        # Filter data for Schizachyrium scoparium
  filter(RespSpp == "SCHSCO")

schg <- glm(Root~SoilType,                             # Fit GLM for root biomass vs. SoilType
            data = psfS,
            family = Gamma(link = "inverse"))
summary(schg)                                          # Summary of the model

summary(glht(schg, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# SORNUT -----------------------------------------------------------------------------------------------------

psfSo <- psf %>%                                       # Filter data for Sorghastrum nutans
  filter(RespSpp == "SORNUT")

sorG <- glm(Root~SoilType,                             # Fit GLM for root biomass vs. SoilType
            data = psfSo,
            family = Gamma(link = "inverse"))
summary(sorG)                                          # Summary of the model

summary(glht(sorG, mcp(SoilType = "Tukey")))           # Post hoc comparisons

# Total Root Response ----------------------------------------------------------------------------------------

Rootg <- glm(Root~SoilType,                            # Fit GLM for root biomass vs. SoilType
             data = psf,
             family = Gamma(link = "identity"))
summary(Rootg)                                         # Summary of the model

Anova(Rootg,                                           # ANOVA for the model
      type = 3)

summary(glht(Rootg, mcp(SoilType = "Tukey")))          # Post hoc comparisons
