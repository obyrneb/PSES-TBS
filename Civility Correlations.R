library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)
library(forcats)
library(GGally)
library(tidyverse)

loadPSES <- function() { 
  # Read all PSES 2017 Subsets. THese are already downloaded.
  ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
  ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
  ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
  ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
  ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
  # These are extra files where some addtional demographic and question information has been mapped
  mapQ <- read.csv("datasets//Question_Mappings.csv")
  mapDemQ <- read.csv("datasets//Demo_Question_Mappings.csv")
}

# Combine subsets as appropriate. Uncomment up to the point we need to stop.
ss1_2 <- rbind(ss1,ss2)
#ss1_3 <- rbind(ss1_2,ss3)
#ss1_4 <- rbind(ss1_5,ss4)
#ss1_5 <- rbind(ss1_4,ss5)

#Extract TBS data only for 2017
civ.df <- subset(ss5, (LEVEL1ID == "26" & LEVEL2ID != "0") & SURVEYR == 2017,
                select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,
                         SCORE100))

# Create question subsections
civ.df$QSection <- substr(civ.df$QUESTION,0,1)
civ.df$QSection <- factor(civ.df$QSection)

# Create demographic question subsections
civ.df$BYCOND[civ.df$DESCRIP_E == "Treasury Board of Canada Secretariat"] <- "TBS"
civ.df$BYCOND[civ.df$DESCRIP_E == "Public Service"] <- "PS" 
#civ.df$DemoQ <- word(civ.df$BYCOND, 1, sep = " =")

# Add question sections from mapping tables
civ.df <- merge(civ.df, mapQ, by = "QUESTION")

civ.df <- subset(civ.df, QSubTheme_E %in% c("Respectful workplace", "Harassment", "Organizational performance") & !is.na(SCORE100),
                 select = c(DESCRIP_E,QUESTION,SCORE100))

civ.df <- spread(civ.df, DESCRIP_E, QUESTION)

ggpairs(civ.df, cardinality_threshold = 23)