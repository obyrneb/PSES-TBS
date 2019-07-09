# This script downloads and loads PSES 2018 data for analysis

library(tidyverse)

# Set up working directory 
mainDir <- getwd()
dataDir <- "datasets"
plotDir <- "plots"
ss1File_2018 <- file.path(mainDir,dataDir,"pses2018_ss1.csv")
ss2File_2018 <- file.path(mainDir,dataDir,"pses2018_ss2.csv")
ss3File_2018 <- file.path(mainDir,dataDir,"pses2018_ss3.csv")
ss4File_2018 <- file.path(mainDir,dataDir,"pses2018_ss4.csv")
ss5File_2018 <- file.path(mainDir,dataDir,"pses2018_ss5.csv")
ss1URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-1_Sous-ensemble-1.csv"
ss2URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-2_Sous-ensemble-2.csv"
ss3URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-3_Sous-ensemble-3.csv"
ss4URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-4_Sous-ensemble-4.csv"
ss5URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-5_Sous-ensemble-5.csv" # Due for release in March 2018

ifelse(!dir.exists(file.path(mainDir, dataDir)), dir.create(file.path(mainDir, dataDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, plotDir)), dir.create(file.path(mainDir, plotDir)), FALSE)

# Download files, unless already downloaded
if (!file.exists(c(ss1File_2018,ss2File_2018,ss3File_2018,ss4File_2018,ss5File_2018))) {
  download.file(ss1URL_2018,ss1File_2018)
  download.file(ss2URL_2018,ss2File_2018)
  download.file(ss3URL_2018,ss3File_2018)
  download.file(ss4URL_2018,ss4File_2018)
  download.file(ss5URL_2018,ss5File_2018)
}

# Load files, unless already loaded
if (!exists("pses2018")) {
  ss1_2018 <- read.csv(ss1File_2018, na.strings = "9999")
  ss2_2018 <- read.csv(ss2File_2018, na.strings = "9999")
  ss3_2018 <- read.csv(ss3File_2018, na.strings = "9999")
  ss4_2018 <- read.csv(ss4File_2018, na.strings = "9999")
  ss5_2018 <- read.csv(ss5File_2018, na.strings = "9999")
}


# Load addtional metadata files  
indicatorMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Indicator_Mapping.csv")) %>%
  select(-TITLE_E,-TITLE_F)

demoMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Demographic_Mapping.csv"))

sectorAbbr <- read.csv(file.path(mainDir,dataDir,"PSES2018_TBS_Sector_Abbreviations.csv")) 

# Build the PSES 2018 dataframe and add metadata via joins
pses2018 <- bind_rows(ss1_2018,ss2_2018,ss3_2018,ss4_2018,ss5_2018) %>%
  mutate(DemoQ = ifelse(is.na(BYCOND),"dept",
                        ifelse(startsWith(BYCOND, "LEVEL"),"org",
                               word(BYCOND, 1, sep = " =")))) %>% 
  left_join(indicatorMap, by = "QUESTION") %>% 
  left_join(sectorAbbr, by = "DESCRIP_E") %>%
  left_join(demoMap, by = "DemoQ")

# Load demo
