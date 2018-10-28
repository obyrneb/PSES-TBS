# Download and initialize PSES 2017 datasets from:
# https://open.canada.ca/data/dataset/9322c841-1498-4f18-91c5-3e09235f63ac

getPSES2017 <- function() {
  # URLs for 2017 PSES dataset 
  URLall <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Open_dataset_Ensemble_donnees_ouvertes.csv"
  URLdoc <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_Supporting_Documentation_Document_reference_SAFF_2017.xlsx"
  URLss1 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv"
  URLss2 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv"
  URLss3 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv"
  URLss4 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv"
  URLss5 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv"
  # Download each 2017 PSES subset file
  download.file(URLss1, "datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv")
  download.file(URLss2, "datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv")
  download.file(URLss3, "datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv")
  download.file(URLss4, "datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv")
  download.file(URLss5, "datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv")
}

loadPSES2017 <- function() { 
  #Read all PSES 2017 Subsets
  ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
  ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
  ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
  ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
  ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
  ss1_5 <- rbind(ss1,ss2)
  ss1_5 <- rbind(ss1_5,ss3)
  ss1_5 <- rbind(ss1_5,ss4)
  ss1_5 <- rbind(ss1_5,ss5)
  pses2017.df <- ss1_5
}
