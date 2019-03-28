library(scales)
library(readxl)
library(tidyverse)

# LOAD DATA
#------------
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

if(!file.exists(c(ss1File_2018,ss2File_2018,ss3File_2018,ss4File_2018,ss5File_2018))) {
  download.file(ss1URL_2018,ss1File_2018)
  download.file(ss2URL_2018,ss2File_2018)
  download.file(ss3URL_2018,ss3File_2018)
  download.file(ss4URL_2018,ss4File_2018)
  download.file(ss5URL_2018,ss5File_2018)
}

if(!exists("pses2018")) {
  ss1_2018 <- read.csv(ss1File_2018, na.strings = "9999")
  ss2_2018 <- read.csv(ss2File_2018, na.strings = "9999")
  ss3_2018 <- read.csv(ss3File_2018, na.strings = "9999")
  ss4_2018 <- read.csv(ss4File_2018, na.strings = "9999")
  ss5_2018 <- read.csv(ss5File_2018, na.strings = "9999")
  indicatorMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Indicator_Mapping.csv")) %>%
    select(-TITLE_E,-TITLE_F)
  pses2018 <- bind_rows(ss1_2018,ss2_2018,ss3_2018,ss4_2018,ss5_2018) %>%
    left_join(indicatorMap, by = "QUESTION")
}
#------------

# AGGREGATE DATA
#------------
TBS.df <- subset(pses2018, LEVEL1ID %in% c("26","0") & SURVEYR == 2018,
                 select=c(LEVEL1ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,
                          SUBINDICATORID, SUBINDICATORENG, SUBINDICATORFRA,
                          QUESTION,TITLE_E,TITLE_F,
                          POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

TBS.df$QSection <- substr(TBS.df$QUESTION,0,1)
TBS.df$QSection <- factor(TBS.df$QSection)

# Create demographic question subsections by extracting the leading code from BYCOND preceding "=" 
# Also replace NAs with "TBS" or "PS" depending on DESCRIP_E
TBS.df$BYCOND[TBS.df$DESCRIP_E == "Treasury Board of Canada Secretariat"] <- "TBS"
TBS.df$BYCOND[TBS.df$DESCRIP_E == "Public Service"] <- "PS" 
TBS.df$DemoQ <- word(TBS.df$BYCOND, 1, sep = " =")

# Replace all LEVELxIDs with a single DemoQ value - "org". This allows more intuitive proportion calculations.
TBS.df$DemoQ[startsWith(TBS.df$DemoQ,"LEVEL")] <- "org"

# Aggregate by demographic and question theme
TBSagg.df <- aggregate(data = TBS.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ 
                         LEVEL1ID + SUBINDICATORID + SUBINDICATORENG + SUBINDICATORFRA + DemoQ + 
                         #DemQ_E + DemQ_F + 
                         BYCOND + DESCRIP_E + DESCRIP_F + SURVEYR, mean)

# Calculate proportions for each TBS demographic group and append to the descriptors (DesProp_E and DesProp_F).
# We will want the description fields with TBS-only proportions to be common to both PS and TBS data
# so we can graph them together. This is why we output to another dataframe (TBSprops.df), which we merge below.
TBSprops.df <- TBSagg.df %>%
  filter(LEVEL1ID == "26") %>%
  group_by(DemoQ, DESCRIP_E) %>%
  mutate(ngrp = sum(ANSCOUNT)) %>%
  ungroup() %>%
  group_by(DemoQ) %>%
  mutate(npop = sum(ANSCOUNT)) %>%
  ungroup() %>%
  mutate(prop = ngrp/npop) %>%
  mutate(DesProp_E = paste0(DESCRIP_E, " (", round((prop*100),0),"%)")) %>%
  mutate(DesProp_F = paste0(DESCRIP_F, " (", round((prop*100),0),"%)")) %>%
  distinct(BYCOND, DesProp_E, DesProp_F) %>%
  # Here, we're just adding an empty line for the Public Service - percentages are only relevant for TBS.
  rbind(c("PS","Public Service","Fonction publique"))

# As mentioned above, we merge  the TBSprops.df dataframe to the orginal dataframe to keep descriptors consistent
# between PS and tBS data. The preceding merge with "mapDemQ" has already stripped away non-shared descriptions.
TBSagg.df <- merge(TBSagg.df, TBSprops.df, by = "BYCOND")

# Transform the positive-neutral-negative crosstab into a list 
TBSagg.df <- gather(TBSagg.df, key = "sentiment", value = "prop", NEGATIVE,NEUTRAL,POSITIVE)

# Order SubThemes by least to most negative  
subIndicatorOrder <- TBSagg.df %>%
  filter(BYCOND == "TBS" & sentiment == "NEGATIVE") %>%
  arrange(prop)
TBSagg.df <- TBSagg.df %>%
  mutate(SUBINDICATORENG = factor(SUBINDICATORENG, unique(subIndicatorOrder$SUBINDICATORENG))) %>%
  mutate(SUBINDICATORFRA = factor(SUBINDICATORFRA, unique(subIndicatorOrder$SUBINDICATORFRA)))

# CHOOSE DEMOGRAPHIC VARIABLES HERE!
# Select demographic groups to plot: AS & CR groups, plus PS and TBS summary columns for comparison
TBSagg.df <- subset(TBSagg.df, DemoQ %in% c("Q27","Q78A","Q87","Q88","Q89") | 
                      BYCOND %in% c("TBS","PS")) 

# Order occupational levels by overall group and then ascending level using the existing "OrderKey" column
# from the mapDemQ lookup table.
TBSagg.df <- TBSagg.df %>% 
  arrange(BYCOND) %>%
  mutate(DesProp_E = factor(DesProp_E, unique(DesProp_E))) %>%
  mutate(DesProp_F = factor(DesProp_F, unique(DesProp_F)))

# Make PS and TBS overall the first levels (don't forget the percentages appended to the descriptors!)
TBSagg.df$DesProp_E <- fct_relevel(TBSagg.df$DesProp_E, 
                                   c("Public Service","Treasury Board of Canada Secretariat (100%)"))
TBSagg.df$DesProp_F <- fct_relevel(TBSagg.df$DesProp_F, 
                                   c("Fonction publique","Secrétariat du Conseil du Trésor du Canada (100%)"))
#------------

# CREATE BILINGUAL LABELS
#------------
# English captions and labels
expl_E <- "Each cell of this chart displays the proportion of responses for a particular subindicator and demographic category. The bars over each column represent the average value for this subindicator for TBS. Subindicators are sorted from least negative to most negative at TBS."
expl_E <- str_wrap(expl_E, 100)
ttl_E <- "PSES@TBS 2018 - Occupational and Employment Equity Groups"
cap_E <- "2018 Public Service Employee Survey Open Datasets"
file_E <- paste0(ttl_E,".pdf")
#TBSagg.df$sentiment_E <- mapvalues(TBSagg.df$sentiment, 
#                                   c("NEGATIVE","NEUTRAL","POSITIVE"),
#                                   c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("NEGATIVE" = "Negative", 
                "NEUTRAL" = "Neutral", 
                "POSITIVE" = "Positive", 
                "Public Service" ="PS", 
                "Treasury Board of Canada Secretariat (100%)" ="TBS")
PNN_E.clrs <- c("NEGATIVE" = "#CD202C", 
                "NEUTRAL" = "#63CECA", 
                "POSITIVE" = "#CCDC00", 
                "Treasury Board of Canada Secretariat (100%)" = "#d1e7ee", 
                "Public Service" = "#fabcb3")

# French captions and labels
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour sous-indicateur et une catégorie démographique particuliers. Les barres sur chaque colonne représentent la valeur moyenne poure sous-indicateur pour le SCT. Les sous-indicateurs sont triés du moins négatif au plus négatif pour le SCT."
expl_F <- str_wrap(expl_F, 100)
ttl_F <- "SAFF@SCT 2018 - Groupes professionnels et d'équité en emploi" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2018"
file_F <- paste0(ttl_F,".pdf")
#TBSagg.df$sentiment_F <- mapvalues(TBSagg.df$sentiment, 
#                                   c("NEGATIVE","NEUTRAL","POSITIVE"),
#                                   c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("NEGATIVE" = "Négatif", 
                "NEUTRAL" = "Neutre", 
                "POSITIVE" = "Positif",
                "Fonction publique" = "FP",
                "Secrétariat du Conseil du Trésor du Canada (100%)" = "SCT")
PNN_F.clrs <- c("NEGATIVE" = "#CD202C", 
                "NEUTRAL" = "#63CECA", 
                "POSITIVE" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada (100%)" = "#d1e7ee", 
                "Fonction publique" = "#fabcb3")

#------------

# CREATE DATAFRAMES FOR PLOT
#------------
# Create separate TBS dataframe
TBSagg_noPS.df <- filter(TBSagg.df, LEVEL1ID == "26" | BYCOND == "PS")

# Create TBS mean column to allow each small multiple to be compared to the TBS mean via geom_errorbar
TBSagg_noPS.df <- TBSagg_noPS.df %>%
  group_by(SUBINDICATORENG, sentiment) %>%
  filter(BYCOND == "TBS") %>%
  select(SUBINDICATORENG, sentiment, TBSmean = prop) %>%
  right_join(TBSagg_noPS.df, by = c("SUBINDICATORENG", "sentiment")) %>%
  select(names(TBSagg_noPS.df), TBSmean) %>%
  mutate(TBSmeanDiff = prop - TBSmean) %>%
  #mutate(TBSmean = prop[which(BYCOND == "TBS")])%>%
  ungroup() %>%
  arrange(SUBINDICATORENG, sentiment, DESCRIP_E)


# Create separate PS dataframe
TBSagg_PS.df <- filter(TBSagg.df, LEVEL1ID == "0" | BYCOND == "TBS")

# Compute PS overall means to compare to the PS, TBS and Sector data
PSoverall <- filter(TBSagg_PS.df, BYCOND %in% c("PS","TBS"))

PSoverall <- PSoverall %>%
  group_by(SUBINDICATORENG, sentiment) %>%
  filter(BYCOND =="PS") %>%
  select(SUBINDICATORENG, sentiment, PSmean = prop) %>%
  right_join(PSoverall, by = c("SUBINDICATORENG", "sentiment")) %>%
  select(names(PSoverall), PSmean) %>%
  #mutate(PSmean = prop[which(BYCOND == "PS")])%>%
  ungroup() %>%
  arrange(SUBINDICATORENG, sentiment)

PSdetail <- filter(TBSagg_PS.df, !(BYCOND %in% c("PS","TBS")))

PSdetail <- mutate(PSdetail, PSmean = prop)

TBSagg_PS.df <- bind_rows(PSoverall, PSdetail)
#------------

# DEFINE PLOT FUNCTION
#------------
plotPSES <- function(language, wdth = 10, hght = 8, textSize = 9) {
  
  if (language == "E") {
    #TBSagg_noPS.df$sentiment_lang <- TBSagg_noPS.df$sentiment_E
    TBSagg_noPS.df$DESCRIP_lang <- TBSagg_noPS.df$DesProp_E
    TBSagg_noPS.df$SUBINDICATOR_lang <- TBSagg_noPS.df$SUBINDICATORENG
    #TBSagg_noPS.df$DemQ_lang <- TBSagg_noPS.df$DemQ_E
    #TBSagg_PS.df$sentiment_lang <- TBSagg_PS.df$sentiment_E
    TBSagg_PS.df$DESCRIP_lang <- TBSagg_PS.df$DesProp_E
    TBSagg_PS.df$SUBINDICATOR_lang <- TBSagg_PS.df$SUBINDICATORENG
    #TBSagg_PS.df$DemQ_lang <- TBSagg_PS.df$DemQ_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    TBSmean_lang <- "TBS mean across all levels"
    PSmean_lang <- "PS mean by level"
    TBS_lang <- "Treasury Board of Canada Secretariat (100%)"
    PS_lang <- "Public Service"
  } else if (language == "F") {
    #TBSagg_noPS.df$sentiment_lang <- TBSagg_noPS.df$sentiment_F
    TBSagg_noPS.df$DESCRIP_lang <- TBSagg_noPS.df$DesProp_F
    TBSagg_noPS.df$SUBINDICATOR_lang <- TBSagg_noPS.df$SUBINDICATORFRA
    #TBSagg_noPS.df$DemQ_lang <- TBSagg_noPS.df$DemQ_F
    #TBSagg_PS.df$sentiment_lang <- TBSagg_PS.df$sentiment_F
    TBSagg_PS.df$DESCRIP_lang <- TBSagg_PS.df$DesProp_F
    TBSagg_PS.df$SUBINDICATOR_lang <- TBSagg_PS.df$SUBINDICATORFRA
    #TBSagg_PS.df$DemQ_lang <- TBSagg_PS.df$DemQ_F
    PNN_lang.clrs <- PNN_F.clrs
    PNN_lang.lbls <- PNN_F.lbls
    expl_lang <- expl_F
    ttl_lang <- ttl_F
    cap_lang <- cap_F
    file_lang <- file_F
    TBSmean_lang <- "moyenne SCT pour tous les niveaux"
    PSmean_lang <- "moyenne FP par niveau"
    TBS_lang <- "Secrétariat du Conseil du Trésor du Canada (100%)"
    PS_lang <- "Fonction publique"
  } else {
    return("Invalid language selection. Choose E (English) or F (French).")
  }
  
  ggplot(data=TBSagg_noPS.df, aes(x=sentiment, y=prop, fill=sentiment)) +
    #geom_rect(data = subset(TBSagg_noPS.df,DESCRIP_lang == TBS_lang),
    #          fill = "grey80",xmin = -Inf,xmax = Inf,
    #          ymin = -Inf,ymax = Inf) +
    #geom_rect(data = subset(TBSagg_noPS.df,DESCRIP_lang == PS_lang),
    #          fill = "grey80",xmin = -Inf,xmax = Inf,
    #          ymin = -Inf,ymax = Inf) +
    #geom_rect(data = subset(TBSagg_noPS.df,DemoQ %in% c("M_Q103A")),
    #          fill = "grey80",xmin = -Inf,xmax = Inf,
    #          ymin = -Inf,ymax = Inf) +
    geom_rect(data = subset(TBSagg_noPS.df,TBSmeanDiff >= 8),
              aes(fill = sentiment),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.5) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    geom_errorbar(aes(ymax=TBSmean, ymin=TBSmean, linetype = TBSmean_lang), colour = "grey20") +
    geom_errorbar(data = TBSagg_PS.df, 
                  aes(ymax=PSmean, ymin=PSmean, linetype = PSmean_lang), colour = "grey20") +
    scale_linetype_manual(values=c("dotted", "solid")) +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    facet_grid(SUBINDICATOR_lang ~ DESCRIP_lang, switch = "y", #scales = "free_y",
               labeller = labeller(DESCRIP_lang = label_wrap_gen(20), SUBINDICATOR_lang = label_wrap_gen(15)))  +
    theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
          plot.subtitle = element_text(face = "bold", size = textSize, colour = "grey40"),
          plot.caption = element_text(face = "italic", size = textSize, colour = "grey45"),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "grey95"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0, "mm"),
          panel.spacing.x = unit(0.5, "mm"),
          strip.text.y = element_text(angle = 180, size = textSize, colour = "grey40", inherit.blank = FALSE),
          strip.text.x = element_text(angle = 90, size = textSize, colour = "grey40", inherit.blank = FALSE),
          strip.background = element_blank(),
          legend.text = element_text(size = textSize, colour = "grey45"),
          legend.title = element_blank(),
          legend.position = "bottom")
  
  ggsave(file_lang, height = hght, width = wdth)
  
  return()
}
#------------
 
# CREATE BILINGUAL PLOTS
#------------
# This chunk simply calls the above function to create two plots, one in French and one in English.
plotPSES("E", 8, 13.5, 8)
plotPSES("F", 8, 13.5, 8)