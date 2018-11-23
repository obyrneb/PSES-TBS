library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)
library(forcats)
library(magrittr)

#loadPSES <- function() { 
  # Read all PSES 2017 Subsets. THese are already downloaded.
  ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
  ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
  ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
  ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
  ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
  # These are extra files where some addtional demographic and question information has been mapped
  mapQ <- read.csv("datasets//Question_Mappings.csv")
  mapDemQ <- read.csv("datasets//Demo_Question_Mappings.csv")
#}

# Combine subsets as appropriate. Uncomment up to the point we need to stop.
ss1_2 <- rbind(ss1,ss2)
ss1_3 <- rbind(ss1_2,ss3)
ss1_4 <- rbind(ss1_3,ss4)
ss1_5 <- rbind(ss1_4,ss5)

#Extract TBS data only for 2017. We only need ss5 as we will be looking at sector data.
ss.df <- subset(ss1_5, ((LEVEL1ID %in% c("26","0") & LEVEL2ID == "0") | (LEVEL1ID == "26")) & SURVEYR == 2017,
                select=c(LEVEL1ID,LEVEL2ID,LEVEL3ID,LEVEL4ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,
                         POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

# Create question subsections
ss.df$QSection <- substr(ss.df$QUESTION,0,1)
ss.df$QSection <- factor(ss.df$QSection)

# Create demographic question subsections 
# (to avoid errors, first need to unfactor BYCOND and then re-factor it)
ss.df$BYCOND <- as.character(ss.df$BYCOND)
ss.df$BYCOND[ss.df$DESCRIP_E == "Treasury Board of Canada Secretariat"] <- "TBS"
ss.df$BYCOND[ss.df$DESCRIP_E == "Public Service"] <- "PS" 
ss.df$DemoQ <- word(ss.df$BYCOND, 1, sep = " =")
ss.df$BYCOND <- as.factor(ss.df$BYCOND)

ss.df$DESCRIP_E <- as.character(ss.df$DESCRIP_E)
ss.df$DESCRIP_E[ss.df$BYCOND == "M_Q103A = 083"] <- "Group: PE"
ss.df$DESCRIP_E[ss.df$BYCOND == "M_Q104 = 12"] <- "Community: Human Resources"
ss.df$DESCRIP_E <- as.factor(ss.df$DESCRIP_E)

#Add question sections from mapping tables
ss.df <- merge(ss.df, mapQ, by = "QUESTION")

# Aggregate by demographic and question subsection
sectorMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ 
                            LEVEL1ID + LEVEL2ID + LEVEL3ID + LEVEL4ID + QSubTheme_E + QSubTheme_F + DemoQ + BYCOND +
                            DESCRIP_E + DESCRIP_F + SURVEYR, mean)

# Calculate proportions for each TBS demographic group and append to the descriptors (DesProp_E and DesProp_F).
# We will want the description fields with TBS-only proportions to be common to both PS and TBS data
# so we can graph them together. This is why we output to another dataframe (TBSprops.df), which we merge below.
TBSprops.df <- sectorMeans.df %>%
  filter(LEVEL1ID == "26") %>%
  dplyr::group_by(DemoQ, DESCRIP_E) %>%
  mutate(ngrp = sum(ANSCOUNT)) %>%
  group_by(DemoQ) %>%
  mutate(npop = sum(ANSCOUNT)) %>%
  ungroup() %>%
  mutate(prop = ngrp/npop) %>%
  mutate(DesProp_E = paste0(DESCRIP_E, " (", round((prop*100),0),"%)")) %>%
  mutate(DesProp_F = paste0(DESCRIP_F, " (", round((prop*100),0),"%)")) %>%
  distinct(DESCRIP_E, DesProp_E, DesProp_F) %>%
  rbind(c("Public Service","Public Service","Fonction publique"))

# DON'T NEED THIS HERE - WE'RE LOOKING AT TBS SECTORS ONLY
# Add demographic sections from mapping tables (the "mapDemQ" lookup table is based on TBS data
# and will therefore filter out all PS data not shared by TBS, e.g., non-TBS occupational groups)
sectorMeans.df <- merge(sectorMeans.df, mapDemQ, by = "BYCOND")

# As mentioned above, we merge  the TBSprops.df dataframe to the orginal dataframe to keep descriptors consistent
# between PS and tBS data. The preceding merge with "mapDemQ" has already stripped away non-shared descriptions.
sectorMeans.df <- merge(sectorMeans.df, TBSprops.df, by = "DESCRIP_E")

# Transform the positive-neutral-negative crosstab into a list 
sectorMeans.df <- melt(sectorMeans.df, measure.vars = c("NEGATIVE","NEUTRAL","POSITIVE"))

# Order SubThemes by least to most negative  
subThemeOrder <- sectorMeans.df %>%
  filter(BYCOND == "LEVEL3ID = 005" & variable == "NEGATIVE") %>%
  arrange(value)
sectorMeans.df <- sectorMeans.df %>%
  mutate(QSubTheme_E = factor(QSubTheme_E, unique(subThemeOrder$QSubTheme_E))) %>%
  mutate(QSubTheme_F = factor(QSubTheme_F, unique(subThemeOrder$QSubTheme_F)))


# Select demographic groups to plot: AS & CR groups, plus PS and TBS summary columns for comparison
sectorMeans.df <- subset(sectorMeans.df, 
                         BYCOND %in% c("M_Q103A = 083", "M_Q104 = 12","TBS","PS") | 
                         (LEVEL3ID == "5" & DemoQ %in% c("LEVEL3ID","LEVEL4ID")))

# Order occupational levels by overall group and then ascending level using the existing "OrderKey" column
# from the mapDemQ lookup table.
sectorMeans.df <- sectorMeans.df %>% 
  arrange(OrderOCHRO) %>%
  mutate(DesProp_E = factor(DesProp_E, unique(DesProp_E))) %>%
  mutate(DesProp_F = factor(DesProp_F, unique(DesProp_F)))

# Make PS and TBS overall the first levels (don't forget the percentages appended to the descriptors!)
sectorMeans.df$DesProp_E <- fct_relevel(sectorMeans.df$DesProp_E, 
                                        c("Public Service", "Treasury Board of Canada Secretariat (100%)"))
sectorMeans.df$DesProp_F <- fct_relevel(sectorMeans.df$DesProp_F, 
                                        c("Fonction publique", "Secrétariat du Conseil du Trésor du Canada (100%)"))

# LABELS
#---------------
#English captions and labels
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question theme and demographic category. The bars over each column represent the average value for this question theme for TBS and the Public Service. Question themes are sorted from least negative to most negative (the red column) for OCHRO as a whole. Cells highlighted in yellow indicate the negative proportion of responses is at least 10 points higher than the TBS average."
expl_E <- paste0(strwrap(expl_E, 100), sep="", collapse="\n")
ttl_E <- "PSES@TBS 2017 - OCHRO Sectors"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
file_E <- paste0(ttl_E,".pdf")
sectorMeans.df$variable_E <- mapvalues(sectorMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative" = "Negative", "Neutral" = "Neutral", "Positive" = "Positive", 
                "Public Service" ="PS", "Treasury Board of Canada Secretariat (100%)" ="TBS")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat (100%)" = "#d1e7ee", "Public Service" = "#fabcb3")

#French Captions and labels
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour un thème de question et une catégorie démographique particuliers. Les barres sur chaque colonne représentent la valeur moyenne pour ce thème de question pour le SCT et la Fonction publique. Les thèmes de la question sont triés du moins négatif au plus négatif (la colonne rouge) pour le tout du BDPRH.Les cellules surlignées en jaune indiquent que la proportion négative de réponses est supérieure d'au moins 10 points à la moyenne du SCT."
expl_F <- paste0(strwrap(expl_F, 100), sep="", collapse="\n")
ttl_F <- "SAFF@SCT 2017 - Secteurs du BDPRH" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
file_F <- paste0(ttl_F,".pdf")
sectorMeans.df$variable_F <- mapvalues(sectorMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif" = "Négatif", "Neutre" = "Neutre", "Positif" = "Positif",
                "Fonction publique" = "FP","Secrétariat du Conseil du Trésor du Canada (100%)" = "SCT")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada (100%)" = "#d1e7ee", "Fonction publique" = "#fabcb3")
#---------------



# Create separate TBS and PS dataframes
TBSmeans.df <- filter(sectorMeans.df, LEVEL1ID == "26" | BYCOND == "PS")
PSmeans.df <- filter(sectorMeans.df, LEVEL1ID == "0" | BYCOND == "TBS")

# Create TBS mean column to allow each small multiple to be compared to the TBS mean via geom_errorbar
TBSmeans.df <- TBSmeans.df %>%
  arrange(QSubTheme_E, variable, DESCRIP_E) %>%
  dplyr::group_by(QSubTheme_E, variable) %>%
  mutate(TBSmean = value[2]) %>%
  mutate(TBSmeanDiff = value - TBSmean)

# Compute PS overall means to compare to the PS, TBS and Sector data

PSoverall <- PSmeans.df %>%
  filter(BYCOND %in% c("PS","TBS")) %>%
  arrange(QSubTheme_E, variable) %>%
  dplyr::group_by(QSubTheme_E, variable) %>%
  mutate(value = value[1])

PSdetail <- filter(PSmeans.df, !(BYCOND %in% c("PS","TBS")))

PSmeans.df <- bind_rows(PSoverall, PSdetail)





# Define a plotting function for all subthemes. Inputs are language (E or F) and graph dimensions in inches.
plotOCHRO <- function(language, wdth = 10, hght = 8, textSize = 9) {
  
  if (language == "E") {
    TBSmeans.df$variable_lang <- TBSmeans.df$variable_E
    TBSmeans.df$DESCRIP_lang <- TBSmeans.df$DesProp_E
    TBSmeans.df$QSubTheme_lang <- TBSmeans.df$QSubTheme_E
    #TBSmeans.df$DemQ_lang <- TBSmeans.df$DemQ_E
    PSmeans.df$variable_lang <- PSmeans.df$variable_E
    PSmeans.df$DESCRIP_lang <- PSmeans.df$DesProp_E
    PSmeans.df$QSubTheme_lang <- PSmeans.df$QSubTheme_E
    #PSmeans.df$DemQ_lang <- PSmeans.df$DemQ_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    TBSmean_lang <- "TBS mean"
    PSmean_lang <- "PS mean"
    TBS_lang <- "Treasury Board of Canada Secretariat (100%)"
    PS_lang <- "Public Service"
  } else if (language == "F") {
    TBSmeans.df$variable_lang <- TBSmeans.df$variable_F
    TBSmeans.df$DESCRIP_lang <- TBSmeans.df$DesProp_F
    TBSmeans.df$QSubTheme_lang <- TBSmeans.df$QSubTheme_F
    #TBSmeans.df$DemQ_lang <- TBSmeans.df$DemQ_F
    PSmeans.df$variable_lang <- PSmeans.df$variable_F
    PSmeans.df$DESCRIP_lang <- PSmeans.df$DesProp_F
    PSmeans.df$QSubTheme_lang <- PSmeans.df$QSubTheme_F
    #PSmeans.df$DemQ_lang <- PSmeans.df$DemQ_F
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
  
  ggplot(data=TBSmeans.df, aes(x=variable_lang, y=value, fill = variable_lang)) +
    geom_rect(data = subset(TBSmeans.df,DESCRIP_lang == TBS_lang),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSmeans.df,DESCRIP_lang == PS_lang),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSmeans.df,DemoQ %in% c("LEVEL3ID","M_Q103A","M_Q104")),
              fill = "grey80",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSmeans.df,TBSmeanDiff >= 10 & variable == "NEGATIVE"),
              fill = "#e3f800",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.8) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    geom_errorbar(aes(ymax=TBSmean, ymin=TBSmean, linetype = TBSmean_lang), colour = "grey20") +
    geom_errorbar(data = PSmeans.df, 
                  aes(ymax=value, ymin=value, linetype = PSmean_lang), colour = "grey20") +
    scale_linetype_manual(values=c("dotted", "solid")) +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    facet_grid(QSubTheme_lang ~ DESCRIP_lang, switch = "y", #scales = "free_y",
               labeller = labeller(DESCRIP_lang = label_wrap_gen(8), QSubTheme_lang = label_wrap_gen(15)))  +
    theme(plot.title = element_text(size = 20, hjust = 0, colour = "grey40"),
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
          strip.text.x = element_text(angle = 0, size = textSize, colour = "grey40", inherit.blank = FALSE),
          strip.background = element_blank(),
          legend.text = element_text(size = textSize, colour = "grey45"),
          legend.title = element_blank(),
          legend.position = "bottom")
  
  ggsave(file_lang, height = hght, width = wdth)
  
  return()
  
}



plotOCHRO("E", 8, 13.5, 8)
plotOCHRO("F", 8, 13.5, 8)