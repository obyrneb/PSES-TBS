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
ss1_3 <- rbind(ss1_2,ss3)
ss1_4 <- rbind(ss1_3,ss4)
ss1_5 <- rbind(ss1_4,ss5)

#Extract TBS data only for 2017
ss.df <- subset(ss1_5, LEVEL1ID %in% c("26","0") & SURVEYR == 2017,
                select=c(LEVEL1ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,
                         POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

# Create question subsections
ss.df$QSection <- substr(ss.df$QUESTION,0,1)
ss.df$QSection <- factor(ss.df$QSection)

# Create demographic question subsections
ss.df$BYCOND[ss.df$DESCRIP_E == "Treasury Board of Canada Secretariat"] <- "TBS"
ss.df$BYCOND[ss.df$DESCRIP_E == "Public Service"] <- "PS" 
ss.df$DemoQ <- word(ss.df$BYCOND, 1, sep = " =")

# Add question sections from mapping tables
ss.df <- merge(ss.df, mapQ, by = "QUESTION")

# Aggregate by demographic and question subsection
demoMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ 
                            LEVEL1ID + QSubTheme_E + QSubTheme_F + DemoQ + BYCOND +
                            DESCRIP_E + DESCRIP_F + SURVEYR, mean)

# Calculate proportions for each TBS demographic group and append to the descriptors (DesProp_E and DesProp_F).
# We will want the description fields with TBS-only proportions to be common to both PS and TBS data
# so we can graph them together. This is why we output to another dataframe (TBSprops.df), which we merge below.
TBSprops.df <- demoMeans.df %>%
  filter(LEVEL1ID == "26") %>%
  group_by(DemoQ, DESCRIP_E) %>%
  mutate(ngrp = sum(ANSCOUNT)) %>%
  group_by(DemoQ) %>%
  mutate(npop = sum(ANSCOUNT)) %>%
  ungroup() %>%
  mutate(prop = ngrp/npop) %>%
  mutate(DesProp_E = paste0(DESCRIP_E, " (", round((prop*100),0),"%)")) %>%
  mutate(DesProp_F = paste0(DESCRIP_F, " (", round((prop*100),0),"%)")) %>%
  distinct(DESCRIP_E, DesProp_E, DesProp_F) %>%
  rbind(c("Public Service","Public Service","Fonction publique"))

# Add demographic sections from mapping tables (the "mapDemQ" lookup table is based on TBS data
# and will therefore filter out all PS data not shared by TBS, e.g., non-TBS occupational groups)
demoMeans.df <- merge(demoMeans.df, mapDemQ, by = "BYCOND")

# As mentioned above, we merge  the TBSprops.df dataframe to the orginal dataframe to keep descriptors consistent
# between PS and tBS data. The preceding merge with "mapDemQ" has already stripped away non-shared descriptions.
demoMeans.df <- merge(demoMeans.df, TBSprops.df, by = "DESCRIP_E")

# Transform the positive-neutral-negative crosstab into a list 
demoMeans.df <- melt(demoMeans.df, measure.vars = c("NEGATIVE","NEUTRAL","POSITIVE"))

# Order SubThemes by least to most negative  
subThemeOrder <- demoMeans.df %>%
  filter(BYCOND == "M_Q103A = 011" & variable == "NEGATIVE") %>%
  arrange(value)
demoMeans.df <- demoMeans.df %>%
  mutate(QSubTheme_E = factor(QSubTheme_E, unique(subThemeOrder$QSubTheme_E))) %>%
  mutate(QSubTheme_F = factor(QSubTheme_F, unique(subThemeOrder$QSubTheme_F)))

# Select demographic groups to plot: AS & CR groups, plus PS and TBS summary columns for comparison
demoMeans.df <- subset(demoMeans.df, BYCOND %in% c("M_Q103A = 011","M_Q103A = 023","TBS","PS") | 
                         DemQ_E %in% c("AS","CR")) 

# Order occupational levels by overall group and then ascending level using the existing "OrderKey" column
# from the mapDemQ lookup table.
demoMeans.df <- demoMeans.df %>% 
  arrange(OrderKey) %>%
  mutate(DesProp_E = factor(DesProp_E, unique(DesProp_E))) %>%
  mutate(DesProp_F = factor(DesProp_F, unique(DesProp_F)))

# Make PS and TBS overall the first levels (don't forget the percentages appended to the descriptors!)
demoMeans.df$DesProp_E <- fct_relevel(demoMeans.df$DesProp_E, c("Public Service",
                                                                "Treasury Board of Canada Secretariat (100%)"))
demoMeans.df$DesProp_F <- fct_relevel(demoMeans.df$DesProp_F, c("Fonction publique",
                                                                "Secrétariat du Conseil du Trésor du Canada (100%)"))

# LABELS
#---------------
#English captions and labels
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question theme and demographic category. The bars over each column represent the average value for this question theme for TBS. Question themes are sorted from least negative to most negative (the red column) for the AS group at TBS."
expl_E <- paste0(strwrap(expl_E, 100), sep="", collapse="\n")
ttl_E <- "PSES@TBS 2017 - AS & CR Groups"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
file_E <- paste0(ttl_E,".pdf")
demoMeans.df$variable_E <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative" = "Negative", "Neutral" = "Neutral", "Positive" = "Positive", 
                "Public Service" ="PS", "Treasury Board of Canada Secretariat (100%)" ="TBS")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat (100%)" = "#d1e7ee", "Public Service" = "#fabcb3")

#French Captions and labels
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour un thème de question et une catégorie démographique particuliers. Les barres sur chaque colonne représentent la valeur moyenne pour ce thème de question pour le SCT. Les thèmes de la question sont triés du moins négatif au plus négatif (la colonne rouge) pour le groupe AS du SCT."
expl_F <- paste0(strwrap(expl_F, 100), sep="", collapse="\n")
ttl_F <- "SAFF@SCT 2017 - Groupes AS & CR" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
file_F <- paste0(ttl_F,".pdf")
demoMeans.df$variable_F <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif" = "Négatif", "Neutre" = "Neutre", "Positif" = "Positif",
                "Fonction publique" = "FP","Secrétariat du Conseil du Trésor du Canada (100%)" = "SCT")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada (100%)" = "#d1e7ee", "Fonction publique" = "#fabcb3")
#---------------



# Create separate TBS and PS dataframes
TBSdemoMeans.df <- filter(demoMeans.df, LEVEL1ID == "26" | BYCOND == "PS")
PSdemoMeans.df <- filter(demoMeans.df, LEVEL1ID == "0" | BYCOND == "TBS")

# Create TBS mean column to allow each small multiple to be compared to the TBS mean via geom_errorbar
TBSdemoMeans.df <- TBSdemoMeans.df %>%
  arrange(QSubTheme_E, variable, DESCRIP_E) %>%
  group_by(QSubTheme_E, variable) %>%
  mutate(TBSmean = value[2]) %>%
  mutate(TBSmeanDiff = value - TBSmean)

# Compute PS overall means to compare to the PS, TBS and Sector data

PSoverall <- PSdemoMeans.df %>%
  filter(BYCOND %in% c("PS","TBS")) %>%
  arrange(QSubTheme_E, variable, OrderKey) %>%
  group_by(QSubTheme_E, variable) %>%
  mutate(value = value[1])

PSdetail <- filter(PSdemoMeans.df, !(BYCOND %in% c("PS","TBS")))

PSdemoMeans.df <- bind_rows(PSoverall, PSdetail)





# Define a plotting function for all subthemes. Inputs are language (E or F) and graph dimensions in inches.
plotIS <- function(language, wdth = 10, hght = 8, textSize = 9) {
  
  if (language == "E") {
    TBSdemoMeans.df$variable_lang <- TBSdemoMeans.df$variable_E
    TBSdemoMeans.df$DESCRIP_lang <- TBSdemoMeans.df$DesProp_E
    TBSdemoMeans.df$QSubTheme_lang <- TBSdemoMeans.df$QSubTheme_E
    TBSdemoMeans.df$DemQ_lang <- TBSdemoMeans.df$DemQ_E
    PSdemoMeans.df$variable_lang <- PSdemoMeans.df$variable_E
    PSdemoMeans.df$DESCRIP_lang <- PSdemoMeans.df$DesProp_E
    PSdemoMeans.df$QSubTheme_lang <- PSdemoMeans.df$QSubTheme_E
    PSdemoMeans.df$DemQ_lang <- PSdemoMeans.df$DemQ_E
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
    TBSdemoMeans.df$variable_lang <- TBSdemoMeans.df$variable_F
    TBSdemoMeans.df$DESCRIP_lang <- TBSdemoMeans.df$DesProp_F
    TBSdemoMeans.df$QSubTheme_lang <- TBSdemoMeans.df$QSubTheme_F
    TBSdemoMeans.df$DemQ_lang <- TBSdemoMeans.df$DemQ_F
    PSdemoMeans.df$variable_lang <- PSdemoMeans.df$variable_F
    PSdemoMeans.df$DESCRIP_lang <- PSdemoMeans.df$DesProp_F
    PSdemoMeans.df$QSubTheme_lang <- PSdemoMeans.df$QSubTheme_F
    PSdemoMeans.df$DemQ_lang <- PSdemoMeans.df$DemQ_F
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
  
  ggplot(data=TBSdemoMeans.df, aes(x=variable_lang, y=value, fill = variable_lang)) +
    geom_rect(data = subset(TBSdemoMeans.df,DESCRIP_lang == TBS_lang),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSdemoMeans.df,DESCRIP_lang == PS_lang),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSdemoMeans.df,DemoQ %in% c("M_Q103A")),
              fill = "grey80",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(TBSdemoMeans.df,TBSmeanDiff >= 8 & variable == "NEGATIVE"),
              fill = "#e3f800",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.8) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    geom_errorbar(aes(ymax=TBSmean, ymin=TBSmean, linetype = TBSmean_lang), colour = "grey20") +
    geom_errorbar(data = PSdemoMeans.df, 
                  aes(ymax=value, ymin=value, linetype = PSmean_lang), colour = "grey20") +
    scale_linetype_manual(values=c("dotted", "solid")) +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    facet_grid(QSubTheme_lang ~ DESCRIP_lang, switch = "y", scales = "free_y",
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



plotIS("E", 8, 13.5, 8)
plotIS("F", 8, 13.5, 8)