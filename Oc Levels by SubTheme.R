library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
mapQ <- read.csv("datasets//Question_Mappings.csv")
mapDemQ <- read.csv("datasets//Demo_Question_Mappings.csv")

#Combine subsets as appropriate
ss1_3 <- rbind(ss1,ss2)
ss1_3 <- rbind(ss1_3,ss3)
#ss1_4 <- rbind(ss1_5,ss4)
#ss1_5 <- rbind(ss1_4,ss5)

#Extract TBS data only for 2017
ss.df <- subset(ss1_3, (LEVEL1ID == "26" & LEVEL2ID == "0") & SURVEYR == 2017,
                select=c(LEVEL1ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,
                         POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

# Create question subsections
ss.df$QSection <- substr(ss.df$QUESTION,0,1)
ss.df$QSection <- factor(ss.df$QSection)

# Create demographic question subsections
ss.df$BYCOND[is.na(ss.df$BYCOND)] <- "TBS" 
ss.df$DemoQ <- word(ss.df$BYCOND, 1, sep = " =")

# Add question and demographic sections from mapping tables
ss.df <- merge(ss.df, mapQ, by = "QUESTION")
ss.df <- merge(ss.df, mapDemQ, by = "BYCOND")

# Aggregate by demographic and question subsection
demoMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ 
                            LEVEL1ID + QSubTheme_E + QSubTheme_F + DemoQ + DemQ_E + DemQ_F + BYCOND +
                            DESCRIP_E + DESCRIP_F + SURVEYR, mean)

# Calculate proportions for each demographic group and append to the descriptors (DESCRIP_E and DESCRIP_F)
demoMeans.df <- demoMeans.df %>% 
  group_by(DemoQ, DESCRIP_E) %>%
  mutate(ngrp = sum(ANSCOUNT)) %>%
  group_by(DemoQ) %>%
  mutate(npop = sum(ANSCOUNT)) %>%
  ungroup() %>%
  mutate(prop = ngrp/npop) %>%
  mutate(DESCRIP_E = paste0(DESCRIP_E, " (", round((prop*100),0),"%)")) %>%
  mutate(DESCRIP_F = paste0(DESCRIP_F, " (", round((prop*100),0),"%)"))

# Transform the positive-neutral-negative crosstab into a list 
demoMeans.df <- melt(demoMeans.df, measure.vars = c("NEGATIVE","NEUTRAL","POSITIVE"))

# Order the descriptions by demographic question
demoMeans.df <- demoMeans.df %>% 
  arrange(DESCRIP_E) %>%
  mutate(DESCRIP_E = factor(DESCRIP_E, unique(DESCRIP_E))) %>%
  mutate(DESCRIP_F = factor(DESCRIP_F, unique(DESCRIP_F))) %>%
  mutate(DemQ_E = factor(DemQ_E, unique(DemQ_E))) %>%
  mutate(DemQ_F = factor(DemQ_F, unique(DemQ_F)))

# Order SubThemes my least to most negative  
subThemeOrder <- demoMeans.df %>%
  filter(DemoQ == "TBS" & variable == "NEGATIVE") %>%
  arrange(value)
demoMeans.df <- demoMeans.df %>%
  mutate(QSubTheme_E = factor(QSubTheme_E, unique(subThemeOrder$QSubTheme_E))) %>%
  mutate(QSubTheme_F = factor(QSubTheme_F, unique(subThemeOrder$QSubTheme_F)))

# Select demopgraphic groups to plot (occupational groups, equity groups, TBS overall)
demoMeans.df <- subset(demoMeans.df, DemoQ %in% c("OCCLEVEL","M_Q103A","TBS"))

# Make TBS overall the first level (don't forget the percentages appended to the descriptors!)
demoMeans.df$DESCRIP_E <- relevel(demoMeans.df$DESCRIP_E, "Treasury Board of Canada Secretariat (100%)")
demoMeans.df$DESCRIP_F <- relevel(demoMeans.df$DESCRIP_F, "Secrétariat du Conseil du Trésor du Canada (100%)")
demoMeans.df$DemQ_E <- relevel(demoMeans.df$DemQ_E, "TBS")
demoMeans.df$DemQ_F <- relevel(demoMeans.df$DemQ_F, "SCT")


# LABELS
#---------------
#English captions and labels
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question theme and demographic category. The bars over each column represent the average value for this question theme for TBS. Question themes are sorted from least negative to most negative (the red column) for TBS."
expl_E <- paste0(strwrap(expl_E, 140), sep="", collapse="\n")
ttl_E <- "PSES@TBS 2017 - Occupational Levels by Question Theme"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
file_E <- paste0(ttl_E,".pdf")
demoMeans.df$variable_E <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative", "Neutral", "Positive", "TBS")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat (100%)" = "#d1e7ee")

#French Captions and labels
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour un thème de question et une catégorie démographique particuliers. Les barres sur chaque colonne représentent la valeur moyenne pour ce thème de question pour le SCT. Les thèmes de la question sont triés du moins négatif au plus négatif (la colonne rouge) pour le SCT."
expl_F <- paste0(strwrap(expl_F, 140), sep="", collapse="\n")
ttl_F <- "SAFF@SCT 2017 - Niveaux professionnels par thème de question" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
file_F <- paste0(ttl_F,".pdf")
demoMeans.df$variable_F <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif", "Neutre", "Positif", "SCT")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada (100%)" = "#d1e7ee")
#---------------

# Create TBS-only dataframe to allow each small multiple to be compared tot he TBS mean via geom_errorbar
demoMeans.df <- demoMeans.df %>%
  arrange(QSubTheme_E, variable, DESCRIP_E) %>%
  group_by(QSubTheme_E, variable) %>%
  mutate(TBSmean = value[1])

#EN plot
plotDemoByTheme <- function(language, wdth = 10, hght = 8) {
  
  if (language == "E") {
    demoMeans.df$variable_lang <- demoMeans.df$variable_E
    demoMeans.df$DESCRIP_lang <- demoMeans.df$DESCRIP_E
    demoMeans.df$QSubTheme_lang <- demoMeans.df$QSubTheme_E
    demoMeans.df$DemQ_lang <- demoMeans.df$DemQ_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    textSize <- 9
    TBS <- "Treasury Board of Canada Secretariat (100%)"
  } else if (language == "F") {
    demoMeans.df$variable_lang <- demoMeans.df$variable_F
    demoMeans.df$DESCRIP_lang <- demoMeans.df$DESCRIP_F
    demoMeans.df$QSubTheme_lang <- demoMeans.df$QSubTheme_F
    demoMeans.df$DemQ_lang <- demoMeans.df$DemQ_F
    PNN_lang.clrs <- PNN_F.clrs
    PNN_lang.lbls <- PNN_F.lbls
    expl_lang <- expl_F
    ttl_lang <- ttl_F
    cap_lang <- cap_F
    file_lang <- file_F
    textSize <- 9
    TBS <- "Secrétariat du Conseil du Trésor du Canada (100%)"
  } else {
    return("Invalid language selection. Choose E (English) or F (French).")
  }
  
  ggplot(data=demoMeans.df, aes(x=variable_lang, y=value, fill = variable_lang)) +
    geom_rect(data = subset(demoMeans.df,DESCRIP_lang == TBS),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_rect(data = subset(demoMeans.df,DemoQ %in% c("M_Q103A")),
              fill = "grey80",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    geom_errorbar(aes(ymax=TBSmean, ymin=TBSmean), colour = "grey30") +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    facet_grid(QSubTheme_lang ~ DESCRIP_lang, switch = "y", #scales = "free_y",
               labeller = labeller(DESCRIP_lang = label_wrap_gen(30), QSubTheme_lang = label_wrap_gen(15)))  +
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

plotDemoByTheme("E", 11, 17)