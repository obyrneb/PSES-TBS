library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

loadPSES <- function() { 
  #Read all PSES 2017 Subsets
  ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
  ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
  ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
  ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
  ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
  mapQ <- read.csv("datasets//Question_Mappings.csv")
}

#Combine subsets as appropriate
ss1_5 <- rbind(ss1,ss2)
#ss1_5 <- rbind(ss1_5,ss3)
#ss1_5 <- rbind(ss1_5,ss4)
#ss1_5 <- rbind(ss1_5,ss5)


#Extract TBS data only for PSES 2017
ss.df <- subset(ss1_5, (LEVEL1ID == "26" & LEVEL2ID == "0") & SURVEYR == 2017 & QUESTION %in% c("A_Q18","E_Q54"),
                select=c(LEVEL1ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

#Create question subsections
ss.df$QSection <- substr(ss.df$QUESTION,0,1)
ss.df$QSection <- factor(ss.df$QSection)
ss.df$BYCOND[is.na(ss.df$BYCOND)] <- "TBS" 
ss.df$DemoQ <- word(ss.df$BYCOND, 1, sep = " =")
#ss.df <- merge(ss.df, mapQ ,"QUESTION")

#Filter data to remove less relevant demographics
ss.df <- subset(ss.df, !(DemoQ %in% c("M_Q108","M_Q109","M_Q110","M_Q111")))

#Aggregate by demographic and question subsection
demoMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ LEVEL1ID + QSection + QUESTION + DemoQ + BYCOND + DESCRIP_E + DESCRIP_F + TITLE_E + TITLE_F + SURVEYR, mean)
demoMeans.df <- melt(demoMeans.df, measure.vars = c("NEGATIVE","NEUTRAL","POSITIVE"))
demoMeans.df <- demoMeans.df %>% 
  arrange(DemoQ,BYCOND) %>%
  mutate(DESCRIP_E = factor(DESCRIP_E, unique(DESCRIP_E))) %>%
  mutate(DESCRIP_F = factor(DESCRIP_F, unique(DESCRIP_F)))

#Make TBS the first level
demoMeans.df$DESCRIP_E <- relevel(demoMeans.df$DESCRIP_E, "Treasury Board of Canada Secretariat")
demoMeans.df$DESCRIP_F <- relevel(demoMeans.df$DESCRIP_F, "Secrétariat du Conseil du Trésor du Canada")


#English captions and labels
file_E <- "PSES 2017 @ TBS - Innovation Questions by Demographic.pdf"
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question category and TBS sector. The bars over each column represent the average value for this question category across sectors."
expl_E <- paste0(strwrap(expl_E, 140), sep="", collapse="\n")
ttl_E <- "PSES 2017 @ TBS - Innovation Questions by Demographic"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
demoMeans.df$variable_E <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative", "Neutral", "Positive", "TBS")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat" = "#d1e7ee")

#French Captions and labels
file_F <- "SAFF 2017 @ SCT - Questions d'innovation par démographie.pdf"
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour une catégorie de questions particulière et un secteur du SCT. Les barres au dessus de chaque colonne représentent la valeur moyenne de cette catégorie de question pour tous les secteurs."
expl_F <- paste0(strwrap(expl_F, 140), sep="", collapse="\n")
ttl_F <- "SAFF 2017 @ SCT - Questions d'innovation par démographie" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
demoMeans.df$variable_F <- mapvalues(demoMeans.df$variable, 
                                     c("NEGATIVE","NEUTRAL","POSITIVE"),
                                     c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif", "Neutre", "Positif", "SCT")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada" = "#d1e7ee")

#Plot function
plotDemoSM <- function(language) {
  
  if (language == "E") {
    demoMeans.df$variable_lang <- demoMeans.df$variable_E
    demoMeans.df$DESCRIP_lang <- demoMeans.df$DESCRIP_E
    demoMeans.df$TITLE_lang <- demoMeans.df$TITLE_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    textSize <- 8
    TBS <- "Treasury Board of Canada Secretariat"
  } else if (language == "F") {
    demoMeans.df$variable_lang <- demoMeans.df$variable_F
    demoMeans.df$DESCRIP_lang <- demoMeans.df$DESCRIP_F
    demoMeans.df$TITLE_lang <- demoMeans.df$TITLE_F
    PNN_lang.clrs <- PNN_F.clrs
    PNN_lang.lbls <- PNN_F.lbls
    expl_lang <- expl_F
    ttl_lang <- ttl_F
    cap_lang <- cap_F
    file_lang <- file_F
    textSize <- 8
    TBS <- "Secrétariat du Conseil du Trésor du Canada"
  } else {
    return("Invalid language selection. Choose E (English) or F (French).")
  }
  
  ggplot(data=demoMeans.df, aes(x=variable_lang ,y=value, fill = variable_lang)) +
    geom_rect(data = subset(demoMeans.df,DESCRIP_lang == TBS),
              aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0,100)) +
    geom_text(vjust = 0.5, hjust = -0.4, colour ="grey45", angle = 90, size = 2.5, aes(label = value)) +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    facet_grid(TITLE_lang ~ DESCRIP_lang, switch = "y", scales = "free_y",
               labeller = labeller(DESCRIP_lang = label_wrap_gen(40), TITLE_lang = label_wrap_gen(30)))  +
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
          legend.position = "bottom",
          legend.title = element_blank())
  
  ggsave(file_lang, height = 6, width = 13)
  
  return()
  
}

plotDemoSM("E")