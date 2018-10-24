library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)

#Import sector-specifc dataset - subset 5 
ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")
PA2017 <- read_excel("datasets//TBS_SCT_2017_PSEAS_SAAFF.xls", sheet = 2, na = "9999")
P2014 <- read.csv("datasets//2014-results-resultats-organization.csv", na.strings = "9999")

sector.ID <- "28" # LEVEL3ID for selected sector
sector_E.short <- "SCMA" #Set English short name
sector_F.short <- "CSAM" #Set French short name
sector.names <- subset(ss5, LEVEL1ID =="26" & LEVEL3ID == sector.ID,select = c(DESCRIP_E,DESCRIP_F))
sector_E.long <- toString(unique(sector.names$DESCRIP_E)) #Set English long name
sector_F.long <- toString(unique(sector.names$DESCRIP_F)) #Set French long name

#Filter data
Qs2017.df <- subset(ss5, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                       #Include TBS topline
                       (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                                              #Include selected TBS sector                       
                                              LEVEL3ID == sector.ID) &
                      #Include questions
                      QUESTION %in% c("A_Q09","A_Q20","E_Q57","A_Q15","D_Q42","D_Q39","E_Q53"),
                    select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE))
Qs2017.df$SURVEYR_E <- mapvalues(Qs2017.df$SURVEYR, 2017, "PSES 2017")
Qs2017.df$SURVEYR_F <- mapvalues(Qs2017.df$SURVEYR, 2017, "SAFF 2017")
Qs2017.df$NewQ <- mapvalues(Qs2017.df$QUESTION, 
                            c("A_Q09","A_Q20","E_Q57","A_Q15","D_Q42","D_Q39","E_Q53"),
                            c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07")) 

Qs2017a.df <- subset(PA2017, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                       #Include TBS topline
                       (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                                              #Include selected TBS sector                       
                                              LEVEL3ID == "302") &
                      #Include questions
                      QUESTION %in% c("Q05","Q07","Q02"),
                    select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE))
Qs2017a.df$SURVEYR_E <- mapvalues(Qs2017a.df$SURVEYR, 2017, "PSEAS 2017")
Qs2017a.df$SURVEYR_F <- mapvalues(Qs2017a.df$SURVEYR, 2017, "SAAFF 2017")
Qs2017a.df$NewQ <- mapvalues(Qs2017a.df$QUESTION, 
                            c("Q05","Q07","Q02"),
                            c("NQ01","NQ02","NQ03"))

Qs2014.df <- subset(P2014, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                       #Include TBS topline
                       (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                                              #Include selected TBS sector                       
                                              LEVEL3ID == "360") &
                      #Include questions
                      QUESTION %in% c("A_Q09","A_Q19","E_Q57","D_Q41","D_Q38","E_Q54"),
                    select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE))
Qs2014.df$SURVEYR_E <- mapvalues(Qs2014.df$SURVEYR, 2014, "PSES 2014")
Qs2014.df$SURVEYR_F <- mapvalues(Qs2014.df$SURVEYR, 2014, "SAFF 2014")
Qs2014.df$NewQ <- mapvalues(Qs2014.df$QUESTION, 
                            c("A_Q09","A_Q19","E_Q57","D_Q41","D_Q38","E_Q54"),
                            c("NQ01","NQ02","NQ03","NQ05","NQ06","NQ07")) 

#QsSCMA.df <- merge(Qs2017.df,Qs2014.df, by = c("DESCRIP_E","DESCRIP_F","NewQ","TITLE_E","TITLE_F","SURVEYR","POSITIVE","NEUTRAL","NEGATIVE"), all = TRUE)
#QsSCMA.df <- merge(QsSCMA.df,Qs2017a.df, by = c("DESCRIP_E","DESCRIP_F","NewQ","TITLE_E","TITLE_F","SURVEYR","POSITIVE","NEUTRAL","NEGATIVE"), all = TRUE)

QsSCMA.df <- rbind(Qs2017.df,Qs2014.df,Qs2017a.df)

QsSCMA.df$SURVEYR_E <- factor(QsSCMA.df$SURVEYR_E, levels = c("PSES 2014","PSEAS 2017","PSES 2017"))
QsSCMA.df$SURVEYR_F <- factor(QsSCMA.df$SURVEYR_F, levels = c("SAFF 2014","SAAFF 2017","SAFF 2017"))

QsSCMA.df$NewQ_E <- mapvalues(QsSCMA.df$NewQ,
                              c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07"),
                              c("I have support at work to balance my work and personal life.",
                                "Overall, I like my job.",
                                "Overall, my department or agency treats me with respect.",
                                "Overall, I feel valued at work.",
                                "Senior management in my department or agency makes effective and timely decisions.",
                                "I receive the support I need from senior management to address unsatisfactory performance issues in my work unit.",
                                "I believe I have opportunities for promotion within my department or agency, given my education, skills and experience."))

QsSCMA.df$NewQ_F <- mapvalues(QsSCMA.df$NewQ,
                              c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07"),
                              c("Je reçois du soutien au travail pour concilier mon travail et ma vie personnelle.",
                                "Dans l’ensemble, j’aime mon emploi.",
                                "Dans l’ensemble, mon ministère ou organisme me traite avec respect.",
                                "Dans l'ensemble, je me sens valorisé(e) au travail.",
                                "La haute direction de mon ministère ou organisme prend des décisions efficaces et opportunes.",
                                "La haute direction m’offre le soutien dont j’ai besoin pour aborder les problèmes de rendement insatisfaisant dans mon unité de travail.",
                                "J’estime avoir des possibilités d’obtenir une promotion au sein de mon ministère ou organisme, compte tenu de ma scolarité, de mes compétences et de mon expérience."))


QsSCMA.df$DESCRIP_E <- factor(QsSCMA.df$DESCRIP_E,
                              levels = c("Public Service","Treasury Board of Canada Secretariat","Strategic Communications and Ministerial Affairs Sector"))
QsSCMA.df$DESCRIP_F <- factor(QsSCMA.df$DESCRIP_F,
                              levels = c("Fonction publique","Secrétariat du Conseil du Trésor du Canada","Secteur des communications stratégiques et affaires ministérielles"))

QsSCMA.df <- melt(QsSCMA.df, measure.vars = c("POSITIVE","NEUTRAL","NEGATIVE"))

#English captions and labels
file_E <- "PSES 2014-2017 @ SCMA.pdf"
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question for a particualr survey. Blank cells indicate that the question was not asked by the corresponding survey."
expl_E <- paste0(strwrap(expl_E, 80), sep="", collapse="\n")
ttl_E <- "PSES 2014-2017 @ SCMA"
cap_E <- "Public Service Employee Survey Open Datasets"
QsSCMA.df$variable_E <- mapvalues(QsSCMA.df$variable, 
                                  c("NEGATIVE","NEUTRAL","POSITIVE"),
                                  c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative", "Neutral", "Positive", "TBS")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat" = "#d1e7ee")

#French Captions and labels
file_F <- "SAFF 2014-2017 @ CSAM.pdf"
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour une questions pour un sondage en particulier. Les cellules vides indiquent que la question ne fût pas posée dans le cadre du sondage correspondant."
expl_F <- paste0(strwrap(expl_F, 80), sep="", collapse="\n")
ttl_F <- "SAFF 2014-2017 @ CSAM" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux"
QsSCMA.df$variable_F <- mapvalues(QsSCMA.df$variable, 
                                  c("NEGATIVE","NEUTRAL","POSITIVE"),
                                  c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif", "Neutre", "Positif", "SCT")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada" = "#d1e7ee")

#Plot function
plotSCMAQs <- function(language, output = "PDF") {
  
  if (language == "E") {
    QsSCMA.df$variable_lang <- QsSCMA.df$variable_E
    QsSCMA.df$DESCRIP_lang <- QsSCMA.df$DESCRIP_E
    QsSCMA.df$TITLE_lang <- QsSCMA.df$TITLE_E
    QsSCMA.df$NewQ_lang <- QsSCMA.df$NewQ_E
    QsSCMA.df$SURVEYR_lang <- QsSCMA.df$SURVEYR_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    textSize <- 8
    TBS <- "Treasury Board of Canada Secretariat"
  } else if (language == "F") {
    QsSCMA.df$variable_lang <- QsSCMA.df$variable_F
    QsSCMA.df$DESCRIP_lang <- QsSCMA.df$DESCRIP_F
    QsSCMA.df$TITLE_lang <- QsSCMA.df$TITLE_F
    QsSCMA.df$NewQ_lang <- QsSCMA.df$NewQ_F
    QsSCMA.df$SURVEYR_lang <- QsSCMA.df$SURVEYR_F
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
  
  
  SCMA.plt <- ggplot(data=QsSCMA.df, aes(x=variable_lang,y=value, fill = variable_lang)) +
    #geom_rect(data = subset(QsSCMA.df,DESCRIP_lang == TBS),
     #         aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
      #        ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    scale_y_continuous(breaks = c(0,50,100), limits = c(0,115), position = "right") +
    geom_text(vjust = -0.4, colour ="grey45", size = 3, aes(label = value)) +
    ggtitle(ttl_lang) +
    labs(fill = "Response type", 
         subtitle = expl_lang,
         caption = cap_lang) +
    facet_grid(NewQ_lang ~ SURVEYR_lang, switch = "y", scales = "free_y",
               labeller = labeller(SURVEYR_lang = label_wrap_gen(30), NewQ_lang = label_wrap_gen(40)))  +
    theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
          plot.subtitle = element_text(face = "bold", size = textSize, colour = "grey40"),
          plot.caption = element_text(face = "italic", size = textSize, colour = "grey45"),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 0, size = textSize, colour = "grey60", inherit.blank = FALSE),
          axis.text.y = element_text(angle = 0, size = textSize, colour = "grey60", inherit.blank = FALSE),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(colour = "grey60"),
          panel.background = element_rect(fill = "grey95"),
          panel.grid = element_blank(),
          panel.spacing.y = unit(2, "mm"),
          panel.spacing.x = unit(1, "mm"),
          strip.text.y = element_text(angle = 180, size = textSize, colour = "grey40", inherit.blank = FALSE),
          strip.text.x = element_text(angle = 0, size = textSize, colour = "grey40", inherit.blank = FALSE),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = textSize, colour = "grey45"),
          legend.title = element_blank())
  
  ggsave(file_lang, plot = SCMA.plt, width = 10, height = 8)
  
  return(SCMA.plt)
  
}

plotSCMAQs("E", "PDF")
plotSCMAQs("F", "PDF")
