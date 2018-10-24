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
P2011 <- read.csv("datasets//2011_results-resultats.csv", header = FALSE, na.strings = "9999")

#Add header names to PSES 2011 dataset
names(P2011) <- c("LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID",
                  "SURVEYR","BYCOND","DEMCODE",
                  "QUESTION",
                  "ANSWER1","ANSWER2","ANSWER3","ANSWER4","ANSWER5","ANSWER6","ANSWER7",
                  "POSITIVE","NEUTRAL","NEGATIVE",
                  "SCORE5","SCORE100","ANSCOUNT")


#Run this code to see all unique names and corresponding LevelIDs for TBS (LEVEL1ID=="26")
sectorList <- subset(ss5, LEVEL1ID == "26", select = c("LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID","DESCRIP_E","DESCRIP_F"))
View(unique(sectorList))

#Select the desired sector via its Level3ID (run code above to find it) and set its  long and short names
sector.ID <- "26" # LEVEL3ID for selected sector
sector_E.short <- "LS" #Set English short name
sector_F.short <- "SJ" #Set French short name
sector.names <- subset(ss5, LEVEL1ID =="26" & LEVEL3ID == sector.ID,select = c(DESCRIP_E,DESCRIP_F))
sector_E.long <- toString(unique(sector.names$DESCRIP_E)) #Set English long name
sector_F.long <- toString(unique(sector.names$DESCRIP_F)) #Set French long nameà

#Match for the sector across other data sets and extract appropriate LEVELIDs
PA2017_sectors <- subset(PA2017, LEVEL1ID == "26", select = c("LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID","DESCRIP_E","DESCRIP_F","SURVEYR"))
P2014_sectors <- subset(PA2014, LEVEL1ID == "26", select = c("LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID","DESCRIP_E","DESCRIP_F","SURVEYR"))
P2011_sectors <- subset(PA2011, LEVEL1ID == "26", select = c("LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID","DESCRIP_E","DESCRIP_F","SURVEYR"))

sectorLookup <- setNames(data.frame(matrix(ncol = 9, nrow = 3)), 
                         c("Survey","LEVEL1ID","LEVEL2ID","LEVEL3ID","LEVEL4ID" ,"LEVEL5ID","DESCRIP_E","DESCRIP_F","SURVEYR"))
sectorLookup$Survey <- c("PA2017","P2014","P2011")
skPA2017

#Filter PSES 2017 data
Qs2017.df <- subset(ss5, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                    #Include TBS topline
                    (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                       #Include selected TBS sector                       
                       LEVEL3ID == sector.ID) &
                      #Include questions
                      QUESTION %in% c("A_Q05","A_Q22a","A_Q22e","B_Q25","B_Q28","D_Q40","D_Q43","G_Q63"),
                    select=c(QUESTION,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE,ANSCOUNT))
Qs2017.df$SURVEYR_E <- mapvalues(Qs2017.df$SURVEYR, 2017, "PSES 2017")
Qs2017.df$SURVEYR_F <- mapvalues(Qs2017.df$SURVEYR, 2017, "SAFF 2017")
Qs2017.df$NewQ <- mapvalues(Qs2017.df$QUESTION, 
                            c("A_Q05","A_Q22a","A_Q22e","B_Q25","B_Q28","D_Q40","D_Q43","G_Q63"),
                            c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07","NQ08")) 


#Filter PSES 2014 data
Qs2014.df <- subset(P2014, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                    #Include TBS topline
                    (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                       #Include selected TBS sector                       
                       LEVEL3ID == "340") &
                      #Include questions
                      QUESTION %in% c("A_Q05","A_Q21a","A_Q21e","B_Q25","B_Q28","D_Q39","D_Q42","G_Q63"),
                    select=c(QUESTION,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE,ANSCOUNT))
Qs2014.df$SURVEYR_E <- mapvalues(Qs2014.df$SURVEYR, 2014, "PSES 2014")
Qs2014.df$SURVEYR_F <- mapvalues(Qs2014.df$SURVEYR, 2014, "SAFF 2014")
Qs2014.df$NewQ <- mapvalues(Qs2014.df$QUESTION, 
                            c("A_Q05","A_Q21a","A_Q21e","B_Q25","B_Q28","D_Q39","D_Q42","G_Q63"),
                            c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07","NQ08")) 

#Filter PSES 2011 data
Qs2011.df <- subset(P2011, 
                    #Include Public Service topline
                    #(LEVEL1ID == "0" |                       
                    #Include TBS topline
                    (LEVEL1ID == "26" & #(LEVEL2ID == "0" |   
                       #Include selected TBS sector                       
                       LEVEL2ID == "170") &
                      #Include questions
                      QUESTION %in% c("B_Q20","A_Q18a","A_Q18e","F_Q42","F_Q46"),
                    select=c(QUESTION,SURVEYR,POSITIVE,NEUTRAL,NEGATIVE,ANSCOUNT))
Qs2011.df$SURVEYR_E <- mapvalues(Qs2011.df$SURVEYR, 2011, "PSES 2011")
Qs2011.df$SURVEYR_F <- mapvalues(Qs2011.df$SURVEYR, 2011, "SAFF 2011")
Qs2011.df$NewQ <- mapvalues(Qs2011.df$QUESTION, 
                            c("B_Q20","A_Q18a","A_Q18e","F_Q42","F_Q46"),
                            c("NQ01","NQ02","NQ03","NQ06","NQ07"))



#Qs.df<- merge(Qs2017.df,Qs2014.df, by = c("DESCRIP_E","DESCRIP_F","NewQ","TITLE_E","TITLE_F","SURVEYR","POSITIVE","NEUTRAL","NEGATIVE"), all = TRUE)
#Qs.df<- merge(Qs.df,Qs2017a.df, by = c("DESCRIP_E","DESCRIP_F","NewQ","TITLE_E","TITLE_F","SURVEYR","POSITIVE","NEUTRAL","NEGATIVE"), all = TRUE)

Qs.df <- rbind(Qs2017.df,Qs2014.df,Qs2011.df)

Qs.df$SURVEYR_E <- factor(Qs.df$SURVEYR_E, levels = c("PSES 2011","PSES 2014","PSES 2017"))
Qs.df$SURVEYR_F <- factor(Qs.df$SURVEYR_F, levels = c("SAFF 2011","SAFF 2014","SAFF 2017"))

Qs.df$NewQ_E <- mapvalues(Qs.df$NewQ,
                              c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07","NQ08"),
                              c("Q05. I get the training I need to do my job.",
                                "Q22a. I feel that the quality of my work suffers because of...constantly changing priorities.",
                                "Q22e. I feel that the quality of my work suffers because of...having to do the same or more work, but with fewer resources.",
                                "Q25. In my work unit, every individual is accepted as an equal member of the team.",
                                "Q28. In my work unit, unsatisfactory employee performance is managed effectively.",
                                "Q40. Senior managers in my department or agency lead by example in ethical behaviour.",
                                "Q43. I believe that senior management will try to resolve concerns raised in this survey.",
                                "Q63. Having carefully read the definition of harassment above, have you been the victim of harassment on the job in the past two years?"))

Qs.df$NewQ_F <- mapvalues(Qs.df$NewQ,
                              c("NQ01","NQ02","NQ03","NQ04","NQ05","NQ06","NQ07","NQ08"),
                              c("Q05. Je reçois la formation dont j’ai besoin pour faire mon travail.",
                                "Q22a. J’estime que la qualité de mon travail est minée parce que...les priorités changent constamment.",
                                "Q22e. J’estime que la qualité de mon travail est minée parce que...je dois faire le même travail, ou en faire plus, avec moins de ressources.",
                                "Q25. Dans mon unité de travail, chaque personne est acceptée comme membre à part entière de l'équipe.",
                                "Q28. Dans mon unité de travail, le rendement insatisfaisant des employé(e)s est géré de manière efficace.",
                                "Q40. Les cadres supérieurs de mon ministère ou organisme montrent l’exemple par leur comportement éthique.",
                                "Q43. Je crois que la haute direction va s’efforcer de résoudre les problèmes soulevés dans le présent sondage.",
                                "Q63. Après avoir lu attentivement la définition du harcèlement ci-dessus, au cours des deux dernières années, avez-vous été victime de harcèlement au travail?"))

Qs.df<- melt(Qs.df, measure.vars = c("POSITIVE","NEUTRAL","NEGATIVE"))

#English captions and labels
file_E <- "PSES 2011-2017 @ Legal Services.pdf"
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question for a particualr survey. Blank cells indicate that the question was not asked by the corresponding survey or that results were suppressed due to a low number of responses (<5 or <10). The question numbers (e.g., Q05) are based on PSES 2017."
expl_E <- paste0(strwrap(expl_E, 90), sep="", collapse="\n")
ttl_E <- "PSES 2011-2017 @ Legal Services"
cap_E <- "Public Service Employee Survey Open Datasets"
Qs.df$variable_E <- mapvalues(Qs.df$variable, 
                                  c("NEGATIVE","NEUTRAL","POSITIVE"),
                                  c("Negative","Neutral","Positive"))
PNN_E.lbls <- c("Negative", "Neutral", "Positive")
PNN_E.clrs <- c("Negative" = "#CD202C", "Neutral" = "#63CECA", "Positive" = "#CCDC00")

#French Captions and labels
file_F <- "SAFF 2011-2017 @ Services juridiques.pdf"
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour une question pour un sondage en particulier. Les cellules vides indiquent que la question ne fût pas posée dans le cadre du sondage correspondant ou que les résultats ont été supprimés en raison d'un faible nombre de réponses (<5 ou <10). Les numéros des questions (par exemple, Q05) proviennent du SAFF 2017."
expl_F <- paste0(strwrap(expl_F, 90), sep="", collapse="\n")
ttl_F <- "SAFF 2011-2017 @ Services juridiques" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux"
Qs.df$variable_F <- mapvalues(Qs.df$variable, 
                                  c("NEGATIVE","NEUTRAL","POSITIVE"),
                                  c("Négatif","Neutre","Positif"))
PNN_F.lbls <- c("Négatif", "Neutre", "Positif")
PNN_F.clrs <- c("Négatif" = "#CD202C", "Neutre" = "#63CECA", "Positif" = "#CCDC00")

#Plot function
plotQs <- function(language) {
  
  if (language == "E") {
    Qs.df$variable_lang <- Qs.df$variable_E
    Qs.df$DESCRIP_lang <- Qs.df$DESCRIP_E
    Qs.df$TITLE_lang <- Qs.df$TITLE_E
    Qs.df$NewQ_lang <- Qs.df$NewQ_E
    Qs.df$SURVEYR_lang <- Qs.df$SURVEYR_E
    PNN_lang.clrs <- PNN_E.clrs
    PNN_lang.lbls <- PNN_E.lbls
    expl_lang <- expl_E
    ttl_lang <- ttl_E
    cap_lang <- cap_E
    file_lang <- file_E
    textSize <- 8
    TBS <- "Treasury Board of Canada Secretariat"
  } else if (language == "F") {
    Qs.df$variable_lang <- Qs.df$variable_F
    Qs.df$DESCRIP_lang <- Qs.df$DESCRIP_F
    Qs.df$TITLE_lang <- Qs.df$TITLE_F
    Qs.df$NewQ_lang <- Qs.df$NewQ_F
    Qs.df$SURVEYR_lang <- Qs.df$SURVEYR_F
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
  
  
ggplot(data=Qs.df, aes(x=variable_lang,y=value, fill = variable_lang)) +
    #geom_rect(data = subset(Qs.df,DESCRIP_lang == TBS),
    #         aes(fill = DESCRIP_lang),xmin = -Inf,xmax = Inf,
    #        ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = PNN_lang.clrs, labels = PNN_lang.lbls) +
    scale_y_continuous(breaks = c(0,50,100), limits = c(0,115), position = "right") +
    geom_text(vjust = -0.4, colour ="grey45", size = 3, aes(label = value)) +
    geom_text(vjust = -0.4, colour ="grey45", size = 3, check_overlap = TRUE, aes(x = 2, y = 100, label = paste0("n=",ANSCOUNT))) +
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
  
  ggsave(file_lang, width = 8, height = 10)
  
  return()
  
}

plotQs("E")
plotQs("F")
