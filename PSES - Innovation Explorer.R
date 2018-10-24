library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

#Import sector-specifc dataset - subset 5 
ss5.df <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")

sector.ID <- "3" # LEVEL3ID for selected sector
sector_E.short <- "CIOB" #Set English short name
sector_F.short <- "" #Set French short name
sector.names <- subset(ss5.df, LEVEL1ID =="26" & LEVEL3ID == sector.ID,select = c(DESCRIP_E,DESCRIP_F))
sector_E.long <- toString(unique(sector.names$DESCRIP_E)) #Set English long name
sector_F.long <- toString(unique(sector.names$DESCRIP_F)) #Set French long name

#Filter data
sd.df <- subset(ss5.df, (LEVEL1ID == "0" |                       #Include Public Service topline
                       (LEVEL1ID == "26" & (LEVEL2ID == "0" |   #Include TBS topline
                        LEVEL3ID == sector.ID)))                #Include selected TBS sector
                        & QUESTION %in% c("A_Q01","A_Q18","A_Q21","A_Q22a","A_Q22c","A_Q22e",
                                          "A_Q22g","D_Q42","D_Q44","E_Q45","E_Q54"),
                select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE))
sd.df <- melt(sd.df)      #Convert positive, neutrla and negative columns to a single list 
sd.df <- na.omit(sd.df)   #Remove n/a values

#Reorder questions
#sd.df$TITLE_E <- factor(sd.df$TITLE_E, levels(sd.df$TITLE_E)[c(2,10,1,3:9,11)], ordered = TRUE)

#Shorten text
sd.df$Org_E <- mapvalues(sd.df$DESCRIP_E, 
                         c("Public Service","Treasury Board of Canada Secretariat",sector_E.long),
                         c("PS","TBS",sector_E.short))
sd.df$Org_E <- factor(sd.df$Org_E, levels = c("PS","TBS",sector_E.short), ordered = TRUE)
sd.df$Org_F <- mapvalues(sd.df$DESCRIP_F, 
                         c("Fonction publique","Secrétariat du Conseil du Trésor du Canada",sector_F.long),
                         c("FP","SCT",sector_F.short))
sd.df$Org_F <- factor(sd.df$Org_F, levels = c("FP","SCT",sector_F.short), ordered = TRUE)


#Plot Data
pdf(paste("PSES 2017 Innovation Questions", sector_E.long, ".pdf"), width = 10, height = 8)
ggplot(data = sd.df, aes(x = Org_E, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(position = "right") +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = c("NEGATIVE" = "#CD202C", "NEUTRAL" = "#63CECA", "POSITIVE" = "#CCDC00")) +
  #geom_text(aes(label=value), vjust = 0, colour = "white", size = 3.5) +
  facet_grid(TITLE_E ~ ., switch = "both", labeller = labeller(TITLE_E = label_wrap_gen(100))) +
  labs(title = paste("PSES 2017 Innovation Questions - ",sector_E.long), caption = "2017 Public Service Employee Survey Open Datasets") +
  theme(plot.title = element_text(size = 16, hjust = 1, colour = "grey40"),
        plot.subtitle = element_text(face = "bold", size = 10, colour = "grey40"),
        plot.caption = element_text(face = "italic", size = 10, colour = "grey45"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 6, colour = "grey45", vjust = 0.5, hjust = 0.5),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = "grey45", vjust = 0, hjust = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(colour = "grey45"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        #strip.placement = "inside",
        #panel.spacing.y = unit(5, "mm"),
        #panel.spacing.x = unit(0.5, "mm"),
        strip.text.y = element_text(angle = 180, size = 10, colour = "grey40", hjust = 0, inherit.blank = FALSE),
        strip.text.x = element_text(size = 10, colour = "grey40", inherit.blank = FALSE),
        strip.background = element_blank(),
        legend.text = element_text(size = 10, colour = "grey45"),
        legend.title = element_blank())
dev.off()