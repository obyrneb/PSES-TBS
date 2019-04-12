library(scales)
library(readxl)
library(tidyverse)
library(ggrepel)
library(grid)
library(cowplot)

# LOAD DATA
#----

mainDir <- getwd()
dataDir <- "datasets"
plotDir <- "plots"
ss5File_2018 <- file.path(mainDir,dataDir,"pses2018_ss5.csv")
ss5File_2017 <- file.path(mainDir,dataDir,"pses2017_ss5.csv")
ss5URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-5_Sous-ensemble-5.csv" # Due for release in March 2018
ss5URL_2017 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv" # Due for release in March 2018

ifelse(!dir.exists(file.path(mainDir, dataDir)), dir.create(file.path(mainDir, dataDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, plotDir)), dir.create(file.path(mainDir, plotDir)), FALSE)

if(!file.exists(ss5File_2018)) {download.file(ss5URL_2018,ss5File_2018)}
if(!file.exists(ss5File_2017)) {download.file(ss5URL_2017,ss5File_2017)}

if(!exists("ss5_2018")) {ss5_2018 <- read.csv(ss5File_2018, na.strings = "9999")}
if(!exists("ss5_2017")) {ss5_2017 <- read.csv(ss5File_2017, na.strings = "9999")}
if(!exists("indicatorMap")) {
  indicatorMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Indicator_Mapping.csv"))}

sectorAbbr <- read.csv(file.path(mainDir,dataDir,"PSES2018_TBS_Sector_Abbreviations.csv"),
                       colClasses = c("character","character"))

Qcorr <- read.csv(file.path(mainDir,dataDir,"PSES2018_Question_Corr.csv"), na.strings = "N/A") %>%
  mutate(PSES_2017 = ifelse(PSES_2018 == "48","63",as.character(PSES_2017))) %>% # Add harassment question from 2017
  mutate(PSES_2017 = ifelse(PSES_2018 == "55","75",as.character(PSES_2017))) %>% # Add discrimination question from 2017
  gather("PSES_year","QUESTION",-QUESTION_E) %>%
  mutate(QUESTION = ifelse(is.na(QUESTION),NA,
                           paste0("Q",
                                  ifelse(QUESTION %in% c("1","2","3","4","5","6","7","8","9"),"0",""),
                                  QUESTION))) %>%
  spread(PSES_year,QUESTION) %>%
  arrange(PSES_2018)

sectors_2017 <- ss5_2017 %>%
  filter(LEVEL1ID %in% c(0,26)) %>%
  mutate(QUESTION = substring(QUESTION,3)) %>%
  left_join(select(Qcorr,QUESTION="PSES_2017",Q2018="PSES_2018"), by = "QUESTION") %>%
  filter(!is.na(Q2018)) %>%
  select(DESCRIP_E,QUESTION="Q2018",s100_2017="SCORE100",agree_2017="AGREE")

question100s <- ss5_2018 %>%
  filter(LEVEL1ID %in% c(0,26)) %>%
  left_join(indicatorMap, by = "QUESTION") %>%
  left_join(sectorAbbr, by = "DESCRIP_E") %>%
  mutate(unitcode = ifelse(BYCOND == "",
                           ifelse(LEVEL1ID == 26, "TBS","PS"),
                           word(BYCOND, 2, sep = " = "))) %>%
  #filter(!(is.na(SCORE100))) %>%
  left_join(sectors_2017, by = c("QUESTION","DESCRIP_E")) %>%
  rename(s100_2018 = "SCORE100") %>%
  gather("SURVEYR","SCORE100",s100_2018,s100_2017) %>%
  mutate(SURVEYR = substring(SURVEYR,6)) %>%
  #filter(!(is.na(SCORE100))) %>%
  group_by(SURVEYR,unitcode) %>%
  mutate(overall100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()

score100s <- question100s %>%
  group_by(SURVEYR,unitcode,DESCRIP_E,DESCRIP_F,abbr_E,INDICATORID,INDICATORENG,INDICATORFRA,overall100) %>%
  summarise(indicator100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()
#----

#----
# SET SECTOR
#apply(sectorAbbr,1,) 



thisSector <- 301
thisAbbr_E <- "OCIO"

sectorData <- score100s %>%
  filter(unitcode %in% c(thisSector,"TBS")) %>%
  mutate(abbr_E = ifelse(unitcode == thisSector, thisAbbr_E, unitcode))

sectorName_E <- sectorData$DESCRIP_E[[1]]
#sectorName_E <- "Office of the Chief Information Officer"

ttl_E <- paste0("PSES 2018 Report Card - ",sectorName_E)

#----
# CONSTRUCT REPORT CARD FUNCTION

report_card <- function(thisSector, question100s, score100s, customName = NULL, customAbbr = NULL) {

  sectorData <- score100s %>%
    filter(unitcode %in% c(thisSector,"TBS")) 
  
  sectorName_E <- ifelse(is.null(customName), sectorData$DESCRIP_E[[1]], customName)
  thisAbbr_E <- ifelse(is.null(customAbbr), sectorData$abbr_E[[1]], customAbbr)
  
  ttl_E <- paste0("PSES 2018 Report Card - ",sectorName_E)
  
  #----
# Consutruct slopechart comparing sector and TBS

# Set slopechart theme
slopeTheme <- list(
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=10, face="bold")),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(text = element_text(colour="grey30")),
  theme(plot.title       = element_text(size=10, hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5)),
  # Change canvas size
  #theme(plot.margin = margin(t = .2, r = 1, b = .2, l = 1, "in")) +
  # Put facet labels on the left and horizontal
  theme(strip.text.y = element_text(angle = 180, size = 8)),
  theme(strip.text.x = element_text(size = 10, colour = "grey30", face="italic")),
  theme(strip.background = element_blank())
  #theme(panel.background = element_rect(fill = "grey95"))
)

# Build slopechart
slope.plt <- ggplot(sectorData, aes(x = SURVEYR, y = round(indicator100,0), group = INDICATORENG)) +
  facet_grid(.~abbr_E) + 
  geom_line(data = sectorData %>% filter(unitcode == thisSector),
            aes(color = INDICATORENG), alpha = 0.6,linetype = 1, size = 1) +
  geom_line(data = sectorData %>% filter(unitcode == "TBS"),
            aes(color = INDICATORENG), alpha = 0.6,linetype = 2, size = 1) +
  scale_colour_brewer(palette = "Set2") +
  geom_text_repel(data = sectorData %>% filter(SURVEYR == 2017), 
                  aes(label = str_wrap(paste0(INDICATORENG," (ind",INDICATORID,")"),10), colour = INDICATORENG), 
                  hjust = 2, 
                  fontface = "bold", 
                  size = 2,
                  nudge_x = -1, 
                  direction = "y") +
  geom_text_repel(data = sectorData %>% filter(SURVEYR == 2018), 
                  aes(label = str_wrap(paste0(INDICATORENG," (ind",INDICATORID,")"),10), colour = INDICATORENG), 
                  hjust = -1, 
                  fontface = "bold", 
                  size = 2,
                  nudge_x = 1, 
                  direction = "y") +
  geom_point(colour = "white", size = 8, shape = 16) +
  geom_text(aes(label = round(indicator100,0), y = round(indicator100,0)),
            size = 3, colour = "grey30", fontface = "bold") +
  #scale_alpha_manual(values = c(1,.67)) +
  # Move the x axis labels up top
  scale_x_discrete(position = "top", expand = expand_scale(add = 1)) +
  scale_y_continuous(expand = expand_scale(add = 1)) +
  # Reuse theme
  slopeTheme

# Create title grob
slope.ttl <- textGrob("Year-to-Year Score 100 Comparsion", gp=gpar(fontsize = 10, fontface = "bold", col = "grey30"))
space.grb <- textGrob("")

# Add title to slopechart
top_left.grb <- plot_grid(slope.ttl,
                       slope.plt,
                       space.grb,
                       nrow=3,rel_heights = c(1,11.5,0.5))
top_left.grb


#----
# Create top negative and postive shifts

# Determine the deltas between PSES 2017 and PSES 2018 data
sectorDeltas <- question100s %>%
  filter(unitcode %in% c(thisSector, "TBS")) %>%
  mutate(abbr_E = ifelse(unitcode == thisSector, thisAbbr_E, unitcode)) %>%
  select(INDICATORID,INDICATORENG,INDICATORFRA,QUESTION,TITLE_E,TITLE_F,
         unitcode,abbr_E,DESCRIP_E,DESCRIP_F,SURVEYR,SCORE100,AGREE) %>%
  spread(SURVEYR,SCORE100) %>%
  mutate(delta = `2018`-`2017`)

# Get the the 10 best and the 10 worst deltas
#best10deltas <- filter(sectorDeltas, unitcode == thisSector & delta > 0) %>% top_n(10,delta) %>% select(QUESTION)
#worst10deltas <- filter(sectorDeltas, unitcode == thisSector & delta < 0) %>% top_n(-10,delta) %>% select(QUESTION)
best10deltas <- filter(sectorDeltas, unitcode == thisSector & delta > 0) %>%
  arrange(desc(delta),`2018`) %>% slice(1:10) %>% select(QUESTION,delta,`2018`)
worst10deltas <- filter(sectorDeltas, unitcode == thisSector & delta < 0) %>%
  arrange(delta,`2018`) %>% slice(1:10) %>% select(QUESTION,delta,`2018`)

# Set the delta theme, which we will reuse for both charts
deltaTheme <- list( 
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.minor.x = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x      = element_text(size = 6)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(text = element_text(colour="grey30")),
  theme(plot.title       = element_text(size = 8, hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5)),
  # Change canvas size
  #theme(plot.margin = margin(t = .2, r = 1, b = .2, l = 1, "in")) +
  # Put facet labels on the left and horizontal
  theme(strip.text.y = element_text(angle = 180, size = 6, hjust = 0)),
  theme(strip.background = element_blank())
  #theme(panel.background = element_rect(fill = "grey95")) +
)

# Use the 10 best deltas to build the data for the "Most postive shifts"
bestData <- sectorDeltas %>%
  inner_join(select(best10deltas,QUESTION), by = "QUESTION") %>%
  filter(unitcode != "TBS")

# Build the "Most postive shifts" chart
best.plt <- ggplot(data = bestData, x = abbr_E, group = abbr_E) +
  geom_col(aes(x = abbr_E, y = `2018`), fill = "#f7f7f7", width = 0.8) +
  geom_hline(aes(yintercept = `2017`), colour = "grey60") +
  geom_hline(aes(yintercept = `2018`), colour = "grey60") +
  geom_segment(aes(x = abbr_E, xend = abbr_E, y = `2017`, yend = `2018`, colour = delta),
               size = 1, linejoin = "mitre", arrow = arrow(length = unit(0.2, "cm"))) +
  #geom_point(aes(x = abbr_E, y = `2017`, alpha = abbr_E), size = 8, shape = 16, colour = "blue") +
  #geom_point(aes(x = abbr_E, y = `2018`, alpha = abbr_E), size = 8, shape = 16, colour = "red") +
  geom_text(aes(label = `2017`, x = abbr_E, y = `2017`),
            size = 3, colour = "grey30", fontface = "plain", hjust = 1.3, vjust = 0.5) +
  geom_text(aes(label = `2018`, x = abbr_E, y = `2018`),
            size = 3, colour = "grey30", fontface = "bold", hjust = -0.3, vjust = 0.5) +
  geom_text(aes(label = paste0("(+",delta,")"), x = abbr_E, y = `2017`, colour = delta),
            size = 3, fontface = "bold", hjust = 2, vjust = 0.5) +
  #geom_text(aes(label = paste0("ind",INDICATORID), x = abbr_E, y = 0),
  #          size = 3, colour = "grey50", fontface = "plain", hjust = -0.1, vjust = 0.5) +
  coord_flip() +
  facet_grid(fct_reorder(paste0(INDICATORID,"-",INDICATORENG,": ",substr(TITLE_E,10,140)),
                         delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
  #scale_alpha_manual(values = c(1,.5)) +
  scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
  deltaTheme

# Use the 10 worst deltas to build the data for the "Most negative shifts"
worstData <- sectorDeltas %>%
  inner_join(select(worst10deltas,QUESTION), by = "QUESTION") %>%
  filter(unitcode != "TBS")

# Build the "Most negative shifts" chart
worst.plt <- ggplot(data = worstData, x = abbr_E, group = abbr_E) +
  geom_col(aes(x = abbr_E, y = `2018`), fill = "#f7f7f7", width = 0.8) +
  geom_hline(aes(yintercept = `2017`), colour = "grey60") +
  geom_hline(aes(yintercept = `2018`), colour = "grey60") +
  geom_segment(aes(x = abbr_E, xend = abbr_E, y = `2017`, yend = `2018`, colour = delta),
               size = 1, linejoin = "mitre", arrow = arrow(length = unit(0.2, "cm"))) +
  #geom_point(aes(x = abbr_E, y = `2017`, alpha = abbr_E), size = 8, shape = 16, colour = "blue") +
  #geom_point(aes(x = abbr_E, y = `2018`, alpha = abbr_E), size = 8, shape = 16, colour = "red") +
  geom_text(aes(label = `2017`, x = abbr_E, y = `2017`),
            size = 3, colour = "grey30", fontface = "plain", hjust = -0.3, vjust = 0.5) +
  geom_text(aes(label = `2018`, x = abbr_E, y = `2018`),
            size = 3, colour = "grey30", fontface = "bold", hjust = 1.3, vjust = 0.5) +
  geom_text(aes(label = paste0("(",delta,")"), x = abbr_E, y = `2018`, colour = delta),
            size = 3, fontface = "bold", hjust = 2, vjust = 0.5) +
  #geom_text(aes(label = paste0("ind",INDICATORID), x = abbr_E, y = 0),
  #          size = 3, colour = "grey50", fontface = "plain", hjust = -0.1, vjust = 0.5) +
  coord_flip() +
  facet_grid(fct_reorder(paste0(INDICATORID,"-",INDICATORENG,": ",substr(TITLE_E,10,140)),
                         delta)~.,switch = "y", labeller = label_wrap_gen(60)) +
  #scale_alpha_manual(values = c(1,.5)) +
  scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
  deltaTheme

# Create chart titles
best.ttl <- textGrob("Top Positive Shifts", gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
worst.ttl <- textGrob("Top Negative Shifts", gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))

# Combine the charts and title into the right-hand pane of the report card
right.grb <- plot_grid(best.ttl,
                       best.plt,
                       worst.ttl,
                       worst.plt,
                       nrow = 4, rel_heights = c(1,12,1,12))
right.grb 

#----
# Create the harassment and discrimination charts

# Extract the harassment and discrimination questions using the appropriate subindicators (12 and 13)
sectorHarDis <- question100s %>%
  filter(unitcode %in% c(thisSector, "TBS") & SURVEYR == 2018 & SUBINDICATORID %in% c(12,13)) %>%
  mutate(abbr_E = ifelse(unitcode == thisSector, thisAbbr_E, unitcode)) %>%
  select(QUESTION,TITLE_E,TITLE_F,unitcode,abbr_E,SURVEYR,AGREE,agree_2017) %>%
  mutate(delta = AGREE - agree_2017) %>%
  rename(`2017` = agree_2017, `2018` = AGREE)

# Build the harassment plot
#har <- ggplot(data = filter(sectorHarDis, QUESTION %in% c("Q48","Q55")), x = abbr_E, group = abbr_E) +
  #geom_col(aes(x = abbr_E, y = `2018`), colour = "grey80", fill = "#f7f7f7", width = 0.8) +
  #geom_segment(aes(x = abbr_E, xend = abbr_E, y = `2017`, yend = `2018`, colour = delta),
  #             size = 1, linejoin = "mitre", arrow = arrow(length = unit(0.3, "cm"))) +
  #geom_point(aes(x = abbr_E, y = `2017`, alpha = abbr_E), size = 8, shape = 16, colour = "blue") +
  #geom_point(aes(x = abbr_E, y = `2018`, alpha = abbr_E), size = 8, shape = 16, colour = "red") +
  #geom_text(aes(label = paste0(`2017`,"%"), x = abbr_E, y = `2017`),
  #          size = 3, colour = "grey30", fontface = "plain", vjust = -0.5) +
  #geom_text(aes(label = paste0(`2018`,"%"), x = abbr_E, y = `2018`),
  #          size = 3, colour = "grey30", fontface = "bold", vjust = -0.5) +
  #geom_text(aes(label = paste0("(",ifelse(delta>0,paste0("+",delta),delta),"%)"), # Add a "+" to delta if positive
  #              x = abbr_E, y = (`2017` + delta/2), colour = delta),
  #          size = 3, fontface = "bold", vjust = 1.5) +
  #geom_text(aes(label = abbr_E, x = abbr_E, y = 0),
  #          size = 3, fontface = "italic", hjust = 1.5, colour = "grey30") +
  #coord_flip() +
  #facet_grid(.~substr(TITLE_E,10,200),switch = "y", labeller = label_wrap_gen(50)) +
  #scale_alpha_manual(values = c(1,.5)) +
  #scale_colour_gradient2(low = "#0571b0", mid = "#f7f7f7", high = "#ca0020") +
  #scale_x_discrete(position = "top") +
  #deltaTheme +
  #theme(
    #axis.text.y = element_text(size = 8),
  #  strip.text.x = element_text(size = 6))

# Create the harassment and discrimination titles
har.ttl <- textGrob("Victims of Harassment", gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
dis.ttl <- textGrob("Victims of Discrimination", gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
har_dis.ttl <- plot_grid(har.ttl,dis.ttl)

# Create a single harassment and discrimination plot showing the rates of each (Q48 and Q55)
har_dis.plt <- ggplot(data = filter(sectorHarDis, QUESTION %in% c("Q48","Q55")), aes(x = abbr_E)) +
  geom_text(aes(label = paste0(abbr_E,": ",`2018`,"%"),y=1, colour = QUESTION), fontface = "bold.italic", size = 4) +
  facet_grid(.~str_wrap(substr(TITLE_E,10,200),50)) +
  scale_x_discrete(position = "bottom") +
  scale_colour_manual(values = c("#7fc97f","#beaed4")) +
  deltaTheme +
  theme(axis.text = element_blank()) +
  theme(strip.text = element_text(size = 7))

#hd.grb <- plot_grid(hd_ttl,hd,nrow=2, rel_heights = c(1,8))

# Extract the data on the nature of harassment
harNatureData <- sectorHarDis %>%
  filter(startsWith(QUESTION,"Q50")) %>%
  mutate(Qshort_E = word(TITLE_E,3, sep = fixed('.'))) %>%
  arrange(desc(unitcode),`2018`) %>%
  mutate(order = ifelse(unitcode == "TBS", row_number(), NA))
# Create the the nature of harassment chart
harNature.plt <- ggplot(harNatureData, aes(x=fct_reorder(str_wrap(substr(Qshort_E,1,50),30), order), y=`2018`)) +
  labs(
    x="Sectors", 
    y="% answering yes in 2018") +
  geom_col(aes(alpha = abbr_E), fill = "#7fc97f") +
  geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
            aes(label=ifelse(`2018`==0,"-",`2018`), y=0)) +
  coord_flip() +
  facet_grid(.~abbr_E) +
  scale_alpha_manual(values = c(1,.5)) +
  deltaTheme +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.text.x = element_blank()) +
  theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))

# Extract the data on the type of discrimination
disTypeData <- sectorHarDis %>%
  filter(startsWith(QUESTION,"Q57")) %>%
  mutate(Qshort_E = word(TITLE_E,3, sep = fixed('.'))) %>%
  arrange(desc(unitcode),`2018`) %>%
  mutate(order = ifelse(unitcode == "TBS", row_number(), NA))
# Create the type of discrimination chart
disType.plt <- ggplot(disTypeData, aes(x=fct_reorder(str_wrap(substr(Qshort_E,1,50),30), order), y=`2018`)) +
  labs(
    x="Sectors", 
    y="% answering yes in 2018") +
  geom_col(aes(alpha = abbr_E), fill = "#beaed4") +
  geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
            aes(label=ifelse(`2018`==0,"-",`2018`), y=0)) +
  coord_flip() +
  facet_grid(.~abbr_E) +
  scale_alpha_manual(values = c(1,.5)) +
  deltaTheme +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.text.x = element_blank()) +
  theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))
 
# Create nature of harassment and type of discrimination titles
harNature.ttl <- textGrob("Nature of Harassment",
                   gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
disType.ttl <- textGrob("Type of Discrimination",
                   gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))

# Combine the nature and type titles horizontally
nature_type.ttl <- plot_grid(harNature.ttl,disType.ttl)

# Combine the nature and type plots horizontally
nature_type.plt <- plot_grid(harNature.plt,disType.plt)

# Stack all of the harassment and discrimination titles and plots together to create the bottom-left panel
bottom_left.grb <- plot_grid(har_dis.ttl,
                         har_dis.plt,
                         nature_type.ttl,
                         nature_type.plt,
                         nrow=4, rel_heights = c(1,3.3,0.7,8))  

# Combine the top-left and bottom-left grobs to create the left-hand pane of the report card
left.grb <- plot_grid(top_left.grb,
                      bottom_left.grb,
                      nrow = 2)

# Create the report card title
report_card.ttl <- textGrob(ttl_E, hjust=0.85,
                         gp = gpar(fontsize = 14, fontface = "bold", col = "grey30"))

# Combine the left- and right-hand panes into a single grob, then add the title on top. All done!
report_card.grb <- plot_grid(left.grb,right.grb)
report_card.plt <- plot_grid(report_card.ttl,
                         report_card.grb,
                         nrow=2,rel_heights = c(1,20))
report_card.plt

# Save the report card into a PDF
ggsave(file.path(mainDir,plotDir,paste0(ttl_E,".pdf")), plot = report_card.plt, height = 8, width = 11)
}

#c(200,201,202,301,302,303,400,401,402,403,404,304,405,406,407,408,305,306,307,308,309,310,311,312,313,314,315,203)

sectorList <- distinct(score100s, unitcode, DESCRIP_E) %>% filter(!unitcode %in% c("TBS","PS","300","999"))

#sectorVector <- sectorList$unitcode

#tv <- c(200,300)

#lapply(sectorList,report_card, question100s, score100s)

for (i in sectorList$unitcode) {
  report_card(i, question100s, score100s)
  }
