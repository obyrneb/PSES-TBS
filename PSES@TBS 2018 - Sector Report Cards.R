library(scales)
library(readxl)
library(tidyverse)
library(ggrepel)
library(grid)
library(cowplot)



#----
# LOAD DATA

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

indicatorMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Indicator_Mapping.csv"))

QCtable <- read.csv(file.path(mainDir,dataDir,"PSES2018_Question_Corr.csv"), na.strings = "N/A")

#----
# SELECT DEPARTMENT
# Run this code to see a list of departments and their LEVEL1ID codes
distinct(ss5_2018, LEVEL1ID, DEPT_E, DEPT_F)

# Enter your department's LEVEL1ID code here (e.g., TBS is 26)
this_dept <- 26

# This next line reads a table where French and English abbreviations
# have been mapped to English sector names the DESCRIP_E field.
# You will need to construct your own, as these are not avaialable publically.
sectorAbbr <- read.csv(file.path(mainDir,dataDir,"PSES2018_TBS_Sector_Abbreviations.csv"),
                       colClasses = c("character","character","character"))

#----
# PRE-PROCESS DATA

# Create Question Correspondance table, add in dicriminationa dn harssment comparators, even if they aren't included
# (Exclusion from  the original table was due to different timeframes in the questions, 24 months in 2017 vs 12 months in 2018.)
QCtable_har_dis <- QCtable %>%
  mutate(PSES_2017 = ifelse(PSES_2018 == "48","63",as.character(PSES_2017))) %>% # Add harassment question from 2017
  mutate(PSES_2017 = ifelse(PSES_2018 == "55","75",as.character(PSES_2017))) %>% # Add discrimination question from 2017
  gather("PSES_year","QUESTION",-QUESTION_E) %>%
  mutate(QUESTION = ifelse(is.na(QUESTION),NA,
                           paste0("Q",
                                  ifelse(QUESTION %in% c("1","2","3","4","5","6","7","8","9"),"0",""),
                                  QUESTION))) %>%
  spread(PSES_year,QUESTION) %>%
  arrange(PSES_2018)

# Get PSES 2017 sector-level results and use the lookup table to filter on 2018 questions and create needed fields to merge
sectors_2017 <- ss5_2017 %>%
  filter(LEVEL1ID %in% c(0,this_dept)) %>%
  mutate(QUESTION = substring(QUESTION,3)) %>%
  left_join(select(QCtable_har_dis,QUESTION="PSES_2017",Q2018="PSES_2018"), by = "QUESTION") %>%
  filter(!is.na(Q2018)) %>%
  select(DESCRIP_E,QUESTION="Q2018",s100_2017="SCORE100",agree_2017="AGREE")

# Filter PSES 2018 sector-level questions and then merge 2017 data. 
# Gather allows us to add rows using the exiting SURVEYR field.
question100s <- ss5_2018 %>%
  filter(LEVEL1ID %in% c(0,this_dept)) %>%
  left_join(select(indicatorMap,-matches("TITLE_")), by = "QUESTION") %>%
  left_join(sectorAbbr, by = "DESCRIP_E") %>%
  mutate(unitcode = ifelse(BYCOND == "",
                           ifelse(LEVEL1ID == this_dept, "dept","PS"),
                           word(BYCOND, 2, sep = " = "))) %>%
  left_join(sectors_2017, by = c("QUESTION","DESCRIP_E")) %>%
  rename(s100_2018 = "SCORE100") %>%
  gather("SURVEYR","SCORE100",s100_2018,s100_2017) %>%
  mutate(SURVEYR = substring(SURVEYR,6)) %>%
  group_by(SURVEYR,unitcode) %>%
  mutate(overall100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()

# Create 2017 and 2018 means for the PSES indicators.
score100s <- question100s %>%
  group_by(SURVEYR,unitcode,DESCRIP_E,DESCRIP_F,abbr_E,abbr_F,INDICATORID,INDICATOR_E,INDICATOR_F,overall100) %>%
  summarise(indicator100 = mean(SCORE100, na.rm = TRUE)) %>%
  ungroup()

#----
# SET SECTOR - for testing purposes only


thisUnitcode <- "301"
thisAbbr <- "OCIO"

customName <- "Office of the Chief Information Officer"
customAbbr <- "OCIO"

sectorData <- score100s %>%
  filter(unitcode %in% c(thisUnitcode,"301")) %>%
  mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode))

thisSectorName_E <- sectorData$DESCRIP_E[[1]]
#thisSectorName_E <- "Office of the Chief Information Officer"

ttl_E <- paste0("PSES 2018 Report Card - ",thisSectorName_E)

lang = "F"
#----## CONSTRUCT REPORT CARD FUNCTION

report_card <- function(thisUnitcode, lang, customName = NULL, customAbbr = NULL, question100s = question100s, score100s = score100s) {

  if (!(lang %in% c("F","E"))) {print("Invalid selection. Choose E or F as a language.")}
  
  # Ensure the unitcode is read as a character, not a numeric
  thisUnitcode <- as.character(thisUnitcode)
  
  # Both of the next blocks select the appropriate bilingual fields and delete
  # fields from the other language. Indicator order is also set to correspond to
  # the numeric indicator ID.
  sectorData <- score100s %>%
    as_tibble() %>% 
    filter(unitcode %in% c(thisUnitcode,"dept")) %>%
    set_names(~sub(paste0("_",lang),"_lang",.x)) %>%
    select(-matches("_[EF]")) %>% 
    mutate(INDICATOR_lang = fct_reorder(INDICATOR_lang,INDICATORID))
  
  questionData <- question100s %>% 
    as_tibble() %>% 
    filter(unitcode %in% c(thisUnitcode,"dept")) %>%
    set_names(~sub(paste0("_",lang),"_lang",.x)) %>%
    select(-matches("_[EF]")) %>% 
    mutate(INDICATOR_lang = fct_reorder(INDICATOR_lang,INDICATORID))
  
  
  # Set sectors name and abbreviation. Retrieve from dataframe by default,
  # otherwise use custom parameters.
  thisSectorName <- ifelse(is.null(customName), 
                           as.character(sectorData$DESCRIP_lang[sectorData$unitcode == thisUnitcode]),
                           customName)
  thisAbbr <- ifelse(is.null(customAbbr),
                     as.character(sectorData$abbr_lang[sectorData$unitcode == thisUnitcode]),
                     customAbbr)

  # Replace existing abbreviation if a custom abbreviation was used
  sectorData <- sectorData %>% mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, abbr_lang))
  questionData <- questionData %>% mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, abbr_lang))
  
  # Make sure that the sector always comes before the department whenever they are compared.
  sectorData <- sectorData %>% mutate(abbr_lang = fct_relevel(abbr_lang,thisAbbr))
  questionData <- questionData %>% mutate(abbr_lang = fct_relevel(abbr_lang,thisAbbr))
  
  # Get the number of respondents for the sector
  thisAnscount <- question100s %>% 
    filter(unitcode == thisUnitcode & SURVEYR == 2018) %>% 
    summarise(ANSCOUNT = max(ANSCOUNT)) %>% 
    pull(ANSCOUNT)
  
  # Create the report card title - it includes the number of respondents.
  ttl_lang <- case_when(
    lang == "E" ~ paste0("PSES 2018 Report Card - ",thisSectorName," (responses = ",thisAnscount,")"),
    lang == "F" ~ paste0("Bulletin SAFF 2018 - ",thisSectorName," (résponses = ",thisAnscount,")")
    )
    
  #----
  ## TOP-LEFT: Consutruct slopechart comparing sector and TBS

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
    theme(axis.text.x.top      = element_text(size = 10, face = "bold")),
    # Remove x & y tick marks
    theme(axis.ticks       = element_blank()),
    # Format title & subtitle
    theme(text = element_text(colour = "grey30")),
    theme(plot.title       = element_text(size = 10, hjust = 0.5)),
    theme(plot.subtitle    = element_text(hjust = 0.5)),
    # Put facet labels on the left and horizontal
    theme(strip.text.y = element_text(angle = 180, size = 8)),
    theme(strip.text.x = element_text(size = 10, colour = "grey30", face = "italic")),
    theme(strip.background = element_blank())
  )
  
  # Build slopechart
  slope.plt <- ggplot(sectorData, aes(x = SURVEYR, y = round(indicator100,0), group = INDICATOR_lang)) +
    facet_grid(.~abbr_lang) + 
    geom_line(aes(color = INDICATOR_lang, linetype = abbr_lang), alpha = 0.6, size = 1) +
    scale_colour_brewer(palette = "Set2") +
    geom_text_repel(data = sectorData %>% filter(SURVEYR == 2017), 
                    aes(label = str_wrap(INDICATOR_lang,10), colour = INDICATOR_lang), 
                    hjust = 2, 
                    fontface = "bold", 
                    size = 2,
                    nudge_x = -1, 
                    direction = "y") +
    geom_text_repel(data = sectorData %>% filter(SURVEYR == 2018), 
                    aes(label = str_wrap(INDICATOR_lang,10), colour = INDICATOR_lang), 
                    hjust = -1, 
                    fontface = "bold", 
                    size = 2,
                    nudge_x = 1, 
                    direction = "y") +
    geom_point(colour = "white", size = 8, shape = 16) +
    geom_text(aes(label = round(indicator100,0), y = round(indicator100,0)),
              size = 3, colour = "grey30", fontface = "bold", alpha = 0.8) +
    scale_x_discrete(position = "top", expand = expand_scale(add = 1)) +
    scale_y_continuous(expand = expand_scale(add = 1)) +
    # Reuse theme
    slopeTheme
  
  # Create title grob
  slope.ttl_lang <- case_when(
    lang == "E" ~ "Year-to-Year Comparision (Score 100)",
    lang == "F" ~ "Comparaison annuelle (Score 100)")
  slope.ttl <- textGrob(slope.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  space.grb <- textGrob("")
  
  # Add title to slopechart
  top_left.grb <- plot_grid(slope.ttl,
                         slope.plt,
                         space.grb,
                         nrow = 3,
                         rel_heights = c(1,11.5,0.5))
  
  #----
  ## RIGHT: Create top negative and postive shifts
  
  # Determine the deltas between PSES 2017 and PSES 2018 data
  sectorDeltas <- questionData %>%
    #filter(unitcode %in% c(thisUnitcode, "dept")) %>%
    #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
    select(INDICATORID,INDICATOR_lang,QUESTION,TITLE_lang,
           unitcode,abbr_lang,DESCRIP_lang,SURVEYR,SCORE100,AGREE) %>%
    spread(SURVEYR,SCORE100) %>%
    mutate(delta = `2018` - `2017`)
  
  # Get the the 10 best and the 10 worst deltas
  best10deltas <- filter(sectorDeltas, unitcode == thisUnitcode & delta > 0) %>%
    arrange(desc(delta),`2018`) %>% slice(1:10) %>% select(QUESTION,delta,`2018`)
  worst10deltas <- filter(sectorDeltas, unitcode == thisUnitcode & delta < 0) %>%
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
    theme(text = element_text(colour = "grey30")),
    theme(plot.title       = element_text(size = 8, hjust = 0.5)),
    theme(plot.subtitle    = element_text(hjust = 0.5)),
    # Put facet labels on the left and horizontal
    theme(strip.text.y = element_text(angle = 180, size = 6, hjust = 0)),
    theme(strip.background = element_blank())
    #theme(panel.background = element_rect(fill = "grey95")) +
  )
  
  deltaTheme2 <- list( 
    theme_bw(),
    # Format tweaks
    # Remove the legend
    theme(legend.position = "none"),
    # Remove the panel border
    theme(panel.border     = element_blank()),
    # Remove just about everything from the y axis
    theme(axis.title.y     = element_blank()),
    theme(axis.text.y      = element_text(size = 6, hjust = 0)), 
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
    theme(text = element_text(colour = "grey30")),
    theme(plot.title       = element_text(size = 8, hjust = 0.5)),
    theme(plot.subtitle    = element_text(hjust = 0.5)),
    # Put facet labels on the left and horizontal
    theme(strip.text.y = element_text(angle = 180, size = 6, hjust = 0)),
    theme(strip.background = element_blank())
    #theme(panel.background = element_rect(fill = "grey95")) +
  )
  
  offset2 <- -0.3
  
  # Use the 10 best deltas to build the data for the "Most postive shifts"
  bestData <- sectorDeltas %>%
    inner_join(select(best10deltas,QUESTION), by = "QUESTION") %>%
    filter(unitcode == thisUnitcode) # ORIGINAL "BEST" CHART - Sector only
  
  bestData2 <- sectorDeltas %>%
    inner_join(select(best10deltas,QUESTION), by = "QUESTION") %>%
    mutate(ind_question = paste0(
      INDICATOR_lang, ": Q", substr(TITLE_lang,10,140)
      )) %>%
    mutate(ind_question = str_wrap(ind_question, width = 60)) %>% 
    mutate(org_level = ifelse(unitcode == "dept","dept","sector")) %>% 
    select(ind_question,org_level, y2017 = `2017`, y2018 = `2018`, delta) %>% 
    nest(y2017, y2018, delta, .key = "value_col") %>%
    spread(key = org_level, value = value_col) %>% 
    unnest(sector,dept, .sep = "_") %>% 
    mutate(ind_question = fct_reorder(ind_question, sector_delta, .desc = FALSE))
  
  # Build the "Most postive shifts" chart
  best.plt <- ggplot(data = bestData, x = abbr_lang, group = abbr_lang) +
    geom_col(aes(x = abbr_lang, y = `2018`), fill = "#f7f7f7", width = 0.8) +
    geom_hline(aes(yintercept = `2017`), colour = "grey60", linetype = 3) +
    geom_hline(aes(yintercept = `2018`, colour = delta)) +
    geom_point(aes(x = abbr_lang, y = (`2017` + delta/2), colour = delta), shape = 62, size = 2) +
    geom_text(aes(label = `2017`, x = abbr_lang, y = `2017`),
              size = 3, colour = "grey30", fontface = "plain", hjust = 1.3, vjust = 0.5) +
    geom_text(aes(label = `2018`, x = abbr_lang, y = `2018`),
              size = 3, colour = "grey30", fontface = "bold", hjust = -0.3, vjust = 0.5) +
    geom_text(aes(label = paste0("+",delta), x = abbr_lang, y = 0, colour = delta),
              size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
    coord_flip() +
    facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
                           delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
    scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme
  
  # ORIGINAL "BEST" CHART - Sector only
  best2.plt <- ggplot(data = bestData2, x = ind_question) +
    geom_col(aes(x = ind_question, y = sector_y2018), fill = "#f7f7f7", width = 0.8) +
    geom_errorbar(aes(x = ind_question, ymin = sector_y2017, ymax = sector_y2017), colour = "grey60", linetype = 3) +
    geom_errorbar(aes(x = ind_question, ymin = sector_y2018, ymax = sector_y2018, colour = sector_delta)) +
    geom_segment(aes(x = as.numeric(ind_question) + offset2, xend = as.numeric(ind_question) + offset2, y = dept_y2017, yend = dept_y2018, colour = dept_delta),
                 position = position_dodge(width = 1)) +
    geom_point(aes(x = as.numeric(ind_question) + offset2, y = dept_y2017, colour = dept_delta),
               position = position_dodge(width = 1), shape = 21, fill = "white") +
    geom_point(aes(x = as.numeric(ind_question) + offset2, y = dept_y2018, colour = dept_delta),
               position = position_dodge(width = 1)) +
    geom_point(aes(x = ind_question, y = (sector_y2017 + sector_delta/2),
                   colour = sector_delta), shape = 62, size = 2) +
    geom_text(aes(label = sector_y2017, x = ind_question, y = sector_y2017),
              size = 3, colour = "grey30", fontface = "plain", hjust = 1.3, vjust = 0.5) +
    geom_text(aes(label = sector_y2018, x = ind_question, y = sector_y2018),
              size = 3, colour = "grey30", fontface = "bold", hjust = -0.3, vjust = 0.5) +
    geom_text(aes(label = paste0("+",sector_delta), x = ind_question, y = 0, colour = sector_delta),
              size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
    coord_flip() +
    #facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
    #                       delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
    scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme2

  # Use the 10 worst deltas to build the data for the "Most negative shifts"
  worstData <- sectorDeltas %>%
    inner_join(select(worst10deltas,QUESTION), by = "QUESTION") %>%
    filter(unitcode == thisUnitcode)
  
  worstData2 <- sectorDeltas %>%
    inner_join(select(worst10deltas,QUESTION), by = "QUESTION") %>%
    mutate(ind_question = paste0(
      INDICATOR_lang, ": Q", substr(TITLE_lang,10,140)
    )) %>%
    mutate(ind_question = str_wrap(ind_question, width = 60)) %>% 
    mutate(org_level = ifelse(unitcode == "dept","dept","sector")) %>% 
    select(ind_question,org_level, y2017 = `2017`, y2018 = `2018`, delta) %>% 
    nest(y2017, y2018, delta, .key = "value_col") %>%
    spread(key = org_level, value = value_col) %>% 
    unnest(sector,dept, .sep = "_") %>% 
    mutate(ind_question = fct_reorder(ind_question, sector_delta, .desc = TRUE))
  
  # Build the "Most negative shifts" chart
  worst.plt <- ggplot(data = worstData, x = abbr_lang, group = abbr_lang) +
    geom_col(aes(x = abbr_lang, y = `2018`), fill = "#f7f7f7", width = 0.8) +
    geom_hline(aes(yintercept = `2017`), colour = "grey60", linetype = 3) +
    geom_hline(aes(yintercept = `2018`, colour = delta)) +
    geom_point(aes(x = abbr_lang, y = (`2017` + delta/2), colour = delta), shape = 60, size = 2) +
    geom_text(aes(label = `2017`, x = abbr_lang, y = `2017`),
              size = 3, colour = "grey30", fontface = "plain", hjust = -0.3, vjust = 0.5) +
    geom_text(aes(label = `2018`, x = abbr_lang, y = `2018`),
              size = 3, colour = "grey30", fontface = "bold", hjust = 1.3, vjust = 0.5) +
    geom_text(aes(label = paste0(delta), x = abbr_lang, y = 0, colour = delta),
              size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
    coord_flip() +
    facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
                           delta)~.,switch = "y", labeller = label_wrap_gen(60)) +
    scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme
  
  worst2.plt <- ggplot(data = worstData2, x = ind_question) +
    geom_col(aes(x = ind_question, y = sector_y2018), fill = "#f7f7f7", width = 0.8) +
    geom_errorbar(aes(x = ind_question, ymin = sector_y2017, ymax = sector_y2017), colour = "grey60", linetype = 3) +
    geom_errorbar(aes(x = ind_question, ymin = sector_y2018, ymax = sector_y2018, colour = sector_delta)) +
    geom_segment(aes(x = as.numeric(ind_question) + offset2, xend = as.numeric(ind_question) + offset2, y = dept_y2017, yend = dept_y2018, colour = dept_delta),
                 position = position_dodge(width = 1)) +
    geom_point(aes(x = as.numeric(ind_question) + offset2, y = dept_y2017, colour = dept_delta),
               position = position_dodge(width = 1), shape = 21, fill = "white") +
    geom_point(aes(x = as.numeric(ind_question) + offset2, y = dept_y2018, colour = dept_delta),
               position = position_dodge(width = 1)) +
    geom_point(aes(x = ind_question, y = (sector_y2017 + sector_delta/2),
                   colour = sector_delta), shape = 60, size = 2) +
    geom_text(aes(label = sector_y2017, x = ind_question, y = sector_y2017),
              size = 3, colour = "grey30", fontface = "plain", hjust = -0.3, vjust = 0.5) +
    geom_text(aes(label = sector_y2018, x = ind_question, y = sector_y2018),
              size = 3, colour = "grey30", fontface = "bold", hjust = 1.3, vjust = 0.5) +
    geom_text(aes(label = paste0(sector_delta), x = ind_question, y = 0, colour = sector_delta),
              size = 3, fontface = "bold", hjust = 0, vjust = 0.5) +
    coord_flip() +
    #facet_grid(fct_reorder(paste0(INDICATOR_lang,": Q",substr(TITLE_lang,10,140)),
    #                       delta,.desc=TRUE)~.,switch = "y", labeller = label_wrap_gen(60)) +
    scale_colour_gradient2(high = "#0571b0", mid = "#bcbcbc", low = "#ca0020") +
    scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100), expand = expand_scale(add = c(0,5))) +
    deltaTheme2
  
  # Create chart titles
  best.ttl_lang <- case_when(
    lang == "E" ~ paste0("Top 10 Positive Shifts for ",thisAbbr," from 2017 to 2018 (Score 100)"),
    lang == "F" ~ paste0("Les 10 changements plus positifs pour ",thisAbbr," de 2017 à 2018 (Score 100)"))
  best.ttl <- textGrob(best.ttl_lang,
                       gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  worst.ttl_lang <- case_when(
    lang == "E" ~ paste0("Top 10 Negative Shifts for ",thisAbbr," from 2017 to 2018 (Score 100)"),
    lang == "F" ~ paste0("Les 10 changements plus négatifs pour ",thisAbbr," de 2017 à 2018 (Score 100)"))
  worst.ttl <- textGrob(worst.ttl_lang,
                        gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  # Combine the charts and title into the right-hand pane of the report card
  right.grb <- plot_grid(best.ttl,
                         best2.plt,
                         worst.ttl,
                         worst2.plt,
                         nrow = 4, rel_heights = c(1,12,1,12))
  
  #----
  ## BOTTOM-LEFT: Create the harassment and discrimination charts
  
  # Extract the harassment and discrimination questions using the appropriate subindicators (12 and 13)
  sectorHarDis <- questionData %>%
    filter(SUBINDICATORID %in% c(12,13) & SURVEYR == 2018) %>%
    expand(QUESTION,unitcode) %>%
    left_join(distinct(select(questionData,unitcode,abbr_lang)), by = "unitcode") %>%
    left_join(distinct(select(questionData,QUESTION,TITLE_lang)), by = "QUESTION") %>%
    left_join(filter(questionData, SURVEYR == 2018), by = c("QUESTION","unitcode","abbr_lang","TITLE_lang")) %>%
    select(QUESTION,TITLE_lang,unitcode,abbr_lang,AGREE,agree_2017,ANSCOUNT) %>%
    #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
    mutate(delta = AGREE - agree_2017) %>%
    rename(`2017` = agree_2017, `2018` = AGREE)
    
    #mutate(i = row_number()) %>%
    #spread(unitcode,QUESTION) %>%
    #gather(key = "unitcode", value = "QUESTION", -i) %>%
    #select(-i) %>%
    
    
    #mutate(abbr_lang = ifelse(unitcode == thisUnitcode, thisAbbr, unitcode)) %>%
    #select(QUESTION,TITLE_lang,unitcode,abbr_lang,SURVEYR,AGREE,agree_2017) %>%
    #mutate(delta = AGREE - agree_2017) %>%
    #rename(`2017` = agree_2017, `2018` = AGREE)

  # Create the harassment and discrimination titles
  
  har.ttl_lang <- case_when(
    lang == "E" ~ "Victims of Harassment (%)",
    lang == "F" ~ "Victimes de harcèlement (%)")
  har.ttl <- textGrob(har.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  dis.ttl_lang <- case_when(
    lang == "E" ~ "Victims of Discrimination (%)",
    lang == "F" ~ "Victimes de discrimination (%)")
  dis.ttl <- textGrob(dis.ttl_lang, gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  har_dis.ttl <- plot_grid(har.ttl,dis.ttl)
  
  # Create a single harassment and discrimination plot showing the rates of each (Q48 and Q55)
  #har_dis.plt <- ggplot(data = filter(sectorHarDis, QUESTION %in% c("Q48","Q55")), aes(x = abbr_lang)) +
  #  geom_text(aes(label = paste0(abbr_lang,": ",`2018`,"%"),y=1, colour = QUESTION), fontface = "bold.italic", size = 4) +
  #  facet_grid(.~str_wrap(substr(TITLE_lang,10,200),50)) +
  #  scale_x_discrete(position = "bottom") +
  #  scale_colour_manual(values = c("#7fc97f","#beaed4")) +
  #  deltaTheme +
  #  theme(axis.text = element_blank()) +
  #  theme(strip.text = element_text(size = 7))
  
  suppressed <- 5/thisAnscount
  
  har_dis <- sectorHarDis %>%
    filter(QUESTION %in% c("Q48","Q55")) %>%
    select(QUESTION,abbr_lang,`2017`,`2018`) %>%
    gather("SURVEYR","AGREE",-QUESTION,-abbr_lang) %>%
    mutate(AGREE = ifelse(is.na(AGREE),suppressed,AGREE))
  
  har_dis.plt <- ggplot(har_dis, aes(x = SURVEYR, y = AGREE, group = abbr_lang)) +
    facet_wrap(~QUESTION, scales = "free_y") + 
    geom_line(data = har_dis,
              aes(linetype = abbr_lang, colour = QUESTION, alpha = abbr_lang), size = 1) +
    scale_colour_manual(values = c("#7fc97f","#beaed4")) +
    scale_alpha_manual(values = c(1,0.5)) +
    geom_text_repel(data = har_dis %>% filter(SURVEYR == 2017), 
                    aes(label = abbr_lang, colour = QUESTION, alpha = abbr_lang), 
                    hjust = 2, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = -1, 
                    direction = "y") +
    geom_text_repel(data = har_dis %>% filter(SURVEYR == 2018), 
                    aes(label = abbr_lang, colour = QUESTION, alpha = abbr_lang), 
                    hjust = -1, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = 1, 
                    direction = "y") +
    geom_point(colour = "white", size = 8, shape = 16) +
    geom_text(aes(label = ifelse(AGREE == suppressed,"n<6",AGREE), y = AGREE),
              size = 3, colour = "grey30", fontface = "bold", alpha = 0.8) +
    scale_x_discrete(position = "top", expand = expand_scale(add = 1)) +
    scale_y_continuous(expand = expand_scale(add = 1)) +
    # Reuse theme
    slopeTheme +
    theme(strip.text = element_blank())
  
  # Extract the data on the nature of harassment
  harNatureData <- sectorHarDis %>%
    filter(startsWith(QUESTION,"Q50")) %>%
    mutate(Qshort_lang = word(TITLE_lang,3, sep = fixed('.'))) %>%
    mutate(`2018` = ifelse(is.na(`2018`),0.5,`2018`)) %>%
    arrange(`2018`) %>%
    mutate(order = ifelse(unitcode == "dept", row_number(), NA))
  
  # Create the the nature of harassment chart
  harNature.plt <- ggplot(harNatureData, 
                          aes(x=fct_reorder(str_wrap(substr(Qshort_lang,1,30),30), order, fun = max, na.rm = TRUE), y=`2018`)) +
    labs(
      x= case_when(lang == "E" ~ "Sectors", lang == "F" ~  "Secteurs"),
      y= case_when(lang == "E" ~ "% answering yes", lang == "F" ~  "% répondant oui")) +
    geom_col(aes(alpha = abbr_lang), fill = "#7fc97f") +
    geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
              aes(label=ifelse(`2018` == 0.5,paste0("n<6"),`2018`), y=0)) +
    coord_flip() +
    facet_grid(.~abbr_lang) +
    scale_alpha_manual(values = c(1,.5)) +
    deltaTheme +
    theme(axis.text.y = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.text.x = element_blank()) +
    theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))
  
  # Extract the data on the type of discrimination
  disTypeData <- sectorHarDis %>%
    filter(startsWith(QUESTION,"Q57")) %>%
    mutate(Qshort_lang = word(TITLE_lang,3, sep = fixed('.'))) %>%
    mutate(`2018` = ifelse(is.na(`2018`),0.5,`2018`)) %>%
    arrange(`2018`) %>%
    mutate(order = ifelse(unitcode == "dept", row_number(), NA))
  
  # Create the type of discrimination chart
  disType.plt <- ggplot(disTypeData,
                        aes(x=fct_reorder(str_wrap(substr(Qshort_lang,1,30),30), order, fun = max, na.rm = TRUE), y=`2018`)) +
    labs(
      x= case_when(lang == "E" ~ "Sectors", lang == "F" ~  "Secteurs"),
      y= case_when(lang == "E" ~ "% answering yes", lang == "F" ~  "% répondant oui")) +
    geom_col(aes(alpha = abbr_lang), fill = "#beaed4") +
    geom_text(hjust=-0.1, vjust=0.5, size=3, colour="grey30", fontface = "bold", 
              aes(label=ifelse(`2018` == 0.5,paste0("n<6"),`2018`), y=0)) +
    coord_flip() +
    facet_grid(.~abbr_lang) +
    scale_alpha_manual(values = c(1,.5)) +
    deltaTheme +
    theme(axis.text.y = element_text(size = 6)) +
    theme(axis.title.x = element_text(size = 8)) +
    theme(axis.text.x = element_blank()) +
    theme(strip.text = element_text(colour = "grey30", face = "italic", size = 8))
   
  # Create nature of harassment and type of discrimination titles
  harNature.ttl_lang <- case_when(
    lang == "E" ~ "Nature of Harassment (2018)",
    lang == "F" ~ "Nature du harcèlement (2018)")
  harNature.ttl <- textGrob(harNature.ttl_lang,
                     gp = gpar(fontsize = 10, fontface = "bold", col = "grey30"))
  
  distype.ttl_lang <- case_when(
    lang == "E" ~ "Type of Discrimination (2018)",
    lang == "F" ~ "Type de discrimination (2018)")
  disType.ttl <- textGrob(distype.ttl_lang,
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
  top.grb <- textGrob(ttl_lang, x = unit(0.01, "npc"), just= "left",
                           gp = gpar(fontsize = 14, fontface = "bold", col = "grey30"))
  
  # Combine the left- and right-hand panes into a single grob, then add the title on top. All done!
  bottom.grb <- plot_grid(left.grb,right.grb)
  report.grb <- plot_grid(top.grb,
                           bottom.grb,
                           nrow=2,rel_heights = c(1,20))
  
  howto.ttl_lang <- case_when(
    lang == "E" ~ "\nHow to read this \nreport card",
    lang == "F" ~ "\nComment lire \nce bulletin"
  )

  limits.ttl_lang <- case_when(
    lang == "E" ~ "\nLimitations",
    lang == "F" ~ "\nLimitations"
  )

  howto.ttl <- textGrob(howto.ttl_lang, hjust = 0.5, gp=gpar(fontsize=12, col ="grey30", fontface = "bold"))
  
  limits.ttl <- textGrob(limits.ttl_lang, hjust = 0.5, gp=gpar(fontsize=12, col ="grey30", fontface = "bold"))
  
  if (lang == "E") {
  
  howto.txt <- textGrob(paste0("
This PSES \"report card\" is made up of panels summarizing
differences between the 2017 and 2018 PSES and between the 
TBS average and the ",thisSectorName,".\n",
"The top-left panel shows differences between groupings of
PSES questions by theme - \"indicators\". The right panel
shows the top 10 most positive and negative shifts in scores.\n 
The score is based on the \"Score 100\" measure for each question.
It is the average of question responses using these weights: 
Very Positive = 100,
Positive = 75,
Neutral = 50,
Negative = 25,
Very Negative = 0.\n
Example:
A score of 52 for \"Employee Engagement\" means that the
average \"Score 100\" of the questions under that theme
(5, 9, 10, 14, 43, 44 and 45)
for that sector was 52 out of 100 - closest to \"Neutral\".\n
Note high scores are always positive and 
low scores are always negative.\n
Example: 
For Q16,\"I feel that the quality of my work suffers because of...\", 
a score of 100 means that the work almost never suffers, while
a score of 0 indicates it almost always suffers.\n
The bottom-left panel shows a summary of discrimination and 
harassment rates and the relevant nature and type. Note that some
data is suppressed when respondents are 5 or less. In that case,
they are replaced with \"n<6\".
                            "),
                            hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
  
  limits.txt <- textGrob("
Not all responses rates were the same for all sectors
(HRD has this available).\n
The \"Score 100\" measure does not reflect the distribution of
responses. Example:
50 people scoring very positive and
50 people scoring very negative
(50%*100 + 50%*0 = 50)
is identical to 100 people
scoring neutral
(100%*50 = 50).\n
The \"Score 100\" measure makes assumptions based on its
weighting (described above).\n
The \"indicators\" are not stable - PSES questions change from year to year
and \"Indicator\" questions are weighted equally.\n
The harassment and discrimination rates were calculated on 
\"the last 24 months\" in 2017 and on \"the last 12 months\" in 2018.
In cases where the rate was suppressed, only TBS data is presented 
on harassment nature and discrimination type.
                            ",
                            hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
  } else if (lang == "F") {
    
    howto.txt <- textGrob(paste0("
Ce « bulletin » du SAFF se compose de panneaux résumant les
différences entre le SAFF de 2017 et celui de 2018 et entre la
moyenne du SCT et celle du ", thisSectorName,".\n ",
"Le panneau en haut à gauche montre les différences entre les groupes de
Questions sur le SAFF par thème, soit « indicateurs. » Le panneau de droite
affiche les 10 changements les plus positifs et les plus négatifs des scores.\n
Le score est basé sur la mesure « Score 100 » pour chaque question.
C'est la moyenne des réponses aux questions utilisant ces poids:
Très positif = 100,
Positif = 75,
Neutre = 50,
Négatif = 25,
Très négatif = 0.\n
Exemple:
Un score de 52 pour « Engagement des employés » signifie que la
moyenne « Score 100 » des questions de ce thème
(5, 9, 10, 14, 43, 44 et 45)
pour ce secteur était de 52 sur 100; plus proche de « neutre.»\n
Notez que les scores élevés sont toujours positifs et les scores faibles,
toujours négatif.\n
Exemple:
Pour la Q16, 
« Je pense que la qualité de mon travail en souffre à cause de ... »
un score de 100 signifie que le travail ne souffre presque jamais,
alors qu'un score de 0 indique qu'il en souffre presque toujours.\n
Le panneau en bas à gauche montre un résumé des taux de discrimination
et de harcèlement et la nature et le type pertinents. 
Notez que certainesdonnées sont supprimées lorsque les répondants 
sont 5 ou moins. Dans ce cas, ils sont remplacés par « n<6.»
                            "),
                          hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
    
    limits.txt <- textGrob("
Les taux de réponse n'étaient pas tous les mêmes pour tous 
les secteurs (ils sont disponibles chez la DRH).\n
La mesure « Score 100 » ne reflète pas la distribution
des réponses. Exemple:
50 personnes marquant très positif et
50 personnes marquant très négatif
(50% * 100 + 50% * 0 = 50)
est identique à 100 personnes
marquant neutre
(100% * 50 = 50).\n
La mesure « Score 100 » fait des hypothèses basées sur sa
pondération (décrite ci-dessus).\n
Les « indicateurs » ne sont pas stables, puisque les questions
du SAFF changent d'année en année, et les questions qui les 
constituent sont pondérées également.\n
Les taux de harcèlement et de discrimination ont été calculés sur
« Les 24 derniers mois » en 2017 et
« les 12 derniers mois » en 2018.
Dans les cas où le taux a été supprimé, seules les données du SCT
sont présentées sur le harcèlement et le type de discrimination.
                            ",
                           hjust = 0.5, gp=gpar(fontsize=6, col ="grey30"))
    
  }
  
  
  descrip.grb <- plot_grid(howto.ttl,howto.txt,limits.ttl,limits.txt, ncol = 1, align = "v", rel_heights = c(1,10,1,7))
  
  report_card.plt <- plot_grid(report.grb,descrip.grb, rel_widths = c(11,3))
  
  
  
  rc_filename <- file.path(mainDir,plotDir,paste0(ttl_lang,".pdf"))
  
  # Save the report card into a PDF
  #ggsave(rc_filename, plot = report_card.plt, height = 8.5, width = 14)
  
  #return(rc_filename)
  return(report_card.plt)
}


#----
### RUN REPORT CARDS

# These are all of TBS's unit codes
#c(200,201,202,301,302,303,400,401,402,403,404,304,405,406,407,408,305,306,307,308,309,310,311,312,313,314,315,203)

# Select all TBS sectors, except CIOB (use OCIO below, CDS (because there is no 2017 comparator) and "I cannot fonmd my sector"
sectorList <- distinct(score100s, unitcode, DESCRIP_E) %>% filter(!unitcode %in% c("300","301","999")) # Exclude CDS, CIOB and NA

#for (i in sectorList$unitcode) { report_card(i, "E", question100s = question100s, score100s = score100s) }
#for (i in sectorList$unitcode) { report_card(i, "F", question100s = question100s, score100s = score100s) }

for (i in sectorList$unitcode) { 
  # Get sector abbreviations and construct a filename
  #sector_abbr_e <- as.character(score100s$abbr_E[score100s$unitcode==i])
  #sector_abbr_f <- as.character(score100s$abbr_F[score100s$unitcode==i])
  #rc_filename <- paste0("PSES_SAFF2018 - ",sector_abbr_e,"_",sector_abbr_f,".pdf")
  
  sector_name <- as.character(score100s$DESCRIP_E[score100s$unitcode==i])
  rc_filename <- paste0("PSES2018 Report Cards (EN&FR) - ",sector_name,".pdf")
  
  rc_e <- report_card(i, "E", question100s = question100s, score100s = score100s) 
  rc_f <- report_card(i, "F", question100s = question100s, score100s = score100s)
  
  # Make bilingual pdf two-pager
  pdf(file.path(mainDir,plotDir,rc_filename),height = 8.5, width = 14,
      useDingbats=FALSE)
  #pdf(file.path(mainDir,plotDir,paste0(i," test.pdf")),height = 8.5, width = 14)
  print(rc_e)
  print(rc_f)
  dev.off()
}

# CIOB has been rechristened OCIO, following elevation of the CIO to DM status.
# These lines ensure the new OCIO descriptors are used for CIOB's unitcode (301).
pdf(file.path(mainDir,plotDir,"PSES2018 Report Cards (EN&FR) - Office of the Chief Information Officer.pdf"),
    height = 8.5, width = 14, useDingbats=FALSE)
report_card(301, "E", "Office of the Chief Information Officer", "OCIO", question100s, score100s)
report_card(301, "F", "Bureau du Dirigeant Principal de l'information", "BDPI", question100s, score100s)
dev.off()

# GOS - 307
pdf(file.path(mainDir,plotDir,"PSES2018 Report Cards (EN&FR) - Government Operations Sector.pdf"),
    height = 8.5, width = 14, useDingbats=FALSE)
report_card(307, "E", question100s = question100s, score100s = score100s)
report_card(307, "F", question100s = question100s, score100s = score100s)
dev.off()
