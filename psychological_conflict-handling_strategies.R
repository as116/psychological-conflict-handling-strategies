########### 
#### Code for Publication
####
#### Annika Stampf, Mark Colley, Ann-Kathrin Knuth, Cagla Tasci, and Enrico Rukzio. 2024. 
#### Examining Psychological Conflict-Handling Strategies for Highly Automated Vehicles to Resolve Legal User-Vehicle Conflicts
#### 

library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))
library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


ggPlotWithoutStats <- function(data, x, y, ylab, xlabels, min=1, max=5, steps=1, paired=FALSE, colors ) {
  assertthat::not_empty(data)
  assertthat::not_empty(x)
  assertthat::not_empty(y)
  assertthat::not_empty(ylab)
  assertthat::not_empty(xlabels)
  
  normality_test <- with(data, tapply(data[[y]], data[[x]], shapiro.test))
  normallyDistributed <- TRUE
  for (i in normality_test) {
    if (!is.null(i)) {
      if (i$p.value < 0.05) {
        normallyDistributed <- FALSE
        break
      }
    }
  }
  
  type <- ifelse(normallyDistributed, "p", "np")
  
  df <- pairwise_comparisons(data = data, x = !!x, y = !!y, type = type, paired = paired, p.adjust.method = "holm") %>%
    dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c)) %>%
    dplyr::arrange(group1) %>%
    dplyr::mutate(asterisk_label = ifelse(`p.value` < 0.05 & `p.value` > 0.01, "*", ifelse(`p.value` < 0.01 & `p.value` > 0.001, "**", ifelse(`p.value` < 0.001, "***", NA))))
  
  df <- df %>% dplyr::filter(!is.na(asterisk_label))
  
  lowestNumberText <- paste0("NA=0.0; else=", toString(round((max(data[[y]]) + 0.5), digits = 2)))
  y_positions_asterisks <- recode(df$asterisk_label, recodes = lowestNumberText)
  
  count <- 0
  for (i in 1:length(y_positions_asterisks)) {
    if(y_positions_asterisks[i] != 0.0){
      y_positions_asterisks[i] <- y_positions_asterisks[i] + count * 0.4
      count = count+1
    }
  }
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_boxplot() +  # Default outline color for boxplot
    geom_point(aes(color = factor(data[[x]]), alpha = 0.7), show.legend = FALSE) +  # Set point color and transparency based on 'x' and remove legend
    scale_x_discrete(labels = xlabels) +
    scale_fill_manual(values = colors) +
    theme(panel.border = element_blank(), 
          text = element_text(size = 18), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.subtitle = element_text(size = 12, face = "bold")) +
    scale_y_continuous(breaks = seq(min, max, steps), limits = c(min, max)) +
    labs(y = ylab, x = "") +   stat_summary(fun = "mean", geom = "point", shape = 18, stroke = 1,
                                            size = 3, color = "#222222")+
    scale_color_manual(values = colors) +  # Set point colors using 'tol' palette
    scale_alpha_continuous(range = c(0.1, 1))  # Adjust the range as needed for transparency
  p
  
  
}

Conditions <- read_xlsx(path = "StudyResults.xlsx", sheet = "Blatt 1")
Conditions <- as.data.frame(Conditions)
names(Conditions)

Conditions$ID <- factor(Conditions$ID)
Conditions$Condition <- factor(Conditions$Condition)

colorsconditions <- c("#CCCCCC", "#A6CEE3", "#A6CEE3", "#EE7674", "#EE7674",  "#EE7674",  "#94C9A9",  "#94C9A9","#94C9A9" ,"#94C9A9", "#C6ECAE", "#C6ECAE");
colorsvalence <- c("#CCCCCC", "#EE7674", "#A6CEE3", "#B2DF8A");
colorsmechanism <- c("#CCCCCC", "#A6CEE3","#C6ECAE", "#94C9A9","#EE7674");

get_values_based_on_condition <- function(condition) {
  if (condition == 1) {
    return(c("Baseline", "Baseline", "Baseline"))
  } else if (condition == 2) {
    return(c("Explanation", "Cognitive", "Neutral"))
  } else if (condition == 3) {
    return(c("Show benefit", "Cognitive", "Neutral"))
  } else if (condition == 4) {
    return(c("Annoyance", "Social", "Negative"))
  } else if (condition == 5) {
    return(c("Command", "Social", "Negative"))
  } else if (condition == 6) {
    return(c("Threat", "Social", "Negative"))
  } else if (condition == 7) {
    return(c("Appeal", "Politeness", "Positive"))
  } else if (condition == 8) {
    return(c("Thanking", "Politeness", "Positive"))
  } else if (condition == 9) {
    return(c("Apologize", "Politeness", "Positive"))
  } else if (condition == 10) {
    return(c("Humorous", "Emotional", "Positive"))
  } else if (condition == 11) {
    return(c("Trigger empathy", "Emotional", "Positive"))
  } else if (condition == 12) {
    return(c("Thanking submissive", "Politeness", "Positive"))
  } else {
    # Default return in case of an unexpected condition value
    return(c(NA, NA, NA))
  }
}

Conditions$Strategy <- NA
Conditions$Mechanism <- NA
Conditions$Valence <- NA
for (i in 1:nrow(Conditions)) {
  condition_value <- Conditions$Condition[i]
  values <- get_values_based_on_condition(condition_value)
  Conditions$Strategy[i] <- values[1]
  Conditions$Mechanism[i] <- values[2]
  Conditions$Valence[i] <- values[3]
}
Conditions$Strategy <- factor(Conditions$Strategy, levels= c("Baseline", "Explanation", "Show benefit", "Annoyance", "Command", "Threat", "Appeal", "Thanking", "Apologize", "Thanking submissive",  "Humorous", "Trigger empathy"))
Conditions$Valence <- factor(Conditions$Valence)
Conditions$Mechanism <- factor(Conditions$Mechanism)

####Strategies###
###Conflict
levels_vec = c("Do not agree at all", 
               "Rather disagree",
               "Neither",
               "Rather agree",
               "Fully agree")
labels_vec = 1:5
for (i in 1:5) {
  column_name <- paste0("C", i)
  Conditions[[column_name]] <- as.numeric(factor(Conditions[[column_name]], 
                                                 levels = levels_vec, 
                                                 labels = labels_vec))
}
Conditions$C2_inv <- (Conditions$C2 * -1) + 6
Conditions$Conflict <- rowSums(Conditions[,c("C1", "C2_inv", "C3", "C4", "C5")])/5.0
ggwithinstatsWithPriorNormalityCheck(data = Conditions, x = "Strategy", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"))
a <- friedmanTest(y=Conditions$Conflict, groups=Conditions$Strategy, blocks=Conditions$ID)
a
d <- dunnTest(Conflict ~ Strategy, data= Conditions, method="holm")
d
group_by(Conditions, Strategy) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(Conflict, na.rm = TRUE),
    sd = sd(Conflict, na.rm = TRUE)
  )

###Trust
levels_vec <- c("Strongly disagree", 
                "Disagree",
                "Mildly disagree",
                "Neutral",
                "Mildly agree",
                "Agree",
                "Strongly agree")
labels_vec <- 1:7
for (i in 1:6) {
  column_name <- paste0("ST", i)
  Conditions[[column_name]] <- as.numeric(factor(Conditions[[column_name]], 
                                                 levels = levels_vec, 
                                                 labels = labels_vec))
}
Conditions$ST2_inv <- (Conditions$ST2 * -1) + 8
Conditions$ST4_inv <- (Conditions$ST4 * -1) + 8
Conditions$ST5_inv <- (Conditions$ST5 * -1) + 8
Conditions$Trust <- rowSums(Conditions[,c("ST1", "ST2_inv", "ST3", "ST4_inv", "ST5_inv", "ST6")])/6.0
ggwithinstatsWithPriorNormalityCheck(data = Conditions, x = "Strategy", y = "Trust", ylab = "STS-AD", xlabels = c("1" = "1"))
a <- friedmanTest(y=Conditions$Trust, groups=Conditions$Strategy, blocks=Conditions$ID)
a
d <- dunnTest(Trust ~ Strategy, data= Conditions, method="holm")
d
group_by(Conditions, Strategy) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(Trust, na.rm = TRUE),
    sd = sd(Trust, na.rm = TRUE)
  )

###Own Items
levels_vec = c("Stimme gar nicht zu", 
               "Stimme eher nicht zu",
               "Weder noch",
               "Stimme eher zu",
               "Stimme voll zu")
labels_vec = 1:5
for (i in 1:4) {
  column_name <- paste0("A", i)
  Conditions[[column_name]] <- as.numeric(factor(Conditions[[column_name]], 
                                                 levels = levels_vec, 
                                                 labels = labels_vec))
}

## Task Feasibility
Conditions$A2_inv <- (Conditions$A2 * -1) + 6
Conditions$Feasibility <- rowSums(Conditions[,c("A1", "A2_inv")])/2.0
ggwithinstatsWithPriorNormalityCheck(data = Conditions, x = "Strategy", y = "Feasibility", ylab = "Task Feasibility", xlabels = c("1" = "1"))
d <- dunnTest(Feasibility ~ Strategy, data= Conditions, method="holm")
d

##Vehicle Behavior Effects Decision
ggwithinstatsWithPriorNormalityCheck(data = Conditions, x = "Condition", y = "A3", ylab = "Influence Vehicle Behavior", xlabels = c("1" = "1"))
d <- dunnTest(A3 ~ Strategy, data= Conditions, method="holm")
d

##Acceptance
ggwithinstatsWithPriorNormalityCheck(data = Conditions, x = "Strategy", y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"))
a <- friedmanTest(y=Conditions$A4, groups=Conditions$Strategy, blocks=Conditions$ID)
a
d <- dunnTest(A4 ~ Strategy, data= Conditions, method="holm")
d

#####Valence and Mechanism Based####
variables_to_average <- c('Conflict', 'Trust', 'Feasibility', 'A3', 'A4')
averaged_data_valence <- Conditions %>%
  group_by(ID, Valence) %>%
  summarise_at(vars(all_of(variables_to_average)), mean, na.rm = TRUE) %>% 
  ungroup()
averaged_data_mechanism <- Conditions %>%
  group_by(ID, Mechanism) %>%
  summarise_at(vars(all_of(variables_to_average)), mean, na.rm = TRUE) %>% 
  ungroup()

###Valence
##Conflict 
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_valence, x = "Valence", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"))
d <- pairwise.t.test(averaged_data_valence$Conflict, averaged_data_valence$Valence, p.adjust.method="holm", paired = TRUE)
d


##Trust
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_valence, x = "Valence", y = "Trust", ylab = "Trust", xlabels = c("1" = "1"))
d <- dunnTest(Trust ~ Valence, data= averaged_data_valence, method="holm")
d


###A3
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_valence, x = "Valence", y = "A3", ylab = "Influence of vehicle on decision", xlabels = c("1" = "1"))
a <- friedmanTest(y=averaged_data_valence$A3, groups=averaged_data_valence$Valence, blocks=averaged_data_valence$ID)
a
d <- dunnTest(A3 ~ Valence, data= averaged_data_valence, method="holm")
d


###A4 Acceptance
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_valence, x = "Valence", y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"))
ggPlotWithoutStats(data = averaged_data_valence, x = "Valence", y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"))
a <- friedmanTest(y=averaged_data_valence$A4, groups=averaged_data_valence$Valence, blocks=averaged_data_valence$ID)
a
d <- dunnTest(A4 ~ Valence, data= averaged_data_valence, method="holm")
d


####Mechanism
###Conflict 
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_mechanism, x = "Mechanism", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"))
a <- friedmanTest(y=averaged_data_mechanism$Conflict, groups=averaged_data_mechanism$Mechanism, blocks=averaged_data_mechanism$ID)
a
d <- dunnTest(Conflict ~ Mechanism, data= averaged_data_mechanism, method="holm")
d

##Trust
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_mechanism, x = "Mechanism", y = "Trust", ylab = "Trust", xlabels = c("1" = "1"))
d <- pairwise.t.test(averaged_data_mechanism$Trust, averaged_data_mechanism$Mechanism, p.adjust.method="holm", paired = TRUE)
d

##A3
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_mechanism, x = "Mechanism", y = "A3", ylab = "Influence of other vehicles on decision", xlabels = c("1" = "1"))
a <- friedmanTest(y=averaged_data_mechanism$A3, groups=averaged_data_mechanism$Mechanism, blocks=averaged_data_mechanism$ID)
a
d <- dunnTest(A3 ~ Mechanism, data= averaged_data_mechanism, method="holm")
d

##A4 Acceptance
ggwithinstatsWithPriorNormalityCheck(data = averaged_data_mechanism, x = "Mechanism", y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"))
ggPlotWithoutStats(data = averaged_data_mechanism, x = "Mechanism", y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"))
a <- friedmanTest(y=averaged_data_mechanism$A4, groups=averaged_data_mechanism$Mechanism, blocks=averaged_data_mechanism$ID)  
a
d <- dunnTest(A4 ~ Mechanism, data= averaged_data_mechanism, method="holm")
d


####Plots###

##Conflict
ggPlotWithoutStats(data = Conditions, min=1, max=5, x = "Strategy", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"), colors = colorsconditions)
ggPlotWithoutStats(data = averaged_data_valence, min=1, max=5, x = "Valence", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"), colors = colorsvalence)
ggPlotWithoutStats(data = averaged_data_mechanism, min=1, max=5, x = "Mechanism", y = "Conflict", ylab = "Conflict", xlabels = c("1" = "1"), colors = colorsmechanism)

##Trust
ggPlotWithoutStats(data = Conditions, x = "Strategy", y = "Trust", max=7, ylab = "STS-AD", xlabels = c("1" = "1"), colors = colorsconditions)
ggPlotWithoutStats(data = averaged_data_valence, x = "Valence", max=7, paired = TRUE, y = "Trust", ylab = "STS-AD",  xlabels = c("1" = "1"), colors = colorsvalence)
ggPlotWithoutStats(data = averaged_data_mechanism, x = "Mechanism", max=7, paired = TRUE,  y = "Trust", ylab = "STS-AD", xlabels = c("1" = "1"), colors = colorsmechanism)

##Acceptance
ggPlotWithoutStats(data = Conditions,  min=1, max=5, x = "Strategy", y = "A4", paired=FALSE, ylab = "Acceptance", xlabels = c("1" = "1"), colors = colorsconditions)
ggPlotWithoutStats(data = averaged_data_valence,  min=1, max=5, x = "Valence", paired=FALSE, y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"), colors = colorsvalence)
ggPlotWithoutStats(data = averaged_data_mechanism,  min=1, max=5, x = "Mechanism", paired=FALSE, y = "A4", ylab = "Acceptance", xlabels = c("1" = "1"), colors = colorsmechanism)

#####Reporting statistics###
##Tables
library(reporttools)
vars <- Conditions[,c('Conflict','Trust', 'A4')]
Conditions$ConditionString <- paste(Conditions$Strategy)
group <- Conditions[,c('ConditionString')]
tableContinuous(vars = vars, group = group, prec = 2, cap = "Table of scores.", lab = "tab:_descr_stat",  stats = c("min", "q1", "median", "mean", "q3", "max",  "s", "iqr"))
