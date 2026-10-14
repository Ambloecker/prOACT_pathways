library(readxl)
library(patchwork)
library(tidyverse)
PrOACT_Spaceparti_table <- read_excel("Data/PrOACT_table.xlsx")
dat <- PrOACT_Spaceparti_table


#calculate score----
#Methods followed in Christie et al. 2018
#scaling: (score - lowest score)/(highest score - lowest score; Gregory et al. 2012 - Structured Decision Making: A Practical Guide to Environmental Management Choices).
#Scores were multiplied by the predefined weightings assigned by the participants, and a 
#total score was calculated based on the sum of the three scores for each safety strategy 
#alternative

#scale the values from 0-1
dat_scale <- dat %>% 
  reframe("Reduce fertilization" = (dat$reduce_fertilization--3)/(3--3),
          "Riparian strips" = (dat$Riparian_strips--3)/(3--3),
          "GES" = (dat$GES --3)/(3--3),
          "EBFM" = (dat$EBFM--3)/(3--3),
          "Non-invasive monitoring" = (dat$non_invasive_monitoring--3)/(3--3),
          "Fisheries monitoring" = (dat$fisheries_monitoring--3)/(3--3),
          "ICES advice system" = (dat$ICES_advice_system--3)/(3--3),
          "Reduce vessels" = (dat$reduce_vessels--3)/(3--3),
          "New quota distribution" = (dat$new_quota_distribution--3)/(3--3),
          "Ban trawling" = (dat$ban_trawling--3)/(3--3),
          "Educational programs" = (dat$educational_programs--3)/(3--3),
          "Environmentally friendly gear" = (dat$environm_friendly_gear--3)/(3--3),
          "Sea Ranger" = (dat$searanger--3)/(3--3),
          "Subsidies" = (dat$subsidies--3)/(3--3),
          "Diversification" = (dat$diversification--3)/(3--3),
          "Re-training" = (dat$re_training--3)/(3--3),
          "MPAs" = (dat$MPAs--3)/(3--3),
          "No take areas" = (dat$no_take_areas--3)/(3--3),
          "Promote tourism" = (dat$promote_trourism--3)/(3--3),
          "Alternative marketing" = (dat$alternative_marketing--3)/(3--3),
          "Top predator reduction" = (dat$top_predator_reduction--3)/(3--3),
          "Coastal infrastructure" = (dat$coastal_infrastructure--3)/(3--3))

#add weights from objectives
#Team A
dat_weights_A <- dat_scale*dat$weight_team1

#Team B
dat_weights_B <- dat_scale*dat$weight_team2
  
#Mean
dat_weights_mean <- dat_scale*dat$weight_average

#add objective names to table
Objectives <- dat$objective

dat_weights_A$Objectives <- Objectives
dat_weights_B$Objectives <- Objectives
dat_weights_mean$Objectives <- Objectives

dat_weights_long_A <- gather(dat_weights_A,  "Alternatives", "Weights","Reduce fertilization":"Coastal infrastructure")
dat_weights_long_B <- gather(dat_weights_B,  "Alternatives", "Weights","Reduce fertilization":"Coastal infrastructure")
dat_weights_long_mean <- gather(dat_weights_mean,  "Alternatives", "Weights","Reduce fertilization":"Coastal infrastructure")

#------------------------------------------------------------------------------------------------------------

#Total score----
#total score calculation----
dat_weights_long_A <- dat_weights_long_A %>% 
  group_by(Alternatives) %>% 
  mutate(total_score = sum(Weights))

dat_weights_long_B <- dat_weights_long_B %>% 
  group_by(Alternatives) %>% 
  mutate(total_score = sum(Weights))

dat_weights_long_mean <- dat_weights_long_mean %>% 
  group_by(Alternatives) %>% 
  mutate(total_score = sum(Weights))

#plot weights and outcomes

#add colors to points
#Ateam
colA <- NULL
colA[dat_weights_long_A$Alternatives == "ICES advice system"] <- "orange" 
colA[dat_weights_long_A$Alternatives == "MPAs"] <- "violetred" 
colA[dat_weights_long_A$Alternatives == "Top predator reduction"] <- "purple" 
colA[dat_weights_long_A$Alternatives == "Subsidies"] <- "steelblue4" 
colA[dat_weights_long_A$Alternatives == "Riparian strips"] <- "black"
colA[dat_weights_long_A$Alternatives == "EBFM"] <- "violetred"
colA[dat_weights_long_A$Alternatives == "GES"] <- "red3"
colA[dat_weights_long_A$Alternatives == "Non-invasive monitoring"] <- "black"
colA[dat_weights_long_A$Alternatives == "Fisheries monitoring"] <- "black"
colA[dat_weights_long_A$Alternatives == "Reduce vessels"] <- "black"
colA[dat_weights_long_A$Alternatives == "New quota distribution"] <- "black"
colA[dat_weights_long_A$Alternatives == "Ban trawling"] <- "black"
colA[dat_weights_long_A$Alternatives == "Educational programs"] <- "black"
colA[dat_weights_long_A$Alternatives == "Reduce fertilization"] <- "black"
colA[dat_weights_long_A$Alternatives == "Environmentally friendly gear"] <- "black"
colA[dat_weights_long_A$Alternatives == "Sea Ranger"] <- "black"
colA[dat_weights_long_A$Alternatives == "Diversification"] <- "black"
colA[dat_weights_long_A$Alternatives == "Re-training"] <- "black"
colA[dat_weights_long_A$Alternatives == "No take areas"] <- "black"
colA[dat_weights_long_A$Alternatives == "Promote tourism"] <- "black"
colA[dat_weights_long_A$Alternatives == "Alternative marketing"] <- "black"
colA[dat_weights_long_A$Alternatives == "Coastal infrastructure"] <- "black"


#Bteam
colB <- NULL
colB[dat_weights_long_B$Alternatives == "ICES advice system"] <- "orange" 
colB[dat_weights_long_B$Alternatives == "MPAs"] <- "violetred" 
colB[dat_weights_long_B$Alternatives == "Top predator reduction"] <- "steelblue4" 
colB[dat_weights_long_B$Alternatives == "Subsidies"] <- "purple" 
colB[dat_weights_long_B$Alternatives == "Reduce fertilization"] <- "violetred" ####
colB[dat_weights_long_B$Alternatives == "Riparian strips"] <- "violetred"
colB[dat_weights_long_B$Alternatives == "EBFM"] <- "black"
colB[dat_weights_long_B$Alternatives == "GES"] <- "red3"
colB[dat_weights_long_B$Alternatives == "Non-invasive monitoring"] <- "black"
colB[dat_weights_long_B$Alternatives == "Fisheries monitoring"] <- "black"
colB[dat_weights_long_B$Alternatives == "Reduce vessels"] <- "black"
colB[dat_weights_long_B$Alternatives == "New quota distribution"] <- "black"
colB[dat_weights_long_B$Alternatives == "Ban trawling"] <- "black"
colB[dat_weights_long_B$Alternatives == "Educational programs"] <- "black"
colB[dat_weights_long_B$Alternatives == "Environmentally friendly gear"] <- "black"
colB[dat_weights_long_B$Alternatives == "Sea Ranger"] <- "black"
colB[dat_weights_long_B$Alternatives == "Diversification"] <- "black"
colB[dat_weights_long_B$Alternatives == "Re-training"] <- "black"
colB[dat_weights_long_B$Alternatives == "No take areas"] <- "black"
colB[dat_weights_long_B$Alternatives == "Promote tourism"] <- "black"
colB[dat_weights_long_B$Alternatives == "Alternative marketing"] <- "black"
colB[dat_weights_long_B$Alternatives == "Coastal infrastructure"] <- "black"

#Average
colAv <- NULL
colAv[dat_weights_long_mean$Alternatives == "ICES advice system"] <- "orange" 
colAv[dat_weights_long_mean$Alternatives == "MPAs"] <- "violetred" 
colAv[dat_weights_long_mean$Alternatives == "Top predator reduction"] <- "steelblue4"
colAv[dat_weights_long_mean$Alternatives == "Subsidies"] <- "purple" #2nd minimum
colAv[dat_weights_long_mean$Alternatives == "Reduce fertilization"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Riparian strips"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "EBFM"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "GES"] <- "red3"
colAv[dat_weights_long_mean$Alternatives == "Non-invasive monitoring"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Fisheries monitoring"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Reduce vessels"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "New quota distribution"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Ban trawling"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Educational programs"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Environmentally friendly gear"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Sea Ranger"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Diversification"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Re-training"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "No take areas"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Promote tourism"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Alternative marketing"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Coastal infrastructure"] <- "black"

#plots
p_teamA <- ggplot(dat_weights_long_A) +
  geom_vline(xintercept = c(1:21), linetype = 3, col = "grey60")+
  geom_point(aes(x=reorder(Alternatives, -total_score), y=total_score), col=colA, size =3)+
  ylab("Ability Score")+
  xlab("Management measures")+
  theme_test()+
  theme(axis.text.x = element_text(angle=90, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size =12),
        axis.title.y = element_text(size = 14))+
  ggtitle("a)")

p_teamB <- ggplot(dat_weights_long_B) +
  geom_vline(xintercept = c(1:21), linetype = 3, col = "grey60")+
  geom_point(aes(x=reorder(Alternatives, -total_score), y=total_score), col=colB, size =3)+
  ylab("")+
  xlab("Management measures")+
  theme_test()+
  theme(axis.text.x = element_text(angle=90, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size =12))+
  ggtitle("b)")

p_mean <- ggplot(dat_weights_long_mean) +
  geom_vline(xintercept = c(1:21), linetype = 3, col = "grey60")+
  geom_point(aes(x=reorder(Alternatives, -total_score), y=total_score), col=colAv, size =3)+
  ylab("")+
  xlab("Management measures")+
  theme_test()+
  theme(axis.text.x = element_text(angle=90, size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size =12))+
  ggtitle("c)")
p_mean

p_teamA + p_teamB+ p_mean + plot_layout()
