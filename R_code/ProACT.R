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
  reframe("reduce fertilization" = (dat$reduce_fertilization--3)/(3--3),
          "riparian strips" = (dat$Riparian_strips--3)/(3--3),
          "GES" = (dat$GES --3)/(3--3),
          "EBFM" = (dat$EBFM--3)/(3--3),
          "non-invasive monitoring" = (dat$non_invasive_monitoring--3)/(3--3),
          "fisheries monitoring" = (dat$fisheries_monitoring--3)/(3--3),
          "ICES advice system" = (dat$ICES_advice_system--3)/(3--3),
          "reduce vessels" = (dat$reduce_vessels--3)/(3--3),
          "new quota distribution" = (dat$new_quota_distribution--3)/(3--3),
          "ban trawling" = (dat$ban_trawling--3)/(3--3),
          "educational programs" = (dat$educational_programs--3)/(3--3),
          "environmentally friendly gear" = (dat$environm_friendly_gear--3)/(3--3),
          "Sea Ranger" = (dat$searanger--3)/(3--3),
          "subsidies" = (dat$subsidies--3)/(3--3),
          "diversification" = (dat$diversification--3)/(3--3),
          "re-training" = (dat$re_training--3)/(3--3),
          "MPAs" = (dat$MPAs--3)/(3--3),
          "no take areas" = (dat$no_take_areas--3)/(3--3),
          "promote trourism" = (dat$promote_trourism--3)/(3--3),
          "alternative marketing" = (dat$alternative_marketing--3)/(3--3),
          "top predator reduction" = (dat$top_predator_reduction--3)/(3--3),
          "coastal infrastructure" = (dat$coastal_infrastructure--3)/(3--3))

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

dat_weights_long_A <- gather(dat_weights_A,  "Alternatives", "Weights","reduce fertilization":"coastal infrastructure")
dat_weights_long_B <- gather(dat_weights_B,  "Alternatives", "Weights","reduce fertilization":"coastal infrastructure")
dat_weights_long_mean <- gather(dat_weights_mean,  "Alternatives", "Weights","reduce fertilization":"coastal infrastructure")

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
colA[dat_weights_long_A$Alternatives == "top predator reduction"] <- "purple" 
colA[dat_weights_long_A$Alternatives == "subsidies"] <- "steelblue4" 
colA[dat_weights_long_A$Alternatives == "riparian strips"] <- "black"
colA[dat_weights_long_A$Alternatives == "EBFM"] <- "violetred"
colA[dat_weights_long_A$Alternatives == "GES"] <- "red3"
colA[dat_weights_long_A$Alternatives == "non-invasive monitoring"] <- "black"
colA[dat_weights_long_A$Alternatives == "fisheries monitoring"] <- "black"
colA[dat_weights_long_A$Alternatives == "reduce vessels"] <- "black"
colA[dat_weights_long_A$Alternatives == "new quota distribution"] <- "black"
colA[dat_weights_long_A$Alternatives == "ban trawling"] <- "black"
colA[dat_weights_long_A$Alternatives == "educational programs"] <- "black"
colA[dat_weights_long_A$Alternatives == "reduce fertilization"] <- "black"
colA[dat_weights_long_A$Alternatives == "environmentally friendly gear"] <- "black"
colA[dat_weights_long_A$Alternatives == "Sea Ranger"] <- "black"
colA[dat_weights_long_A$Alternatives == "diversification"] <- "black"
colA[dat_weights_long_A$Alternatives == "re-training"] <- "black"
colA[dat_weights_long_A$Alternatives == "no take areas"] <- "black"
colA[dat_weights_long_A$Alternatives == "promote trourism"] <- "black"
colA[dat_weights_long_A$Alternatives == "alternative marketing"] <- "black"
colA[dat_weights_long_A$Alternatives == "coastal infrastructure"] <- "black"


#Bteam
colB <- NULL
colB[dat_weights_long_B$Alternatives == "ICES advice system"] <- "orange" 
colB[dat_weights_long_B$Alternatives == "MPAs"] <- "violetred" 
colB[dat_weights_long_B$Alternatives == "top predator reduction"] <- "steelblue4" 
colB[dat_weights_long_B$Alternatives == "subsidies"] <- "purple" 
colB[dat_weights_long_B$Alternatives == "reduce fertilization"] <- "violetred" ####
colB[dat_weights_long_B$Alternatives == "riparian strips"] <- "violetred"
colB[dat_weights_long_B$Alternatives == "EBFM"] <- "black"
colB[dat_weights_long_B$Alternatives == "GES"] <- "red3"
colB[dat_weights_long_B$Alternatives == "non-invasive monitoring"] <- "black"
colB[dat_weights_long_B$Alternatives == "fisheries monitoring"] <- "black"
colB[dat_weights_long_B$Alternatives == "reduce vessels"] <- "black"
colB[dat_weights_long_B$Alternatives == "new quota distribution"] <- "black"
colB[dat_weights_long_B$Alternatives == "ban trawling"] <- "black"
colB[dat_weights_long_B$Alternatives == "educational programs"] <- "black"
colB[dat_weights_long_B$Alternatives == "environmentally friendly gear"] <- "black"
colB[dat_weights_long_B$Alternatives == "Sea Ranger"] <- "black"
colB[dat_weights_long_B$Alternatives == "diversification"] <- "black"
colB[dat_weights_long_B$Alternatives == "re-training"] <- "black"
colB[dat_weights_long_B$Alternatives == "no take areas"] <- "black"
colB[dat_weights_long_B$Alternatives == "promote trourism"] <- "black"
colB[dat_weights_long_B$Alternatives == "alternative marketing"] <- "black"
colB[dat_weights_long_B$Alternatives == "coastal infrastructure"] <- "black"

#Average
colAv <- NULL
colAv[dat_weights_long_mean$Alternatives == "ICES advice system"] <- "orange" 
colAv[dat_weights_long_mean$Alternatives == "MPAs"] <- "violetred" 
colAv[dat_weights_long_mean$Alternatives == "top predator reduction"] <- "steelblue4"
colAv[dat_weights_long_mean$Alternatives == "subsidies"] <- "purple" #2nd minimum
colAv[dat_weights_long_mean$Alternatives == "reduce fertilization"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "riparian strips"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "EBFM"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "GES"] <- "red3"
colAv[dat_weights_long_mean$Alternatives == "non-invasive monitoring"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "fisheries monitoring"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "reduce vessels"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "new quota distribution"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "ban trawling"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "educational programs"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "environmentally friendly gear"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "Sea Ranger"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "diversification"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "re-training"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "no take areas"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "promote trourism"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "alternative marketing"] <- "black"
colAv[dat_weights_long_mean$Alternatives == "coastal infrastructure"] <- "black"

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
