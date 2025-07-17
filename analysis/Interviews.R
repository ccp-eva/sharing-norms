###################
###Load packages###
###################
library(dplyr)
library(brms)
library(ggplot2)
library(AnthroTools)
library(tidyverse)
library(bayestestR)
library(psych)

#######################################
##load data from source file location##
#######################################

i <- read_csv("../data/Interview_Data_learn.csv")
d <- read_csv("../data/Interview_Data_child_know.csv")
irr <- read_csv("../data/Interview_Data_IRR.csv")
fl <- read_csv("../data/Interview_Data_free_list.csv")

#######################################
###One-on-one interview demographics###
#######################################
nrow(i) ##66 adult participants
round(table(i$Gender)/nrow(i),2)*100  ##58% F
round(table(i$Ethnicity)/nrow(i),2)*100 ##47% BaYaka
round(table(i$Village)/nrow(i),2)*100 ##52% Village 1

####################################
###IRR for social learning coding###
####################################

cohen.kappa(cbind(irr$SLL, irr$SPC)) ##kappa=0.93 [0.88-0.99]
sum(irr$Agree)/nrow(irr) ##96% agreement

#################################
###Post interview demographics###
#################################
##subset to kids under 17
d<-subset(d, Age<17)

length(unique(d$PID)) ##119 participants under 17 years

dp<-d%>%group_by(PID)%>%summarise(Age=unique(Age), Gender=unique(Gender), Ethnicity=unique(Ethnicity), Village=unique(Village))

round(mean(dp$Age),2) ##mean age 9.74
round(sd(dp$Age),2) ##SD age 2.82
round(table(dp$Gender)/nrow(dp),2) ##46% girls
round(table(dp$Ethnicity)/nrow(dp),2) ##51% BaYaka
round(table(dp$Village)/nrow(dp),2) ##45% Village 1

remove<-d %>% group_by(PID) %>% filter(sum(share %in% c("NOT_SURE", "NO_RESPONSE")) == 2) %>% distinct(PID) %>% pull(PID) 

remove ##4 participants did not respond to *both* questions

d <- d %>%  filter(!PID %in% remove) ##remove these participants

length(unique(d$PID)) ####this leaves us with 115 participants

rm(remove)

######################
###Learning Pathway###
######################
fl_intra<-subset(fl, share=="intra")
fl_inter<-subset(fl, share=="inter")

## descriptive stats for free-list responses for intra-ethnic sharing
fl_intra %>%  count(Subj, Ethnicity) %>% group_by(Ethnicity) %>%
  summarise(
    mean_obs = mean(n),
    min_obs = min(n),
    max_obs = max(n),
    .groups = "drop"
  )

## descriptive stats for free-list responses for intra-ethnic sharing
fl_inter %>%  count(Subj, Ethnicity) %>% group_by(Ethnicity) %>%
  summarise(
    mean_obs = mean(n),
    min_obs = min(n),
    max_obs = max(n),
    .groups = "drop"
  )

##For some reason AnthroTools doesn't believe the fl datasets are datasets so we need to remind it
fl_intra<-as.data.frame(fl_intra)
fl_inter<-as.data.frame(fl_inter)

##this is a bit ugly, please forgive me
##for intra
sfl_intra <- CalculateSalience(fl_intra, GROUPING = "Ethnicity")
dsil_intra <- SalienceByCode(sfl_intra,GROUPING = "Ethnicity", dealWithDoubles = "MAX")
dsil_intra_BY<-subset(dsil_intra, GROUPING=="BaYaka")
dsil_intra_BA<-subset(dsil_intra, GROUPING=="Bandongo")


FLT_intra <- FreeListTable(fl_intra, tableType="FREQUENCY", GROUPING = "Ethnicity")
FLT_intra_BY<-subset(FLT_intra, Group=="BaYaka")
FLT_intra_BA<-subset(FLT_intra, Group=="Bandongo")

##for inter
sfl_inter <- CalculateSalience(fl_inter, GROUPING = "Ethnicity")
dsil_inter <- SalienceByCode(sfl_inter,GROUPING = "Ethnicity", dealWithDoubles = "MAX")
dsil_inter_BY<-subset(dsil_inter, GROUPING=="BaYaka")
dsil_inter_BA<-subset(dsil_inter, GROUPING=="Bandongo")


FLT_inter <- FreeListTable(fl_inter, tableType="FREQUENCY", GROUPING = "Ethnicity")
FLT_inter_BY<-subset(FLT_inter, Group=="BaYaka")
FLT_inter_BA<-subset(FLT_inter, Group=="Bandongo")


##Make the table
##make little function to help things along
summarize_fl <- function(data, meta_data) {
  data %>%
    select(-Subject, -Group) %>%                    
    colSums() %>%
    as.data.frame() %>%
    rename(FREQ = 1) %>%
    tibble::rownames_to_column("CODE") %>%
    merge(meta_data, by = "CODE")
}

BY_intra <- summarize_fl(FLT_intra_BY, dsil_intra_BY)
BY_intra <- BY_intra %>% dplyr::select(-MeanSalience, -SumSalience, -GROUPING) %>% rename( BY_intra_Freq = FREQ, BY_intra_SS = SmithsS)


BA_intra <- summarize_fl(FLT_intra_BA, dsil_intra_BA)
BA_intra <- BA_intra %>% dplyr::select(-MeanSalience, -SumSalience, -GROUPING) %>% rename( BA_intra_Freq = FREQ, BA_intra_SS = SmithsS)

BY_inter <- summarize_fl(FLT_inter_BY, dsil_inter_BY)
BY_inter <- BY_inter %>% dplyr::select(-MeanSalience, -SumSalience, -GROUPING) %>% rename( BY_inter_Freq = FREQ, BY_inter_SS = SmithsS)


BA_inter <- summarize_fl(FLT_inter_BA, dsil_inter_BA)
BA_inter <- BA_inter %>% dplyr::select(-MeanSalience, -SumSalience, -GROUPING) %>% rename( BA_inter_Freq = FREQ, BA_inter_SS = SmithsS)


BA_table  <- merge(BA_intra, BA_inter, by = "CODE")
BY_table  <- merge(BY_intra, BY_inter, by = "CODE")
all_table <- merge(BA_table, BY_table, by = "CODE")

all_table <- all_table %>% mutate(across(ends_with("_SS"), ~ round(.x, 3)))
write_csv(all_table, "all_table.csv")

########################
###Learning Mechanism###
########################
##Make tables of mechanism frequency and %
i%>% group_by(Ethnicity)%>%summarise(sample=n())

intra.learn <- i %>%
  group_by(Ethnicity, Intra.Learn) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(percent = round((count / sum(count))*100, 2)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Ethnicity,
    values_from = c("count", "percent"),
    values_fill = 0
  )

as.data.frame(intra.learn)


inter.learn <- i %>%
  group_by(Ethnicity, Inter.Learn) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(percent = round(100 * count / sum(count), 2)) %>%
  ungroup()%>%
  pivot_wider(
    names_from = Ethnicity,
    values_from = c("count", "percent"),
    values_fill = 0
  )

as.data.frame(inter.learn)


###########################
##Wrangle data for models##
###########################

##Make the data long, and clarify the condition.Five BaYaka said they never learned to share inter-ethnically, so we remove them. Two Bandongo adults said they learned to share inter-ethnically in adulthood, we removed them
i_long <- i %>%
  select(PID, Ethnicity, Gender, intra = Intra.Age, inter = Inter.Age) %>%
  pivot_longer(cols = c(intra, inter), names_to = "condition", values_to = "Age") %>%
  filter(Age!="NA", Age != "Adult")


length(unique(i_long$PID)) ##still 66 participants

##Remove what are essentially NAs
d<-subset(d, share!="NOT_SURE")
d<-subset(d, share!="NO_RESPONSE")
length(unique(d$PID)) ##confirming our sample remains the same, it does at 115

d$age<-floor(d$Age) ##age is usually rounded down, so we'll do that here
d$age_z<-scale(d$age) ##z-score standardize for the model

##############
##Set priors##
##############

priors1<-c(prior(normal(0, 1), class = Intercept, dpar="muMiddle"),
           prior(normal(0, 1), class = b, dpar="muMiddle"),
           prior(exponential(1),class=sd, dpar="muMiddle"),
           prior(normal(0, 1), class = Intercept, dpar="muTeen"),
           prior(normal(0, 1), class = b, dpar="muTeen"),
           prior(exponential(1),class=sd, dpar="muTeen"))

priors2<-c(prior(normal(0, 1), class = Intercept),
           prior(normal(0, 1), class = b),
           prior(exponential(1),class=sd))

####################
###Fit the models###
####################
M1<-brm(data=i_long, family=categorical(), Age ~ Gender + condition * Ethnicity + (1|PID), prior=priors1, iter=10000, cores = 4, control = list(adapt_delta = 0.99, max_treedepth=15), set.seed(123456))


M2<-brm(data = d, family = bernoulli(), share ~ Gender + Condition * age_z * Ethnicity  + (1|PID) , prior=priors2, iter=10000, cores = 4, control = list(adapt_delta = 0.99, max_treedepth=15), set.seed(123456))

###############
###Summaries###
###############
summary(M1)
summary(M2)


##################
###Sample Table###
##################
table(i$Intra.Age, i$Ethnicity)
table(i$Inter.Age, i$Ethnicity)
table(i$Ethnicity)

d$age_cat<-ifelse(d$age<8, "Early",ifelse(d$age>12, "Ado","Middle"))
d <- d %>%  mutate(age_cat = factor(age_cat, levels = c("Early", "Middle", "Ado")))

d.table <- d %>%
  group_by(Ethnicity, Condition, age_cat) %>%
  summarise(count_yes = sum(share=="YES"), count=n(), percent=round((count_yes/count)*100, 2)) %>%
  mutate(column_key = paste(Ethnicity, Condition, sep = "_"))

as.data.frame(d.table)

##############
###Figure 2###
##############

##Set conditions
conditions1<-data.frame(Ethnicity=c("Bandongo","BaYaka"))
new_order<-c("intra","inter")

O<-conditional_effects(M1, categorical=TRUE, effects="condition", conditions=conditions1, epred=TRUE, prob = 0.89)

o <- plot(O, plot = FALSE)[[1]]  # plot = FALSE avoids auto-plotting

o<-as.data.frame(o[["data"]])
o$Age<-ifelse(o$effect2__=="Early", "Early Childhood", ifelse(o$effect2__=="Middle","Middle Childhood", "Adolescence"))
Age_order<-c("Early Childhood", "Middle Childhood","Adolescence")
o$Age<-factor(o$Age, levels=Age_order)

o$condition <- factor(o$condition, levels = new_order)

ggplot(o, aes(x = condition, y = estimate__, color = Age)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), 
                position = position_dodge(width = 0.6), width = 0.2) +
  facet_wrap(~ Ethnicity) +
  labs(x = NULL, y = "Predicted Probability of Age of Acquisition") +
  scale_x_discrete(labels = c("Intra-Ethnic","Inter-Ethnic"))+
  theme_minimal(base_size = 14)+
  
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  ) 

##############
###Figure 3###
##############
conditions2<-data.frame(Ethnicity=c("Bandongo","BaYaka"))

P<-conditional_effects(M2, effects="age_z:Condition",conditions=conditions2, epred=TRUE, prob = 0.89)

p <- plot(P, plot = FALSE)[[1]]  # plot = FALSE avoids auto-plotting

p + facet_grid(~Ethnicity) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Standardized Age (z)",
    y = "Predicted Probability of Self-Reported Sharing Knowledge"
  ) +
  scale_color_manual(
    breaks = c("intra", "inter"),
    values = c("intra" = "#43a2ca", "inter" = "#feb24c"),
    labels = c("Intra-ethnic", "Inter-ethnic")
  ) +
  scale_fill_manual(
    breaks = c("intra", "inter"),
    values = c("intra" = "#43a2ca", "inter" = "#feb24c"),
    labels = c("Intra-ethnic", "Inter-ethnic")
  ) +
  guides(
    color = guide_legend(title = "Condition"),
    fill = guide_legend(title = "Condition")
  ) +
  scale_x_continuous(
    name = "Age (years)",
    breaks = seq(-1, 2, by = 1),
    labels = round(mean(d$age) + sd(d$age) * seq(-1, 2, by = 1))
  ) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

