#Load packages and functions
library(ggplot2)
library(tidyr)
library(dplyr)
library(prevalence)
setwd("/Users/szasulja/Dropbox/Public/bora_facexinst_paper/data/")
source("summarySE.R")

########################################
# STUDY 1 - "RELATIVE TRUST EVALUATIONS"
########################################

rawdata <- read.csv("study1.csv")

#RECODE VALUES 

#originally chairman coded as "2". recode, so "2" consequently denotes orig. face
rawdata$AsymChair <- ifelse(rawdata$UglyChair == 1, 2,
                            ifelse(rawdata$UglyChair == 2, 1, NA))
rawdata$AsymChair2 <- ifelse(rawdata$UglyChair2 == 1, 2,
                             ifelse(rawdata$UglyChair2 == 2, 1, NA))
#2000 recorded as 1, 1999 as 2 etc. 
rawdata$age <- rawdata$year + 14

#TIDY DATA

data <- rawdata %>%
        gather(group, pref, c(AttrChair, AttrChair2, AsymChair, AsymChair2, 
                              Losers, Losers2), na.rm = T) %>%
        transmute(group = as.factor(group), pref = as.factor(pref),
                  pref = factor(pref, labels = c("Asymmetric", "Original")))

levels(data$group) <- list(Congruent_Group = c("AttrChair", "AttrChair2"),
                           Control_Group = c("Losers", "Losers2"),
                           Incongruent_Group = c("AsymChair", "AsymChair2"))[c(2, 1, 3)]


#Figure 2. Bar chart with confidence intervals

#Get frequencies for groups. 
clean <- data.frame(table(data$group, data$pref))
names(clean) <- c("group", "value", "freq") #this seems redundant but next line won't run w/o it
clean <- cbind(clean, rep(tapply(clean$freq, clean$group, sum), 2))
names(clean) <- c("group", "value", "freq", "trials")
levels(clean$group) <- c("Control Group", "Congruent Group", "Incongruent Group")

#calculate Agresti-Coull interval (adjusted Wald interval)
clean <- cbind(clean, propCI(clean$freq, clean$trials, method = "ac")[, c(3, 6:7)])

#Plot and save to working dir. Figure 2. 
ggplot(clean, aes(y = p, x = value)) + 
        geom_bar(stat = "identity") + 
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, 
                      position=position_dodge(0.1)) + 
        facet_grid(~ group) + 
        xlab("") + 
        ylab("Proportion of choices") + 
        theme_classic() +
        theme(text = element_text(size = 14))
ggsave('figure2.pdf', height = 12, width = 24, units = "cm")


# ANALYSIS

#Control vs Congruent group - preference for face
chisq.test(table(data$group, data$pref)[c(1,2), ])

#Control vs Incongruent group - preference for face
chisq.test(table(data$group, data$pref)[c(1,3), ])

#Congruent vs Incongruent - preference for institutional competence
#in congruent condition Original (column 2) = competent.
#in incongruent condition Asymmetric (column 1) = competent. 
#create "newtable" where both conditions: competent in (column 1)
newtable <- rbind(table(data$group, data$pref)[c(2),c(2,1)], 
                  table(data$group, data$pref)[c(3),])
chisq.test((newtable))


###########################################
# STUDY 2 - "INDEPENDENT TRUST EVALUATIONS"
###########################################

#Set working directory
rawdata <- read.csv("study2.csv",
                    header = T, na.strings = "")

#Exclude non-US 
rawdata <- rawdata[!is.na(rawdata$Intro), ]

# TIDY DATA
data <- rawdata %>% 
        gather(treat, trust, c(AA, AS, FA, FS, CA, CS), na.rm = T) %>%
        gather(group1, time, c(AA_T_3, AS_T_3, FA_T_3, FS_T_3, CA_T_3, CS_T_3), 
               na.rm = T) %>%
        separate(treat, c("position", "face"), sep = 1) %>%
        transmute(position = as.factor(position),
                  position = factor(position, labels = c("Activist", "Chairman", "Former \n Chairman")),
                  position = factor(position, levels(position)[c(1,3,2)]),
                  face = as.factor(face), 
                  face = factor(face, labels = c("Asymmetric", "Symmetric")),
                  trust = trust-1, #adjust scales to 0 min value. 
                  time = time, 
                  interest = interest-1,
                  poltrust = poltrust-1)


# ANALYSE
sumdat1 <- summarySE(data, measurevar = "trust", groupvars = c("face", "position"))

#Paper 1. Figure 3. manuscript .v6
ggplot(sumdat1, aes(x=position, y=mean, fill=face)) + 
        geom_bar(stat = "identity", position=position_dodge(0.9)) + 
        geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, 
                      position=position_dodge(0.9)) +
        ylim(0,10) + 
        ylab("Mean trust in Jim Smith") +
        xlab("") + 
        scale_fill_grey(name = "Facial \nappearance") + 
        theme_classic() + 
        theme(text = element_text(size = 10), 
              legend.position = c(0.8, 0.84)) 
ggsave("figure3.pdf", height = 10, width = 8, units = "cm")

summary(aov(trust ~ position * face, data))
summary(lm(trust ~ position + face, data))


#####################################################################
# STUDY 3 - "INDEPENDENT TRUST EVALUATIONS IN A DIFFERENT POPULATION"
# A.K.A "RUS" SURVEY 
#####################################################################

rawdata <- read.csv("study3.csv", header = T, na.strings = "", 
                    fileEncoding = "ISO-8859-1")

#How long was the survey?
starttime <- strptime(rawdata$V8, "%m/%d/%Y %H:%M")
endtime <- strptime(rawdata$V9, "%m/%d/%Y %H:%M")
mean(endtime - starttime)


#Subset variables from my exp.
# which(names(data) == "F.Act.Stim") #first var.
# which(names(data) == "Cha.Char_8") #last var.
rawdata <- rawdata[, 29:94]
names(rawdata) <- gsub("Stim.1", "Neg", names(rawdata))
names(rawdata) <- gsub("Stim", "Pos", names(rawdata))



# TIDY DATA  

data <- rawdata %>%
        #Treatment groups
        gather(group, true, c(F.Act.Pos, F.Act.Neg, F.For.Pos, F.For.Neg, 
                              F.Cha.Pos, F.Cha.Neg),
               na.rm = TRUE) %>%
        extract(group, c("position", "face"), "F\\.(...)\\.(...)") %>%
        #Tidy Find
        gather(group2, find, c(Act.Find, For.Find, Cha.Find), na.rm = TRUE) %>% 
        #Tidy Solve
        gather(group3, solve, c(Act.Solve, For.Solve, Cha.Solve), na.rm = TRUE) %>% 
        #Tidy Responsetime
        gather(group4, resp_time, 
               c(F.Act.Time_3, F.Act.Time_3.1, F.For.Time_3, F.For.Time_3.1,
                 F.Cha.Time_3, F.Cha.Time_3.1),
               na.rm = TRUE)   %>%
        #Tidy characters
        gather(group5, character, contains("Char_"), na.rm = TRUE) %>%
        extract(group5, "char", ".......(...)") %>%
        spread(char, character) %>%
        transmute(position = as.factor(position), 
                  position = factor(position, levels(position)[c(1, 3, 2)]), 
                  position = factor(position, labels = c("Activist", 
                                                         "Former \n Chairman", 
                                                         "Chairman")),
                  face = as.factor(face), 
                  face = factor(face, labels= c("Asymmetric", "Original")),
                  find = find, solve = solve, resp_time = resp_time,
                  wellmeaning = r_1, conscientious = r_2, sincere = r_3, 
                  friendly = r_4, competent = r_5, intelligent = r_6, 
                  effective = r_7, confident = r_8)


#create summary table for solve
sumdat2 <- summarySE(data, measurevar = "solve", groupvars = c("face", "position"))

# Figure 4. - Barplot with confidence intervals. 
ggplot(sumdat2, aes(x=position, y=mean, fill=face)) + 
        geom_bar(stat = "identity", position=position_dodge(0.9)) + 
        geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, 
                      position=position_dodge(0.9)) +
        ylim(0,10) + ylab("Mean trust in finding a solution") +
        xlab("") + 
        scale_fill_grey(name = "Facial \nappearance") + 
        theme_classic() + 
        theme(text = element_text(size = 10), 
              legend.position = c(0.8, 0.84)) 

ggsave('Figure4.pdf', height = 10, width = 8, units = "cm")


# MAIN ANALYSIS

#Is there an interaction between the two treatments? 
summary(aov(solve ~ face*position, data))

#What is te main effect of the treatments? 
summary(lm(solve ~ face + position, data))

# Robustness checks 

#exclude outliers in response time. 
robdat1 <- data[!data$resp_time %in% boxplot.stats(data$resp_time)$out,]

#Repeat models above. 
summary(aov(solve ~ face*position, robdat1))
summary(lm(solve ~ face + position, robdat1))


#Demonstrate that results do not depend on choice of DV 
summary(aov(find ~ face*position, data))
summary(lm(find ~ face + position, data))

summary(aov(find ~ face*position, robdat1))
summary(lm(find ~ face + position, robdat1))


#####################################################################
# STUDY 4 - "INDEPENDENT TRUST EVALUATIONS WITH AND WITHOUT 
# A POLITICAL CONTEXT
#####################################################################
#Load data
rawdata <- read.csv("study4.csv", header = T, na.strings = "")
rawdata <- rawdata[!is.na(rawdata$Intro), ] #exclude non-US 

# TIDY DATA  

data <- rawdata %>% 
        #gather all DVs
        gather(treat, success, c(CO, CA, SO, SA, NO, NA.), na.rm = T) %>%
        gather(g1, comp, contains("comp"), na.rm = T) %>%
        gather(g2, attr, contains("attr"), na.rm = T) %>%
        gather(g3, trust, contains("_trust"), na.rm = T) %>%
        gather(g4, time, contains("time_3"), na.rm = T) %>%
        #separate two factors of treatment
        separate(treat, c("position", "face", "drop"), sep = c(1,2)) %>% 
        mutate(age = (year + 14), 
               position = as.factor(position),
               face = as.factor(face)) %>% 
        select(c(position, face, success, comp, attr, trust, time, interest,
                 poltrust, age, sex, attention))

#relabel for nice plots 
data$position <- factor(data$position, levels(data$position)[c(2, 1, 3)])
data$position <- factor(data$position, labels= c("No Context", "Citizen", 
                                                 "School Board"))
data$face <- factor(data$face, labels = c("Asymmetric", "Original"))

#rescale for min = 0. 
data$trust <- data$trust - 1 
data$comp <- data$comp - 1 
data$attr <- data$attr - 1 

#Subset to attentive respondents. 
data2 <- data[data$attention < 4 & !is.na(data$attention), ]
data2 <- data2[data2$time < min(boxplot.stats(data2$time)$out),]

# ANALYSIS 

# Manipulation checks

# Facial appearance affects perceived attractiveness
summary(aov(attr ~ face, data2))

# Institutional position affects trust in successfully solving coll. act. prob.
data4 <- data2[data2$position != "No Context", ]
summary(aov(success~position, data4))

# DV = competence 

#Figure 5. 
sumdat2 <- summarySE(data2, measurevar = "comp", groupvars = c("face", "position"))

ggplot(sumdat2, aes(x = position, y = mean, fill = face)) + 
        geom_bar(stat = "identity", position=position_dodge(0.9)) + 
        geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, 
                      position=position_dodge(0.9)) +
        ylim(0,6) + ylab("Mean competence in Jim Smith") +
        xlab("") + 
        scale_fill_grey(name = "Facial \nappearance") + 
        theme_classic() + 
        theme(text = element_text(size = 10), 
              legend.position = c(0.8, 0.84)) 
ggsave("figure5.pdf", height = 10, width = 8, units = "cm")

# Formal test. 
summary(lm(comp ~ face * position, data2))


# DV = trust 

sumdat1 <- summarySE(data, measurevar = "trust", groupvars = c("face", "position"))

ggplot(sumdat1, aes(x = position, y = mean, fill = face)) + 
        geom_bar(stat = "identity", position=position_dodge(0.9)) + 
        geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, 
                      position=position_dodge(0.9)) +
        ylim(0,6) + ylab("Mean trust in Jim Smith") +
        xlab("") + 
        scale_fill_grey(name = "Facial \nappearance") + 
        theme_classic() + 
        theme(text = element_text(size = 14), 
              legend.position = c(0.8, 0.84)) 

# Formal test 
summary(lm(trust ~ face * position, data2))
