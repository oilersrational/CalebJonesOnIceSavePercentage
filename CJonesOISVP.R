##Caleb Jones and PDO



##The claim is that Caleb Jones's goal numbers are not a true reflection of how good a player he
#is because he suffered from a low on-ice save%

##The counterclaim (that I've made before in person and on Twitter) is that he really is not 
#very good, at least defensively, and cannot be exonerated by low OISV%


##So, what are the questions one can ask to test either claim?
#In other words, what patterns would we see in Jones's data under either assumption

#Before we start, set your working directory to wherever you want to load your files from
setwd("~/Desktop/HockeyStuff/CJonesProj")
#and load in the packages - make sure you first install them with "install.packages("dplyr")"
library(dplyr)
library(BayesFactor)
library(ggplot2)
library(stats)


#####1. Correlation between OISV% in successive years#####
##Well, the most obvious way to exonerate Jones would be to show no correlation between OISV%
# from one year to the next. In other words, if OISV% is random, a player who suffered in this
# area one year might benefit from it the next

#So, let's get defenceman data from the last two seasons to compute this correlation
#I usually use NaturalStatTrick for this type of data
#This is 5v5 count data for each of the last two regular seasons
data2020 = read.csv("dmen2020.csv")
data2021 = read.csv("dmen2021.csv")

#First, I want to only select for defencemen who played let's say 400 minutes over each of the past two seasons
#Here we use the dplyr package which is literal magic
data2020 = data2020 %>%
  filter(data2020$TOI >= 400)
data2021 = data2021 %>%
  filter(data2021$TOI >= 400)

#And let's just reduce these to only those who are in both new data frames
data = merge(data2020, data2021, by = "Player", suffixes = c("2020", "2021"))

#And we're left with a sample of 161 defencemen.
#The first test is whether there's a correlation between 2020 OISV% and 2021 OISV%
test1 = cor.test(data$On.Ice.SV.2020, data$On.Ice.SV.2021, method = "pearson")
test1

#So, clearly no correlation here, but the method we used makes it difficult to know for sure that there is an absence of an effect
#let's conduct a Bayesian test so we can actually quantify the probability that this correlation is 0
corBF1 = correlationBF(data$On.Ice.SV.2021, data$On.Ice.SV.2020, rscale = "medium", nullInterval = NULL,
              posterior = FALSE)
#This is the value we're looking for. 5.44 times more likely that our correlation is 0 than that it is 
#larger than about .5
BF1 = 1/(extractBF(corBF1)$bf)
BF1

#Let's plot it
ggplot(data, aes(On.Ice.SV.2020, On.Ice.SV.2021)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE, color='chocolate1') +
  theme_minimal() +
  labs(x = '2020 On-Ice Save %', y = '2021 On-Ice Save %', title = 'Correlation between 2020 and 2021 On-Ice SV% among NHL D-men') +
  geom_label(data = subset(data, Team2021 == "EDM"), 
             aes(label = Player), hjust = "left") +
  scale_x_continuous(limits = c(87.5, 97.5)) +
  scale_y_continuous(limits = c(87.5, 95), breaks = c(87.5, 90.0, 92.5, 95)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))


#In sum, impossible to predict next season's OISV% from this year's.

#####2. Correlation between chance quality against and OISV%
#BUT, we aren't quite done there.
#I have a theory that OISV% is lower for defencemen who leak high quality scoring chances, which
#doesn't sound groundbreaking but is nontrivial to quantify
#Once again using dplyr
#Total chances against
data = data %>%
  mutate(TCA2020 = HDCA2020 + MDCA2020 + LDCA2020)

#and For
data = data %>%
  mutate(TCF2020 = HDCF2020 + MDCF2020 + LDCF2020)

#High Danger chances against as a percentage of total chances against
#This can be thought of as a pseudo-measure of chance quality against
data = data %>%
  mutate(HDCAP2020 = (HDCA2020/TCA2020)*100)

#Same but with high and medium chances (just in case it makes more sense to use this)
data = data %>%
  mutate(HMDCAP2020 = ((HDCA2020 + MDCA2020)/TCA2020)*100)

#The same but now for 2021
data = data %>%
  mutate(TCA2021 = HDCA2021 + MDCA2021 + LDCA2021)

#For
data = data %>%
  mutate(TCF2021 = HDCF2021 + MDCF2021 + LDCF2021)

#High Danger chances against as a percentage of total chances against
data = data %>%
  mutate(HDCAP2021 = (HDCA2021/TCA2021)*100)

#Same but with high and medium chances
data = data %>%
  mutate(HMDCAP2021 = ((HDCA2021 + MDCA2021)/TCA2021)*100)

#What I want to know is whether those who leak high quality chances suffer from low OISV%
#Simple correlation for each year
#2020
test2a = cor.test(data$HDCAP2020, data$On.Ice.SV.2020, method = "pearson")
test2a

#2021 - unusual that the correlation is so much lower this year
test2b = cor.test(data$HDCAP2021, data$On.Ice.SV.2021, method = "pearson")
test2b


ggplot(data, aes(HDCAP2020, On.Ice.SV.2020)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE, color='chocolate1') +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'Chance Quality Against vs On-Ice Save % (2020)') +
  geom_label(data = subset(data, Team2020 == "EDM"), aes(label = Player), hjust = "left") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  annotate("text", x = 17, y = 89, label = "italic(r) == -.29",
           parse = T)

ggplot(data, aes(HDCAP2021, On.Ice.SV.2021)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE, color='chocolate1') +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'Chance Quality Against vs On-Ice Save % (2021)') +
  geom_label(data = subset(data, Team2021 == "EDM"), aes(label = Player), hjust = "left") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  annotate("text", x = 17, y = 87.75, label = "italic(r) == -.14",
           parse = T)

#So, negative correlation between HQ chances against and OISV%
#But note that this year seems a bit noisier (smaller sample size, of course)

#Wonder if there's a chance quality correlation year-to-year 

#####3. Chance quality 2020 vs 2021 correlation
##Simple enough, do those who leak high quality chances do so year after year?
test3 = cor.test(data$HDCAP2020, data$HDCAP2021, method = "pearson")
test3


#Yes, very clearly so
ggplot(data, aes(HDCAP2020, HDCAP2021)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE, color='chocolate1') +
  theme_minimal() +
  labs(x = '% High Quality Chances Against (2020)', y = '% High Quality Chances Against (2021)', title = 'Chance Quality Against 2020 vs 2021') +
  geom_label(data = subset(data, Team2021 == "EDM"), aes(label = Player), hjust = "left") +
  scale_x_continuous(limits = c(14.2, 28), breaks = c(15.0, 17.5, 20.0, 22.5, 25.0, 27.5)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  annotate("text", x = 23, y = 15.5, label = "italic(r) == .33",
           parse = T)



#####4. Predicting OISV% with Chance Quality Against and some goalie stats#####
##The problem with the above is we're predicting OISV% without even factoring in goalie performance
##As a result, these correlations might be a bit deflated
##What if we predicted OISV% using both chance quality and goalie save %?

#This part is a bit difficult because we need to add in some measure of team and/or goalie save% 
#Knowing that our dependent measure here is OISV%, we want to abstract from that a bit. Could do per-game
#save %, but on a per-game basis, the chance quality variable might get pretty noisy. Let's do team season
#save % and defenceman season avg chance quality -> defenceman OISV%

#So I download regular season 5v5 team data from Natural Stat Trick
teamdata2020 = read.csv("team2020.csv")
teamdata2021 = read.csv("team2021.csv")

#An unusual thing here: some players played for more than one team in each season. I'm not going to 
#bother weighting their aveerage save percentage to their teams, but rather I'll work off of the
#assumption that, on average, players generally play more games for the team they start the season with.
data$Team2020 = gsub(",.*", "", data$Team2020)
data$Team2021 = gsub(",.*", "", data$Team2021)

#So i want to add the team sv% data to our data frame
#Easiest way is to rename the variable in the team data frame to match those of our player data frame
colnames(teamdata2020) = paste(colnames(teamdata2020), "2020", sep = "")
colnames(teamdata2021) = paste(colnames(teamdata2021), "2021", sep = "")

#Let's merge these data into our working data frame
#Just for hygiene, lets keep only the columns we care about from each team df
teamdata2020 = teamdata2020[, c(2, 36, 47, 71)]
teamdata2021 = teamdata2021[, c(2, 36, 47, 71)]
newdata = merge(data, teamdata2020, by = "Team2020")
newdata = merge(newdata, teamdata2021, by = "Team2021")

#This is a regression model predicting OISV% with team SV% and chance quality against
#Before I run these, let's make sure the data is in a comprehensiblee order
newdata = newdata[order(newdata$Player),]

model2020 = lm (On.Ice.SV.2020 ~ HDCAP2020 + SV.2020, newdata)
summary(model2020)

betamodel2020 = lm (scale(On.Ice.SV.2020) ~ scale(HDCAP2020) + scale(SV.2020), newdata)
summary(betamodel2020)

model2021 = lm (scale(On.Ice.SV.2021) ~ scale(HDCAP2021) * scale(SV.2021), newdata)
summary(model2021)

ggplot(newdata, aes(y = On.Ice.SV.2020, x = HDCAP2020)) +
  geom_point(aes(colour = SV.2020)) +
  geom_smooth(mapping = aes(y = On.Ice.SV.2020, x = HDCAP2020), method = 'lm', se = TRUE, color = 'chocolate1', formula = ) +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'On-Ice Save % by Team Save % and Chance Quality Against (2020)') +
  geom_label(data = subset(data, Team2020 == "EDM"), aes(label = Player), hjust = "left") +
  labs(colour = 'Team SV%') +
  scale_color_gradient(low = "midnightblue", high = "steelblue1") +
  scale_x_continuous(limits = c(14.2, 28), breaks = c(15.0, 17.5, 20.0, 22.5, 25.0, 27.5)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  annotate("text", x = 17, y = 88.5, label = "atop(italic(beta[CQA]) == -.14,italic(beta[SVP]) == .53)",
           parse = T)

ggplot(newdata, aes(y = On.Ice.SV.2021, x = HDCAP2021)) +
  geom_point(aes(color = SV.2021)) +
  geom_smooth(mapping = aes(y = On.Ice.SV.2021, x = HDCAP2021), method = 'lm', se = TRUE, color = 'chocolate1', formula = ) +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'On-Ice Save % by Team Save % and Chance Quality Against (2021)') +
  geom_label(data = subset(data, Team2021 == "EDM"), aes(label = Player), hjust = "left") +
  labs(colour = 'Team SV%') +
  scale_color_gradient(low = "midnightblue", high = "steelblue1") +
  scale_x_continuous(limits = c(14.2, 25.2), breaks = c(15.0, 17.5, 20.0, 22.5, 25.0)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  annotate("text", x = 17, y = 88.5, label = "atop(italic(beta[CQA]) == -.068, italic(beta[SVP]) == .50)",
           parse = T)+
  annotate("text", x = 17, y = 87.7, label = "italic(beta[CQASVP]) == .162",
           parse = T)


model2021p = lm (On.Ice.SV.2021 ~ HDCAP2021 * SV.2021, newdata)
model2020p = lm (On.Ice.SV.2020 ~ HDCAP2020 * SV.2020, newdata)



##I'm gonna put the instructions inline here cause the next package is less standard
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra", force = T)
library(ggiraphExtra)

ggPredict(model2021p) +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'On-Ice Save % by Team Save % and Chance Quality Against (2021)') +
  labs(colour = 'Team SV%', fill = element_blank()) +
  scale_color_gradient(low = "midnightblue", high = "steelblue1") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  guides(fill=FALSE)

ggPredict(model2020p) +
  theme_minimal() +
  labs(x = '% High Quality Chances Against', y = 'On-Ice Save %', title = 'On-Ice Save % by Team Save % and Chance Quality Against (2021)') +
  labs(colour = 'Team SV%', fill = element_blank()) +
  scale_color_gradient(low = "midnightblue", high = "steelblue1") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15)) +
  guides(fill=FALSE)
##

##So, Chance quality against is predictive of OISV% even after accounting for team SV%, and the full
#model is ~okay~, with an R-squared of ~.35
#We've already established that chance quality is relatively stable (particularly for Caleb Jones)
#So, one could take that as evidence not to expect his OISV% (and thus his goal #s) to change drastically
#One more question we can answer is: if JOnes got the OISV% predicted by the model, what do his goal numbers look like?


#####5. Examining model-fitted values and applying those to goals#####
##First need an index of the player or players we're looking for (Jones & other Oilers for interest)
oilIndex = newdata$Team2021 == "EDM"

summary(model2020)
preds2020 = predict.lm(model2020, se.fit = T, interval = "confidence", level = .8)

preds2020 = preds2020$fit
preds2020 = as.data.frame(preds2020)
preds2020 = preds2020[oilIndex,]

###The index has Oilers players in this order:
#Adam Larsson
#Caleb Jones
#Darnell Nurse
#Kulikov
#Ethan Bear
#Kris Russell
#Tyson Barrie

#First column is the estimate, and the higher and lower numbers are the boundaries with which we can
#be 80% certain the true value falls between them



##So, the model has predicted Jones to have an OISV% of 91.28%, but the confidence interval is between 
#91.13% and 91.43%, higher only than Barrie
##At 91.28% at 5v5, Jones would have been on the ice for 178 (Shots against) * (1-.9128)
JonesPredGoalsA = (1-.9128) * 178
JonesPredGoalsA





