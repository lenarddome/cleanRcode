library(tidyverse)

wordnaming.Data <- read_csv("https://raw.githubusercontent.com/ajwills72/rminr-data/master/wordnaming2.csv")

wordnaming.Data %>% head()

   

require(ggplot2)

wordnaming.Data %>% group_by(subj, congru, medit) %>%
summarise(acc = mean(acc)) -> aggregated_data

# not seeing anything interesting here
aggregated_data %>% ggplot(aes(x = acc, fill = congru, group = congru)) +  geom_density(alpha = 0.5) + facet_wrap("medit", nrow = 3)

require(BayesFactor)

wordnaming.Data <- wordnaming.Data[wordnaming.Data$congru != "neutral" &
                                   wordnaming.Data$medit != "relax", ]

wordnaming.Data$subj <- as.factor(wordnaming.Data$subj)
wordnaming.Data$medit <- as.factor(wordnaming.Data$medit)
wordnaming.Data$congru <- as.factor(wordnaming.Data$congru)

`data-for-anova` <- wordnaming.Data %>% group_by(subj, medit, congru) %>% summarise(rt = mean(rt))

# what we came here to do
anovaBF(formula=rt ~ medit*congru + subj, data=`data-for-anova`, whichRandom="subj", multicore = TRUE)


ttestBF(x = wordnaming.Data$acc[wordnaming.Data$congru == "cong"], mu = 0.5)
ttestBF(x = wordnaming.Data$acc[wordnaming.Data$congru == "incong"], mu = 0.5)

## congruant above change bf  large 
## incongruent chance bf large
## neutral large
## nothing interesting here

## TODO:check if subjects individually were above 0.5

ttestBF(formula = acc ~ sex, data = wordnaming.Data)
BayesFactor::ttestBF(formula = acc ~ medit, data = wordnaming.Data)
ttestBF(formula = acc ~ congru, data = wordnaming.Data)

wordnaming.Data.rt <- wordnaming.Data %>% group_by(subj, sex, medit, congru) %>% summarise(rt = mean(rt))

ttestBF(formula = rt ~ sex, data = wordnaming.Data.rt)
ttestBF(formula = rt ~ medit, data = wordnaming.Data.rt)
ttestBF(formula = rt ~ congru, data = wordnaming.Data.rt)
## found something
wordnaming.Data.rt %>% ggplot(aes(x=congru,y=rt,fill=medit)) +
geom_violin() +
stat_summary(position = "dodge") +
facet_grid(medit ~ sex)
