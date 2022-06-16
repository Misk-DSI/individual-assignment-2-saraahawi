# (1) Calling libraries to environment
library(tidyverse)
library(ggthemes)
library(janitor)
library(GGally)
library(corrplot)
library(prettydoc)
library(reactable)
library(patchwork)
library(forcats)
library(here)



# (2) Importing my csv from data folder and initializing the here function
        # here("individual-assignment-2-saraahawi.Rproj")
        # i_am("./data/winequality-red.csv")
wine <- as_tibble(read_csv("./data/winequality-red.csv"))
    

# (3) Data cleaning 
    # Take a quick look at the data.
glimpse(wine)
summary(wine) # Summary tells us that we have 1,599 non-NULL values.  

# We just need to make sure we don't have duplicates then we're good to go.
get_dupes(wine) 
wine <- distinct(wine)
glimpse(wine)
# Great! around 240 duplicate values were removed and we're now good to go!

# Okay let's now see what we're working with by viewing column names: 
names(wine) # Names contain spaces which is rather unconventional for EDA.

# Pass variable names to janitor::clean_names function to replace spaces by underscores.
wine <- wine %>%
  clean_names()
# Check
names(wine)

# Great! Let's move on. 

# (4) Exploratory Data Analysis

# Some of the questions I wanna answer about my data are:
# 1- Which variables have the most impact on quality? How are variables correlated amongst each other?
    # A correlation map will roughly answer that question for me. 
corrplot(cor(wine), method = "color", type = "lower", is.corr = TRUE, mar=c(0,0,1,0),
         title = 'Correlation Map for Dataset Variables',addCoef.col = TRUE,
         tl.cex = 0.6, tl.col = 'black', number.cex=0.5)

# Let’s have our initial assumptions confirmed by a boxplot.
wine %>% 
  ggplot(aes(factor(quality),`alcohol`, group=quality)) +
  geom_boxplot() +
  xlab("Quality") + 
  ylab("Alcohol") +
  ggtitle("Alcohol & Quality") +
  theme_economist()

wine %>% 
  ggplot(aes(factor(quality), `volatile_acidity`, group=quality)) +
  geom_boxplot() + 
  xlab("Quality") + 
  ylab("Volatile Acidity") +
  ggtitle("Volatile Acidity & Quality") +
  theme_economist()

# 2- Are there any more hidden patterns the correlation map isn't showing me? 
#    What are other variables that can have a less direct contribution to wine quality but affect it nonetheless?

# If we look at the quality score distribution in our data, we will 
# see that around 80% of the observations in the dataset are either 
# at 5 or 6, while 3, 4 and 7, 8 account for the remaining 20%.

# As a result, it will be hard to recognize important patterns across 
# different quality scores because patterns associated with 5 and 6 
# will be the most dominant. 


wine %>% 
  group_by(quality) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))


ggplot(wine,aes(quality)) + 
  geom_bar(stat = "count") +
  coord_flip() +
  scale_x_continuous(breaks = seq(3,8)) +
  xlab("Quality Score") + 
  ylab("Count") +
  ggtitle("Distribution of Wine Quality Scores") + 
  theme_economist()

# To solve that problem, another classification scheme has to be implemented where wine 
# quality scores are further divided into three tiers: Poor (scores : 3 & 4), 
# Mediocre (scores: 5 & 6), and Excellent (scores: 7 & 8).

wine$quality_tier <- ifelse(wine$quality > 6,"Excellent", ifelse(wine$quality < 5, "Poor", "Mediocre"))
Quality <- factor(wine$quality_tier) # Now, categories are Poor (scores : 3 & 4), 
# Mediocre (scores: 5 & 6), and Excellent (scores: 7 & 8).

ggplot(wine, aes(quality_tier)) + 
  geom_bar(stat = "count") +
  coord_flip() +
  xlab(label = NULL) + 
  ylab("Count") +
  ggtitle("Distribution of Wine Quality Tiers") + 
  theme_economist()

# Now we can see the effect of different variables on the quality tiers more clearly and accurately. 

# Let's look at distributions of variables respective to every quality tier. 

# 1. Alcohol
ggplot(wine,aes(x=alcohol,fill=Quality)) + 
  geom_density(alpha=0.25) +
  geom_vline(aes(xintercept=mean(alcohol[quality_tier=="Excellent"])),color="red",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(alcohol[quality_tier=="Mediocre"])),color="green",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(alcohol[quality_tier=="Poor"])),color="blue",linetype="dashed",lwd=1) +
  scale_x_continuous(breaks = seq(7,14,1))+
  xlab(label = "Alcohol Level (%)") +
  ylab(label = NULL) +
  ggtitle("Distribution of Alcohol Levels")+
  theme_economist()

# 2. Volatile Acidity
ggplot(wine, aes(x = volatile_acidity,fill=Quality)) + 
  geom_density(alpha=0.25) +
  geom_vline(aes(xintercept=mean(volatile_acidity[quality_tier=="Excellent"])),color="red",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(volatile_acidity[quality_tier=="Mediocre"])),color="green",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(volatile_acidity[quality_tier=="Poor"])),color="blue",linetype="dashed",lwd=1) +
  xlab(label = "Volatile Acidity Level") +
  ylab(label = NULL) +
  ggtitle("Distribution of Volatile Acidity Levels")+
  theme_economist()

# 3. Density
ggplot(wine, aes(x = density,fill=Quality)) + 
  geom_density(alpha=0.25) +
  geom_vline(aes(xintercept=mean(density[quality_tier=="Excellent"])),color="red",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(density[quality_tier=="Mediocre"])),color="green",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(density[quality_tier=="Poor"])),color="blue",linetype="dashed",lwd=1) +
  xlab(label = "Density") +
  ylab(label = NULL) +
  ggtitle("Distribution of Density")+
  theme_economist()

# 4. Fixed Acidity
ggplot(wine, aes(x = fixed_acidity, fill=Quality)) + 
  geom_density(alpha=0.25, show.legend = FALSE) +
  geom_vline(aes(xintercept=mean(fixed_acidity[quality_tier=="Excellent"])),color="red",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(fixed_acidity[quality_tier=="Mediocre"])),color="green",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(fixed_acidity[quality_tier=="Poor"])),color="blue",linetype="dashed",lwd=1) +
  xlab(label = "Fixed Acidity Level") +
  ylab(label = NULL) +
  ggtitle("Fixed Acidity Levels")+
  theme_economist()

# 5. Citric Acid
ggplot(wine, aes(x = citric_acid,fill=Quality)) + 
  geom_density(alpha=0.25, show.legend = FALSE) +
  geom_vline(aes(xintercept=mean(citric_acid[quality_tier=="Excellent"])),color="red",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(citric_acid[quality_tier=="Mediocre"])),color="green",linetype="dashed",lwd=1) +
  geom_vline(aes(xintercept=mean(citric_acid[quality_tier=="Poor"])),color="blue",linetype="dashed",lwd=1) +
  xlab(label = "Citric Acidity Level") +
  ylab(label = NULL) +
  ggtitle("Citric Acid Levels")+
  theme_economist()
