# ----------------------------------------------------------------------------------------------------------
#
# Why are Tinder App Store Ratings Declining?
# Dylan Wiwad
# March 18th, 2022
#
# ----------------------------------------------------------------------------------------------------------

# Set the working directory and retrieve the data
setwd("~/dropbox/work/postdoc/dating_apps/")
data <- read.csv("DatingAppReviewsDataset.csv", header=T)

# This are app ratings scraped from the google playstore--presumably all android users?
# Data spans 2013 - 2022

# Just look and see what our variables are and the type
summary(data)
cbind(colnames(data))

# Load most relevant packages
library(ggplot2)
library(wesanderson)
library(scales)
library(tidyverse)
library(scales)

# ----------------------------------------------------------------------------------------------------------
#
# What app has the most reviews?
#
# ----------------------------------------------------------------------------------------------------------

# Get the counts for each app
plyr::count(data$App)

# Specify my color palette I'm going to use for most figures
wes_pal <- wes_palette("Darjeeling2")

# Order the factors
data$App <- factor(data$App, levels = c("Hinge", "Bumble", "Tinder"))

# Plot number of ratings by app
ggplot(data, aes(App, fill = App)) + geom_bar() + 
  scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Number of Reviews") + 
  ggtitle("Reviews per App") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# ----------------------------------------------------------------------------------------------------------
#
# What is the average rating, by app?
#
# ----------------------------------------------------------------------------------------------------------

# Is there any missing data in the ratings?
sum(is.na(data$Rating))
plyr::count(data$Rating)

# There is one 0, drop it.
data <- data[which(data$Rating >= 1),]

# Clearly quite the bimodal distribution. Lots of 1s and 5s. Is this true by app?
ggplot(data, aes(Rating, fill = App)) + geom_histogram(position = "dodge") +
  scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Count") + 
  ggtitle("Distribution of Ratings by App") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# Yes, all the apps follow this sort of inverse u shape in their ratings. So what is the average?
ggplot(data, aes(App, Rating, fill = App)) + geom_bar(stat = "summary", fun.y = "mean") +
  scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  scale_y_continuous(limits=c(1,5), oob = rescale_none) +
  ylab("Average Rating") + 
  ggtitle("Average Ratings by App") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# So despite Hinge being the least used app, it has the highest rating. What is causing the disconnect?
# Why doesn't it have more traction despite being the highest rated app?
# Is Hinge just slow on the uptake?


# ----------------------------------------------------------------------------------------------------------
#
# Plotting the rating trajectory by time, over years
#
# ----------------------------------------------------------------------------------------------------------

# Get the date into a date format and bring out the year
head(data$Date.Time)
head(data$Year)

data$Date <- as.Date(data$Date.Time, format = "%d-%m-%Y")

data$Year <- format(as.Date(data$Date, format="%d/%m/%Y"),"%Y")
data$yr_mo <- format(as.Date(data$Date, format="%d/%m/%Y"),"%Y-%m")

# How many reviews per year?
plyr::count(data$Year)

# Growth by app?
ggplot(data, aes(Year, group=App)) + geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  aes(fill = as.factor(App)) +
  ylab("Number of Ratings") + 
  ggtitle("Number of Ratings (2013 - 2022)") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") + facet_wrap(~ data$App, nrow = 3)

# Rating over time by years
ggplot(data, aes(Year, Rating, color = App, group = App)) + geom_point(stat = "summary", fun.y = "mean", size = 3) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.5) + 
  scale_color_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  ylab("Mean Rating") + 
  ggtitle("Average Rating Over Time (2013 - 2022)") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

# Rating by month
ggplot(data, aes(yr_mo, Rating, color = App, group = App)) + geom_point(stat = "summary", fun.y = "mean", size = 3) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.5) + 
  scale_color_manual(values = c("#ECCBAE", "#046C9A", "#D69C4E")) +
  ylab("Mean Rating") + 
  ggtitle("Average Rating Over Time (2013 - 2022)") +
  xlab("Time (Monthly 2013 - 2022)") + 
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks = c("2014-01", "2015-01", "2016-01", "2017-01",
                              "2018-01", "2019-01", "2020-01", "2021-01",
                              "2022-01"))

# There are two standout interesting cases here--Tinder and Hinge.
# Hinge experienced a huge boom, both in number of ratings and in average ratings.
# Tinder, OTOH, has been steady on the number of ratings but has declined every year

# Select just the tinder data
tindat <- data[which(data$App == "Tinder"),]

# make date numeric for regression
tindat$time <- as.POSIXlt(tindat$Date, format = "%m/%d/%Y %H:%M:%S %p")
tindat$time <- as.numeric(tindat$time)

# Quickly check, is an mlm necessary?
library(lmerTest)
library(lme4)

# Compute the null model
nm <- lmer(scale(Rating) ~ 1 + (1|time), data = tindat)
summary(nm)

# How much variance in rating is explained by the time of the review?
ICC <- (0.06457*0.06457)/((0.06457*0.06457)+(0.93456*0.93456))
ICC

# Are Tinder ratings declining over time
mod <- lm(scale(Rating) ~ scale(time), data=tindat)
summary(mod)
confint(mod)

# checking assumptions
par(mfrow = c(2, 2))
plot(mod)

# Violates the assumption of normality - get robust standard errors
library(sjPlot)
tab_model(mod, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")

# Rating by month - Tinder
ggplot(tindat, aes(yr_mo, Rating, group = 1)) + 
  geom_point(stat = "summary", fun.y = "mean", size = 3, color = "#D69C4E") +
  geom_line(stat = "summary", size = 1.5, color = "#D69C4E") +
  ylab("Mean Rating (2013 - 2022)") + 
  ggtitle("Average Tinder Rating Over Time") +
  xlab("Time (Monthly 2013 - 2022)") + 
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank()) +
  geom_smooth(method = lm, se = T, color = "#046C9A") +
  scale_x_discrete(breaks = c("2014-01", "2015-01", "2016-01", "2017-01",
                              "2018-01", "2019-01", "2020-01", "2021-01",
                              "2022-01")) +
  annotate(geom = "text", x = "2021-01", y = 4, label = "ß = -0.12, p < .001,\n95% CI [-.127,-.122]")

# Tinder's rating has been falling over time
# ----------------------------------------------------------------------------------------------------------
#
# Why has Tinder's rating been falling over time?
#
# ----------------------------------------------------------------------------------------------------------

# Now we're going to turn to just the tinder data. What are the users saying about tinder?
# clean up for politeness.

library(stringr)

# Turn the reviews to lowercase, make sure they are character vectors
tindat$Review <- tolower(tindat$Review)
tindat$Review <- as.character(tindat$Review)

# How many words total and per review, prior to removing non-ascii text?
tindat$word_count <- 0
tindat$word_count <- sapply(strsplit(tindat$Review, " "), length)
sum(tindat$word_count) # 7.8 million words total
psych::describe(tindat$word_count) # average of 15 words per review

# Brings in our dictionary of stopwords from the TM package
library(tm)
stopwords <- stopwords('en')


# Removes any element from "stopwords" from our list
tindat$Review <- removeWords(tindat$Review, stopwords)

# Compute the positive and negative word frequencies
library(politeness)

# Get Politeness metrics
polite <- politeness(tindat$Review, num_mc_cores = 4)  

# Merge the politeness data in with the overall tinder data
tindat <- cbind(tindat, polite)

# Remove all the rows that are not ascii
tindat$Review_clean <- textclean::replace_non_ascii(tindat$Review, replacement = "", remove.nonconverted = TRUE)

# Recompute the word counts now that we have trimmed the text down
tindat$word_count <- 0
tindat$word_count <- sapply(strsplit(tindat$Review_clean, " "), length)
sum(tindat$word_count) # 4.4 million words total
psych::describe(tindat$word_count) # average of 8 words per review

# Compute the proportion of words that are positive and negative in each review
tindat$pos_freq <- tindat$Positive.Emotion/tindat$word_count
tindat$neg_freq <- tindat$Negative.Emotion/tindat$word_count

# Is the frequency of positive and negative words changing over time? 

modp <- lm(scale(pos_freq) ~ scale(time), data = tindat)
summary(modp)


modn <- lm(scale(neg_freq) ~ scale(time), data = tindat)
summary(modn)

confint(modp)
confint(modn)

# checking assumptions
par(mfrow = c(2, 2))
plot(modp)

par(mfrow = c(2, 2))
plot(modn)


# Violates the assumption of normality - get robust standard errors
tab_model(modp, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")
tab_model(modn, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")


# Have to make the data long to plot positive and negative words
library(reshape2)
cols <- c("X", "Year", "yr_mo", "Positive.Emotion", "Negative.Emotion", "Review_clean")
df <- tindat[cols]

df <- gather(tindat, Emotion, Count, Positive.Emotion:Negative.Emotion, factor_key = TRUE)

df$Emotion <- recode(df$Emotion, Positive.Emotion = "Positive")
df$Emotion <- recode(df$Emotion, Negative.Emotion = "Negative")

df$Year <- as.factor(df$Year)

# How many words total and per review?
df$word_count <- 0
df$word_count <- sapply(strsplit(df$Review_clean, " "), length)
df$rel_freq <- df$Count/df$word_count

# Plot positive and negative words over time.
ggplot(df, aes(Year, rel_freq, color = Emotion, group = Emotion)) + geom_point(stat = "summary", fun.y = "mean", size = 3) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.5) +
  scale_color_manual(values = c("#3B9AB2", "#F21A00")) +
  ylab("Relative Frequency") + 
  ggtitle("Relative Frequency of Emotions Over Time") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels=percent) + 
  geom_smooth(method = "lm", color = "black")
annotate(geom = "text", x = "2021", y = 26, label = "ß = -0.12, p < .001,\n95% CI [-.127,-.122]")

# There was a 40% increase in negative content in 2020 compared with 2019.
# This corresponds with the big increase in negative content in the reviews

# Why? What are the negative words being spoken?
# Pull out the clean review text for tokenizing
cols <- c("Review_clean")
review_text <- tindat[cols]


library(dplyr)

# Turn it into a tibble so I can tokenize
text_tok <- as_tibble(review_text, text = text)

# Tokenize
library(tidytext)
text_tok <-  text_tok %>%
  select(Review_clean) %>%
  unnest_tokens(word, Review_clean)

# Count the number of occurrences of each word
words <- text_tok %>% count(word, sort=TRUE)

# Let's just use the bing sentiment to classify each word as positive or negative and plot
# the top 20 most frequently occuring negative words
sent <- get_sentiments("bing")
merged <- merge(words, sent, by = "word")

neg <- merged[which(merged$sentiment == "negative"),]

neg <- neg[order(-neg$n),]

# I summed up the total occurrences of negative words for the denominator
neg$prop <- (neg$n / 308729)

pal <- wes_palette("Zissou1", 20, type = "continuous")

neg %>% 
  filter(n > 3260) %>% 
  mutate(word = reorder(word, prop)) %>% 
  ggplot(aes(word, prop, fill = word)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Relative Frequency ", title = "Top 20 Negative Words \n") +
  geom_text(aes(label = round(prop*100,2)), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position =  "none") +
  scale_y_continuous(labels=percent) +
  scale_fill_manual(values = pal)

# So this begs the question - has the word fake been increasing over time?
text_tok_date <- as_tibble(dy[cols], text = text)

text_tok_date <-  tindat %>%
  select(Review_clean, yr_mo, Year) %>%
  unnest_tokens(word, Review_clean)

words <- text_tok_date %>% count(word, yr_mo, sort=TRUE)

# Get the total review words per month
rev_words <- aggregate(n ~ yr_mo, words, sum)

# app is the number one word, drop it
#words <- words[which(words$word != "app"),]

# Let's just use the bing sentiment to classify each word as positive or negative and plot
# the top 20 most pos and neg words
sent <- get_sentiments("bing")
merged <- merge(words, sent, by = "word")
merged <- merge(merged, rev_words, by = "yr_mo")

neg <- merged[which(merged$sentiment == "negative"),]

neg <- neg[order(-neg$n),]

# Just as a test lets plot fake and error over time
l <- c("fake", "error")
fd <- neg[which(neg$word %in% c("fake", "error")),]

# Get the proportion of "fake" mentions
fd$word_prop <- (fd$n.x / fd$n.y)
fd$word <- as.factor(fd$word)

pal <- wes_palette("Zissou1", 2, type = "continuous")

ggplot(fd, aes(yr_mo, word_prop, color = word, group = word)) + 
  geom_point(stat = "summary", fun.y = "mean", size = 3,) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.5) +
  scale_color_manual(values = pal) +
  ylab("Relative Frequency'") + 
  ggtitle("Reviews complaining of 'Fake' & 'Error' Over Time") +
  xlab("Time (2013 - 2022)") +
  theme_minimal(base_size = 18) +
  scale_y_continuous(labels = percent) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank()) +
  geom_smooth(method = "lm") +
  scale_x_discrete(breaks = c("2014-01", "2015-01", "2016-01", "2017-01",
                              "2018-01", "2019-01", "2020-01", "2021-01",
                              "2022-01"))


fd$time <- as.Date(paste0(fd$yr_mo, '-01'), format='%Y-%m-%d')


# Model - are they changing over time?
e <- fd[which(fd$word == "error"),]
f <- fd[which(fd$word == "fake"),]

etime <- lm(scale(word_prop) ~ scale(time), data = e)
summary(etime)
confint(etime)

ftime <- lm(scale(word_prop) ~ scale(time), data = f)
summary(ftime)
confint(ftime)

# checking assumptions
par(mfrow = c(2, 2))
plot(etime)

par(mfrow = c(2, 2))
plot(ftime)


# Violates the assumption of normality - get robust standard errors
tab_model(etime, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")
tab_model(ftime, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")


# ----------------------------------------------------------------------------------------------------------
#
# Word Embeddings
#
# ----------------------------------------------------------------------------------------------------------


library(word2vec)

# set the seed for replicability
set.seed(123456789)

# train the model
model <- word2vec(x = tindat$Review_clean, type = "cbow", dim = 15, iter = 20)
embedding <- as.matrix(model)

# Get similarity ratings
embedding <- predict(model, c("fake", "error"), type = "embedding")
lookslike <- predict(model, c("fake", "error"), type = "nearest", top_n = 50)

# get the top words that are similar to fake and error in datafgrames
fake_sim <- data.frame(lookslike$fake)
error_sim <- data.frame(lookslike$error)

pal <- wes_palette("Zissou1", 50, type = "continuous")

# Plot the top 50 words similar to fake
a <- fake_sim %>% 
  mutate(term2 = reorder(term2, similarity)) %>% 
  ggplot(aes(term2, similarity, fill = term2)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Similarity ", title = "Top 50 Words Most Similar\nto 'Fake' \n") +
  geom_text(aes(label = round(similarity,2)), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position =  "none") + 
  scale_fill_manual(values = pal)

# Plot the top 50 words similar to error
b <- error_sim %>% 
  mutate(term2 = reorder(term2, similarity)) %>% 
  ggplot(aes(term2, similarity, fill = term2)) + 
  geom_col() +
  coord_flip() +
  labs(x = "", y = "\n Similarity ", title = "Top 50 Words Most Similar\nto 'Error' \n") +
  geom_text(aes(label = round(similarity,2)), hjust = 1.2, colour = "white", fontface = "bold") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position =  "none") + 
  scale_fill_manual(values = pal)

# Plot them together
library(ggpubr)
ggarrange(a, b, ncol = 2)

# Turn this into a dataset
embeddat <- data.frame(lookslike)

# Plot the similarity scores
fake_words <- fake_sim[which(fake_sim$term1 == 'fake'),]
error_words <- error_sim[which(error_sim$term1 == 'error'),]

words_to_use <- c(fake_words$term2, error_words$term2, "fake", "error")

# Okay now we have all the words that are in that embedding, let's call it over .9

fcd <- neg[which(neg$word %in% words_to_use),]

# Quickly make fake and crash lists to categorize
fl <- c(fake_words$term2, "fake")
cl <- c(error_words$term2, "error")

fcd$Category <- ""

fcd$Category[fcd$word %in% fl] <- "Spam"
fcd$Category[fcd$word %in% cl] <- "Bugs"


# Get the proportion of "fake" mentions
fcd$word_prop <- (fcd$n.x / fcd$n.y)

# Plot Spam and Bug words over time
test <- fcd %>%
  group_by(yr_mo, Category) %>%
  summarise(sum = sum(n.x))

cols <- c("yr_mo", "n.y")
mo_words <- fcd[cols]

test <- merge(test, mo_words, by = "yr_mo")
test$word_prop <- (test$sum / test$n.y)


pal <- wes_palette("Zissou1", 2, type = "continuous")

ggplot(test, aes(yr_mo, word_prop, color = Category, group = Category)) + 
  geom_point(stat = "summary", fun.y = "mean", size = 3,) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.5) +
  scale_color_manual(values = pal) +
  ylab("Relative Frequency") + 
  ggtitle("Reviews complaining of Bugs and Spam Over Time") +
  xlab("Time") +
  theme_minimal(base_size = 18) +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = percent) +
  geom_smooth(method = "lm") +
  scale_x_discrete(breaks = c("2014-01", "2015-01", "2016-01", "2017-01",
                              "2018-01", "2019-01", "2020-01", "2021-01",
                              "2022-01"))


test$time <- as.Date(paste0(test$yr_mo, '-01'), format='%Y-%m-%d')


# Model the change over time with linear regressions
e <- test[which(test$Category == "Bugs"),]
f <- test[which(test$Category == "Spam"),]

etime <- lm(scale(word_prop) ~ scale(time), data = e)
summary(etime)
confint(etime)

ftime <- lm(scale(word_prop) ~ scale(time), data = f)
summary(ftime)
confint(ftime)

# checking assumptions
par(mfrow = c(2, 2))
plot(etime)

par(mfrow = c(2, 2))
plot(ftime)

# Violates the assumption of normality and homoscedasticity - get robust standard errors
tab_model(etime, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")
tab_model(ftime, show.stat = T, show.se = T, vcov.fun = "vcovHC", vcov.type = "HC2")


# ------------------------------------------------------------------------------------------------------
#
# Final multilevel models
#
# ------------------------------------------------------------------------------------------------------

# Do I want to use a fixed versus random effects model?
# Given that we have no clustering variable other than year, a fixed effects model works here?

test <- distinct(test)
test$sum <- NULL


test_wide <-spread(test, Category, word_prop)

tindat <- merge(tindat, test_wide, by = "yr_mo")

# piecewise regression -> estimate the breakpoints over time
library(segmented)
fit <- lm(Rating ~ time.x, data = tindat)

# I'm predicting three breakpoints, so where are they?
segmented.fit <- segmented(fit, seg.Z = ~time, npsi = 3)


summary(segmented.fit)

# First decline
first <- tindat[which(tindat$time.x <= 1443107404),]

# mid 
mid <- tindat[which(tindat$time.x > 1443107404 & tindat$time.x <= 1539466868),]

# Second decline
second <- tindat[which(tindat$time.x > 1539466868 & tindat$time.x <= 1610511915),]

# last decline
final <- tindat[which(tindat$time.x > 1610511915),]

plyr::count(first$Date) # July 16 2013 to September 24 2015
plyr::count(mid$yr_mo) # September 25 2015 to October 13 2018
plyr::count(second$yr_mo) # October 14 2018 to January 13 2021
plyr::count(final$yr_mo) # January 14 2021 to February 18 2022


# models in each section
fmod <- lmer(scale(Rating) ~ scale(Bugs) + scale(Spam) + (1 | time.x), data = first)
mmod <- lmer(scale(Rating) ~ scale(Bugs) + scale(Spam) + (1 | time.x), data = mid)
smod <- lmer(scale(Rating) ~ scale(Bugs) + scale(Spam) + (1 | time.x), data = second)
finmod <- lmer(scale(Rating) ~ scale(Bugs) + scale(Spam)+ (1 | time.x), data = final)

summary(fmod)
confint(fmod)

summary(mmod)
confint(mmod)

summary(smod)
confint(smod)

summary(finmod)
confint(finmod)

library(car)
vif(fmod)
vif(mmod)
vif(smod)
vif(finmod)

diagnostic_plot <- sjPlot::plot_model(fmod, type = "diag")
diagnostic_plot[[1]] # q-q plot for normality
diagnostic_plot[[3]] # check again with normal curve
diagnostic_plot[[4]] # residuals vs. fitted for homoscedasticity

diagnostic_plot <- sjPlot::plot_model(smod, type = "diag")
diagnostic_plot[[1]] # q-q plot for normality
diagnostic_plot[[3]] # check again with normal curve
diagnostic_plot[[4]] # residuals vs. fitted for homoscedasticity



# checking assumptions
par(mfrow = c(2, 2))
plot(fmod)

par(mfrow = c(2, 2))
plot(smod)

# Violates the assumption of normality and homoscedasticity - get robust standard errors
library(clubSandwich)

rob_se <- sqrt(diag(vcovCR(fmod, type = "CR3")))
std_se <- sqrt(diag(vcov(fmod)))
cbind(rob_se, std_se)
coef_test(fmod, vcov = "CR3")


rob_se <- sqrt(diag(vcovCR(smod, type = "CR3")))
std_se <- sqrt(diag(vcov(smod)))
cbind(rob_se, std_se)
coef_test(smod, vcov = "CR3")


# Build the final plot of regression coeficients 
coefs <- c(-.140, -.083, .015, -.125)
low <- c(-.156, -.101, .002, -.137)
high <- c(-.124, -.066, .027, -.112)
group <- c("Bugs", "Spam", "Bugs", "Spam")
time <- c("Initial Ratings Decline", "Initial Ratings Decline", 
          "Second Ratings Decline", "Second Ratings Decline")

listoflists=list(coefs,low,high,group, time)

coef_dat <- as.data.frame(listoflists, col.names = c("coef", "low", "high", "group", "time"))

coef_dat$group <- factor(coef_dat$group, levels = c("Bugs", "Spam"))

coef_dat$time <- factor(coef_dat$time,levels = c("Initial Ratings Decline",
                                                 "Second Ratings Decline"))

ggplot(data = coef_dat, aes(group, coef, color = group)) + geom_point(size = 4, position = position_dodge(width = .8)) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, position = position_dodge(width = .8)) +
  geom_hline(yintercept = 0, linetype = "solid") +
  geom_hline(yintercept = -.10, linetype = "longdash") +
  ylab("Beta Weight") +
  ggtitle("Standardized Effect of Bug and Spam Complaints\nOn Rating Across Time") +
  xlab("Natural Language Content of the Reviews") +
  theme_bw(base_size = 18) + theme(panel.border = element_blank(), axis.line = element_line(colour = "black"),
                                   plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ time, nrow = 1) +
  scale_color_manual("group", values = c("Bugs" = "#232066", "Spam" = "#E91D0E"))













