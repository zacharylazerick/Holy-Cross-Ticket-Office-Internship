##2018_2021 Football Game-by-Game Report
FBGBG <- read.csv("2021_2022RevenueReportFBGamebyGame.csv")
FBGBG <- FBGBG[-c(4), ] ##Deletes the Polar Park Game from Data Set
FBGBG <- subset(FBGBG, select = -c(18:24)) ##Deletes Additional Revenue Source Sales Data
attach(FBGBG)

PercentTicketsSold <- TotalTicketsSold / TotalTicketsOut
PercentSeasonRevenue <- SeasonTicketRevenue / TotalTicketRevenue
PercentNewRevenue <- 1 - PercentSeasonRevenue
AtdInflation = Attendance / TotalTicketsOut

FBGBG <- data.frame(FBGBG, PercentTicketsSold, PercentSeasonRevenue,
                    PercentNewRevenue, AtdInflation)

attach(FBGBG)

##Recode Variables to use in Data Visualization and Analysis
FBGBG$ConferenceGame <- as.factor(FBGBG$ConferenceGame)
FBGBG$Occasion <- as.factor(FBGBG$Occasion)
FBGBG$Weather <- as.factor(FBGBG$Weather)


###############################################################################################
##Boxplot Analysis of Total Ticket Revenue for Conference and Non-Conference Games

##Boxplot
boxplotTTRCG <- boxplot(TotalTicketRevenue ~ ConferenceGame, data = FBGBG, 
                        xlab = "Conference Game", ylab = "Total Revenue From Ticket Sales", 
                        main = "Total Ticket Revenue for Conference and Non-Conference Games")

##Boxplot Summary Statistics
TTRCG.stats <- boxplotTTRCG$stats
##Change Names of Rows and Columns for Easy Interpretability
colnames(TTRCG.stats) <- c("No", "Yes")
rownames(TTRCG.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
##Output the Summary Statistics
TTRCG.stats

##Boxplot Confidence Intervals
TTRCG.conf <- boxplotTTRCG$conf
##Change Names of Rows and Columns for Easy Interpretability
colnames(TTRCG.conf) <- c("No", "Yes")
rownames(TTRCG.conf) <- c("Lower Bound", "Upper Bound")
##Output the Confidence Intervals
TTRCG.conf


##Boxplot Analysis of Total Ticket Revenue per Season

##Boxplot
boxplotTTRS <- boxplot(TotalTicketRevenue ~ Season, data = FBGBG, 
                       xlab = "Season", ylab = "Total Ticket Revenue From Ticket Sales", 
                       main = "Total Ticket Revenue per Season")
##Boxplot Summary Statistics
TTRS.stats <- boxplotTTRS$stats
##Change Names of Rows and Columns for Easy Interpretability
colnames(TTRS.stats) <- c("2018", "2019", "2021")
rownames(TTRS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
##Output Summary Statistics
TTRS.stats

##Boxplot Confidence Intervals
TTRS.conf <- boxplotTTRS$conf
##Change Names of Rows and Columns for Easy Interpretability
colnames(TTRS.conf) <- c("2018", "2019", "2021")
rownames(TTRS.conf) <- c("Lower Bound", "Upper Bound")
##Output Confidence Intervals
TTRS.conf


##Boxplot Analysis of Percent Tickets Sold per Season

##Boxplot
boxplotPTSS <- boxplot(PercentTicketsSold ~ Season, data = FBGBG, 
                       xlab = "Season", ylab = "Percent Tickets Sold", 
                       main = "Percent Tickets Sold per Season")

##Summary Statistics
PTSS.stats <- boxplotPTSS$stats
colnames(PTSS.stats) <- c("2018", "2019", "2021")
rownames(PTSS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
PTSS.stats

##Confidence Intervals
PTSS.conf <- boxplotPTSS$conf
colnames(PTSS.conf) <- c("2018", "2019", "2021")
rownames(PTSS.conf) <- c("Lower Bound", "Upper Bound")
PTSS.conf


##Boxplot Analysis of Percent New Revenue for Conference and Non-Conference Games
boxplotPNRCG <- boxplot(PercentNewRevenue ~ ConferenceGame, data = FBGBG, 
                        xlab = "Conference Game", ylab = "Percent New Revenue", 
                        main = "Percent New Revenue for Conference and Non-Conference Games")

##Summary Statistics
PNRCG.stats <- boxplotPNRCG$stats
colnames(PNRCG.stats) <- c("No", "Yes")
rownames(PNRCG.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
PNRCG.stats

##Confidence Intervals
PNRCG.conf <- boxplotPNRCG$conf
colnames(PNRCG.conf) <- c("No", "Yes")
rownames(PNRCG.conf) <- c("Lower Bound", "Upper Bound")
PNRCG.conf


##Boxplot Analysis of Percent New Revenue per Season
boxplotPNRS <- boxplot(PercentNewRevenue ~ Season, data = FBGBG, 
                       xlab = "Season", ylab = "Percent New Revenue", 
                       main = "Percent New Revenue per Season")

##Summary Statistics
PNRS.stats <- boxplotPNRS$stats
colnames(PNRS.stats) <- c("2018", "2019", "2021")
rownames(PNRS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
PNRS.stats

##Confidence Intervals
PNRS.conf <- boxplotPNRS$conf
colnames(PNRS.conf) <- c("2018", "2019", "2021")
rownames(PNRS.conf) <- c("Lower Bound", "Upper Bound")
PNRS.conf


##Attendance vs. Total Ticket Revenue
plot(Attendance, TotalTicketRevenue, type = "p", 
     xlab = "Attendance", ylab = "TotalTicketRevenue ($)")
lm.fit.TRevenue <- lm(TotalTicketRevenue ~ Attendance, data = FBGBG)
abline(lm.fit.TRevenue, col = "green", lwd = 2)
abline(h = mean(TotalTicketRevenue), v = mean(Attendance), col = "blue", lwd = 1)

##Attendance vs. Total Tickets Sold
plot(Attendance, TotalTicketsSold, type = "p", 
     xlab = "Attendance", ylab = "Total Tickets Sold")
lm.fit.TRevenue6 <- lm(TotalTicketsSold ~ Attendance, data = FBGBG)
abline(lm.fit.TRevenue6, col = "green", lwd = 2)
abline(h = mean(TotalTicketsSold), v = mean(Attendance), col = "blue", lwd = 1)

mean(TotalTicketsSold >= Attendance)

##Attendance vs. Total Tickets Out
plot(Attendance, TotalTicketsOut, type = "p", 
     xlab = "Attendance", ylab = "Total Tickets Out")
lm.fit.TRevenue5 <- lm(TotalTicketsOut ~ Attendance, data = FBGBG)
abline(lm.fit.TRevenue5, col = "green", lwd = 2)
abline(h = mean(TotalTicketsOut), v = mean(Attendance), col = "blue", lwd = 1)

mean(Attendance >= TotalTicketsOut)

mean(AtdInflation)

boxplotINFS <- boxplot(AtdInflation ~ Season, data = FBGBG, 
                       xlab = "Season", ylab = "Attendance Over-Reported by", 
                       main = "Attendance Inflation per Season")
INFS.stats <- boxplotINFS$stats
colnames(INFS.stats) <- c("2018", "2019", "2021")
rownames(INFS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")
INFS.stats

INFS.conf <- boxplotINFS$conf
colnames(INFS.conf) <- c("2018", "2019", "2021")
rownames(INFS.conf) <- c("Lower Bound", "Upper Bound")
INFS.conf


##Percent Tickets Sold vs. Total Ticket Revenue
plot(PercentTicketsSold, TotalTicketRevenue, type = "p", 
     xlab = "Percent Tickets Sold", ylab = "Total Ticket Revenue")
lm.fit.TRevenue2 <- lm(TotalTicketRevenue ~ PercentTicketsSold, data = FBGBG)
abline(lm.fit.TRevenue2, col = "red", lwd = 2)
abline(h = mean(TotalTicketsSold), v = mean(PercentTicketsSold), col = "blue" , lwd = "1")


##Total Tickets Sold vs. Total Ticket Revenue
##Significant
plot(TotalTicketsSold, TotalTicketRevenue, type = "p", 
     xlab = "Total Tickets Sold", ylab = "Total Ticket Revenue ($)")
lm.fit.TRevenue3 <- lm(TotalTicketRevenue ~ TotalTicketsSold, data = FBGBG)
abline(reg = lm.fit.TRevenue3, col = "green", lwd = 2)
abline(v = mean(TotalTicketsSold), h = mean(TotalTicketRevenue), col = "blue", lwd = 1)





##Test 1
##Set Seed for Repeatability
set.seed(1)

##Split Data into a Training Set and a Test Set
##Test Set is everything that is not the Training Set
train <- sample(1:nrow(FBGBG), (nrow(FBGBG)/2))
FBTest <- FBGBG[-train, ]

##Store the True Values of Conference Game for the Test Set
ConferenceGameTest <- ConferenceGame[-train]

##Model 1
glm.fit.FBGBG1 <- glm(ConferenceGame ~ Temperature, 
                      data = FBGBG, family = binomial)

##Model 1 Posterior Probability of being a Conference Game
glm.probs.FBGBG1 <- predict(glm.fit.FBGBG1, FBTest, type = "response")


##Model 1 Conference Game Prediction 
##If the Posterior Probability is Greater than .5, Conference Game = "Yes"
FB.pred1 <- rep("No", (nrow(FBGBG)/2))
FB.pred1[glm.probs.FBGBG1 > .5] <- "Yes"

##Model 1 Predicted Conference Game vs. True Conference Game
table(FB.pred1, ConferenceGameTest)

##Model 1 Total Percentage of Correct Predictions
mean(FB.pred1 == ConferenceGameTest)

##Model 2
glm.fit.FBGBG2 <- glm(ConferenceGame ~ Temperature + Attendance, 
                      data = FBGBG, family = binomial)

##Model 2 Posterior Probability of being a Conference Game
glm.probs.FBGBG2 <- predict(glm.fit.FBGBG2, FBTest, type = "response")

##Model 2 Conference Game Prediction
##If the Posterior Probability is Greater than .5, Conference Game = "Yes" 
FB.pred2 <- rep("No", (nrow(FBGBG)/2))
FB.pred2[glm.probs.FBGBG2 > .5] <- "Yes"

##Model 2 Predicted Conference Game vs. True COnference Game
table(FB.pred2, ConferenceGameTest)

##Model 2 Total Percentage of Correct Predictions
mean(FB.pred2 == ConferenceGameTest)

##Model 3 
glm.fit.FBGBG3 <- glm(ConferenceGame ~ Temperature + Attendance + TotalTicketRevenue, 
                      data = FBGBG, family = binomial)

##Model 3 Posterior Probability of being a Conference Game
glm.probs.FBGBG3 <- predict(glm.fit.FBGBG3, FBTest, type = "response")

##Model 3 Conference Game Predictions
##If the Posterior Probability is Greater than .5, Conference Game = "Yes"
FB.pred3 <- rep("No", (nrow(FBGBG)/2))
FB.pred3[glm.probs.FBGBG3 > .5] <- "Yes"

##Model 3 Predicted Conference Games vs. True Conference Games
table(FB.pred3, ConferenceGameTest)

##Model 3 Total Percentage of Correct Predictions
mean(FB.pred3 == ConferenceGameTest)
```

```{r, include = F}
##Test 2
set.seed(4)

train <- sample(1:nrow(FBGBG), (nrow(FBGBG)/2))
FBTest <- FBGBG[-train, ]

ConferenceGameTest <- ConferenceGame[-train]

##Model 1
glm.fit.FBGBG1 <- glm(ConferenceGame ~ Temperature, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG1 <- predict(glm.fit.FBGBG1, FBTest, type = "response")

FB.pred1 <- rep("No", (nrow(FBGBG)/2))
FB.pred1[glm.probs.FBGBG1 > .5] <- "Yes"

table(FB.pred1, ConferenceGameTest)
mean(FB.pred1 == ConferenceGameTest)

##Model 2
glm.fit.FBGBG2 <- glm(ConferenceGame ~ Temperature + Attendance, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG2 <- predict(glm.fit.FBGBG2, FBTest, type = "response")

FB.pred2 <- rep("No", (nrow(FBGBG)/2))
FB.pred2[glm.probs.FBGBG2 > .5] <- "Yes"

table(FB.pred2, ConferenceGameTest)
mean(FB.pred2 == ConferenceGameTest)

##Model 3
glm.fit.FBGBG3 <- glm(ConferenceGame ~ Temperature + Attendance + TotalTicketRevenue, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG3 <- predict(glm.fit.FBGBG3, FBTest, type = "response")

FB.pred3 <- rep("No", (nrow(FBGBG)/2))
FB.pred3[glm.probs.FBGBG3 > .5] <- "Yes"

table(FB.pred3, ConferenceGameTest)
mean(FB.pred3 == ConferenceGameTest)

```

```{r, include = F}
##Test 3
set.seed(7)

train <- sample(1:nrow(FBGBG), (nrow(FBGBG)/2))
FBTest <- FBGBG[-train, ]

ConferenceGameTest <- ConferenceGame[-train]

##Model 1 
glm.fit.FBGBG1 <- glm(ConferenceGame ~ Temperature, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG1 <- predict(glm.fit.FBGBG1, FBTest, type = "response")

FB.pred1 <- rep("No", (nrow(FBGBG)/2))
FB.pred1[glm.probs.FBGBG1 > .5] <- "Yes"

table(FB.pred1, ConferenceGameTest)
mean(FB.pred1 == ConferenceGameTest)


##Model 2 
glm.fit.FBGBG2 <- glm(ConferenceGame ~ Temperature + Attendance, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG2 <- predict(glm.fit.FBGBG2, FBTest, type = "response")

FB.pred2 <- rep("No", (nrow(FBGBG)/2))
FB.pred2[glm.probs.FBGBG2 > .5] <- "Yes"

table(FB.pred2, ConferenceGameTest)
mean(FB.pred2 == ConferenceGameTest)


##Model 3
glm.fit.FBGBG3 <- glm(ConferenceGame ~ Temperature + Attendance + TotalTicketRevenue, 
                      data = FBGBG, family = binomial)

glm.probs.FBGBG3 <- predict(glm.fit.FBGBG3, FBTest, type = "response")

FB.pred3 <- rep("No", (nrow(FBGBG)/2))
FB.pred3[glm.probs.FBGBG3 > .5] <- "Yes"

table(FB.pred3, ConferenceGameTest)
mean(FB.pred3 == ConferenceGameTest)