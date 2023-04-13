##Men's Basketball Game-by-Game Analysis

############################################################################################
####Format the Data#########################################################################
############################################################################################

write.csv(MBBGBG, "C://users//lazlo//Desktop//2014_2021MBBGBG.csv", row.names = F)

MBBGBG <- read.csv("2014_2021MBBGBG.csv")

##Save Total Tickets Scanned for later
##Only Data from 2021 Available
##Previous data is unavailable due to constant system migration
##Also Missing data for 2 of the 2021 games
TotalTicketsScanned <- MBBGBG$TotalScannedTickets
TotalTicketsScanned <- na.omit(TotalTicketsScanned)
TotalTicketsScanned
##Deletes TotalTicketsScanned Column from data set
MBBGBG <- MBBGBG[, -14]

##Order Factor Day of Week
MBBGBG$GameDoW <- ordered(MBBGBG$GameDoW, levels = c("Sunday", "Monday", "Tuesday", 
                                                    "Wednesday", "Thursday", 
                                                    "Friday", "Saturday"))

##Order Factor Game Time
MBBGBG$GameTime <- ordered(MBBGBG$GameTime, levels = c("12:00", "13:00", "14:00",
                                                       "15:00", "16:00", "17:00", 
                                                       "19:00", "20:00"))

##Create isSpecial Variable ##Binary to check Occasion Status
isSpecial <- rep("Yes", 95)

for (i in 1:nrow(MBBGBG)) {
  if (Occasion[i] == "Home") {
    isSpecial[i] <- "No"
  }
  else {
    isSpecial[i] <- "Yes"
  }
}

##Set isSpecial as a Factor
isSpecial <- as.factor(isSpecial)

##Set Occasion as an Ordered Factor
MBBGBG$Occasion <- ordered(MBBGBG$Occasion, levels = c("Home", "Home-Opener", 
                                                       "Senior Day", "Winter Homecoming",
                                                       "Toy's For Tots", "Turnpike Trophy",
                                                       "Salute to Service", '"Color" Out',
                                                       "Brooklyn Hoops Invitational", 
                                                       "Banner Raising Ceremony"))

attach(MBBGBG)

View(MBBGBG)

############################################################################################
####Data Setup: Season Totals###############################################################
############################################################################################

##Because UMASS Amherst v. Holy Cross played at the DCU Center in 2016, 
##The Total Tickets Sold, Total Ticket Revenue, and Total Tickets Out
##Are unavailable for this game, therefore, Season Totals for these metrics are 
##Unable to be calculated, and only estimated for this season. 
SeasonTotals2016 <- rep(0, 3)
SeasonTotals2016[1] <- 9754
SeasonTotals2016[2] <- 80258.07
SeasonTotals2016[3] <- 15884

##Total Number of Tickets Sold (i.e. Those that earned revenue) for each season
SeasonTotalTicketsSold <- rep(0, 7)
for(i in 1:6) {
    SeasonTotalTicketsSold[i] <- sum(TotalTicketsSold[Season == 2013 + i])
}
SeasonTotalTicketsSold[3] <- SeasonTotals2016[1]
SeasonTotalTicketsSold[7] <- sum(TotalTicketsSold[Season == 2021])

##Total Amount of Ticket Revenue each Season
SeasonTotalTicketRevenue <- rep(0, 7)
for(i in 1:6) {
    SeasonTotalTicketRevenue[i] <- sum(TotalTicketRevenue[Season == 2013 + i])
}
SeasonTotalTicketRevenue[3] <- SeasonTotals2016[2]
SeasonTotalTicketRevenue[7] <- sum(TotalTicketRevenue[Season == 2021])

##Total Number of Tickets Out each Season
SeasonTotalTicketsOut <- rep(0, 7)
for(i in 1:6) {
  SeasonTotalTicketsOut[i] <- sum(TotalTicketsOut[Season == 2013 + i])
}
SeasonTotalTicketsOut[3] <- SeasonTotals2016[3]
SeasonTotalTicketsOut[7] <- sum(TotalTicketsOut[Season == 2021])

############################################################################################
####Part I: Season Totals###################################################################
############################################################################################

##Define a Year Variable
Year <- c(2014, 2015, 2016, 2017, 2018, 2019, 2021)

plot(Year, SeasonTotalTicketsSold, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Tickets Sold each Season", 
     xlab = "Season", ylab = "Amount of Tickets")

plot(Year, SeasonTotalTicketRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Revenue Earned each Season", 
     xlab = "Season", ylab = "Revenue ($)")

plot(Year, SeasonTotalTicketsOut, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Tickets Out each Season", 
     xlab = "Season", ylab = "Amount of Tickets")

############################################################################################
####Section II: Single-Game Season Breakdowns####################################################################
############################################################################################

#############################################################################################
####Part I#####################################################################################
###################################################################################################

cols = c("pink", "red", "light green", "light blue", "blue", "violet", "purple")

##Boxplot of Game Ticket Sales by Season
boxplotTTSS <- boxplot(TotalTicketsSold ~ Season, data = MBBGBG, 
                       main = "Single-Game Ticket Sale Spread each Season",
                       xlab = "Season", ylab = "Ticket Sales", 
                       col = cols, na.action = NULL)

##Statistics
TTSS.stats <- boxplotTTSS$stats
colnames(TTSS.stats) <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021")
rownames(TTSS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSS.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldSeasonBoxplotStatistics.csv", 
          row.names = T)


##Boxplot of Game Revenue by Season
boxplotTTRS <- boxplot(TotalTicketRevenue ~ Season, data = MBBGBG, 
                       main = "Single-Game Ticket Revenue Spread each Season", 
                       xlab = "Season", ylab = "Revenue ($)", 
                       col = cols, na.action = NULL)

##Boxplot Statistics
TTRS.stats <- boxplotTTRS$stats
colnames(TTRS.stats) <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021")
rownames(TTRS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTRS.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketRevenueSeasonBoxplotStatistics.csv", 
          row.names = T)


##Boxplot of Game Total Tickets Out by Season
boxplotTTOS <- boxplot(TotalTicketsOut ~ Season, data = MBBGBG,
                       main = "Single-Game Tickets Out Spread by each Season", 
                       xlab = "Season", ylab = "Tickets Out", 
                       col = cols, na.action = NULL)

##Boxplot Statistics
TTOS.stats <- boxplotTTOS$stats
colnames(TTOS.stats) <- c("2014", "2015", "2016", "2017", "2018", "2019", "2021")
rownames(TTOS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv
write.csv(TTOS.stats, "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsOutSeasonBoxplotStatistics.csv", 
          row.names = T)


############################################################################################
####Part II: Other Single-Game Ticket Metrics###############################################
#########################################################################################

##Boxplot of Game Total Tickets Sold by Day of Week
cols <- rainbow(length(factor(levels(GameDoW))))
boxplotTTSDOW <- boxplot(TotalTicketsSold ~ GameDoW, data = MBBGBG, 
                         main = "Single-Game Tickets Sold by Day of Week",
                         xlab = "Day of Week", ylab = "Tickets Sold", 
                         col = cols, na.action = NULL)

##Statistics
TTSDOW.stats <- boxplotTTSDOW$stats
colnames(TTSDOW.stats) <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday")
rownames(TTSDOW.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSDOW.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldDayofWeekBoxplotStatistics.csv", 
          row.names = T)

##Boxplot of Single Game Tickets Sold by Game Time
cols <- rainbow(length(factor(levels(GameTime))))
boxplotTTSGT <- boxplot(TotalTicketsSold ~ GameTime, data = MBBGBG, 
                        main = "Single-Game Tickets Sold by Game Time", 
                        xlab = "Game Time", ylab = "Tickets Sold", 
                        col = cols, na.action = NULL)

##Statistics
TTSGT.stats <- boxplotTTSGT$stats
colnames(TTSGT.stats) <- c("12:00", "13:00", "14:00", "15:00", "16:00", 
                           "17:00", "19:00", "21:00")
rownames(TTSGT.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSGT.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldGameTimeBoxplotStatistics.csv", 
          row.names = T)

##Boxplot of Single Game Tickets Sold by isSpecial
cols <- rainbow(length(factor(levels(isSpecial))))
boxplotTTSiS <- boxplot(TotalTicketsSold ~ isSpecial, data = MBBGBG, 
                       main = "Single-Game Tickets Sold by Day of Week", 
                       xlab = "Occasion (Y/N)", ylab = "Tickets Sold", 
                       col = cols, na.action = NULL)

##Statistics
TTSiS.stats <- boxplotTTSiS$stats
colnames(TTSiS.stats) <- c("No", "Yes")
rownames(TTSiS.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSiS.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldisSpecialBoxplotStatistics.csv", 
          row.names = T)

##Boxplot of Single Game Tickets Sold by Occasion
cols <- rainbow(length(factor(levels(MBBGBG$Occasion))))
boxplotTTSO <- boxplot(TotalTicketsSold ~ MBBGBG$Occasion, data = MBBGBG, 
                       main = "Single-Game Tickets Sold by Occasion", 
                       xlab = "Occasion", ylab = "Tickets Sold", 
                       col = cols, na.action = NULL)

##Statistics
TTSO.stats <- boxplotTTSO$stats
colnames(TTSO.stats) <- c("Home", "Home-Opener", 
                          "Senior Day", "Winter Homecoming",
                          "Toy's for Tots", "Turnpike Trophy",
                          "Salute to Service", '"Color" Out',
                          "Brooklyn Hoops Invitational", 
                          "Banner Raising Ceremony")
rownames(TTSO.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSO.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldOccasionBoxplotStatistics.csv", 
          row.names = T)

#######################################################################################
####Part III: Conference Games#########################################################
#######################################################################################

cols <- c("red", "blue")

##Boxplot Total Tickets Sold for Conference and Non-Conference Games
boxplotTTSCG <- boxplot(TotalTicketsSold ~ ConferenceGame, data = MBBGBG, 
                        main = "Total Tickets Sold by Conference Game", 
                        xlab = "Conference Game", ylab = "Tickets Sold", 
                        col = cols, na.action = NULL)

##Statistics
TTSCG.stats <- boxplotTTSCG$stats
colnames(TTSCG.stats) <- c("No", "Yes")
rownames(TTSCG.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTSCG.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsSoldConferenceGameBoxplotStatistics.csv", 
          row.names = T)

##Boxplot of Total Ticket Revenue for Conference and Non-Conference Game
boxplotTTRCG <- boxplot(TotalTicketRevenue ~ ConferenceGame, data = MBBGBG, 
                        main = "Total Ticket Revenue by Conference Game", 
                        xlab = "Conference Game", ylab = "Ticket Revenue", 
                        col = cols, na.action = NULL)

##Statistics
TTRCG.stats <- boxplotTTRCG$stats
colnames(TTRCG.stats) <- c("No", "Yes")
rownames(TTRCG.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTRCG.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketRevenueConferenceGameBoxplotStatistics.csv", 
          row.names = T)

##Boxplot of Total Tickets Out for Conference and Non-Conference Games
boxplotTTOCG <- boxplot(TotalTicketsOut ~ ConferenceGame, data = MBBGBG, 
                        main = "Total Tickets Old by Conference Game", 
                        xlab = "Conference Game", ylab = "Tickets Out", 
                        col = cols, na.action = NULL)

##Statistics
TTOCG.stats <- boxplotTTOCG$stats
colnames(TTOCG.stats) <- c("No", "Yes")
rownames(TTOCG.stats) <- c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum")

##Write the Summary Statistics to a .csv to use in R Markdown Table Generator
write.csv(TTOCG.stats, 
          "C://users//lazlo//Desktop//2014_2021MBBGBG//TotalTicketsOutConferenceGameBoxplotStatistics.csv", 
          row.names = T)