##2016-2021 Football Season Ticket Revenue Report

##########################################################################################
##Initialization##########################################################################
##########################################################################################


##Read in the Data
FBSTRR <- read.csv("2016_2021FBSeasonTicketRevenueReport.csv")

##Attach for Easy Function Calls
attach(FBSTRR)

View(FBSTRR)

##Save any Changes or Calculations as a .csv File
write.csv(FBSTRR, "c://users/lazlo/Desktop/2016_2021FBSTRR.csv", row.names = F)


###########################################################################################
###########################################################################################



###########################################################################################
##Variable Creation########################################################################
###########################################################################################

##Recalculated Renewal Rate.
ReportedRenewalRate <- RenewalRate
for (i in 1:4) {
  RenewalRate[i] = RenewedSeasonTicketAccounts[i] / TotalSeasonTicketAccounts[i+1] * 100
}
RenewalRate <- round(RenewalRate, digits = 2)
RenewalRate
ReportedRenewalRate

##Difference in Reported vs. Calculated Renewal Rate
DiffRenewalRate <- (ReportedRenewalRate - RenewalRate)

##Absolute Value of the Difference in Renewal Rates
AbsDiffRenewalRate <- abs(DiffRenewalRate)
##Average Difference of Reported Renewal Rate
mean(AbsDiffRenewalRate)
##Average Difference of Renewal Rate, not counting 2016
mean(AbsDiffRenewalRate[1:4])

FBSTRR <- data.frame(FBSTRR, ReportedRenewalRate, TicketRenewalRate)

##########################################################################################
##########################################################################################


##########################################################################################
####Part I################################################################################
##########################################################################################

##Total Number of Season Ticket Accounts Per Year
plot(Year, TotalSeasonTicketAccounts, type = "l", col = "red", lwd = 3,
     main = "Total Number of Season Ticket Accounts per Year", 
     xlab = "Year", ylab = "Season Ticket Accounts")

##Renewed vs. New Season Ticket Accounts per Year
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(FBSTRR[c("RenewedSeasonTicketAccounts", "NewSeasonTicketAccounts")]),
        beside = T, 
        ylim = c(0, max(FBSTRR[c("RenewedSeasonTicketAccounts", "NewSeasonTicketAccounts")])*1.5),
        border = cols, col = cols, names.arg = FBSTRR$Year, 
        main = "Renewed vs. New Season Ticket Accounts per Year",
        xlab = "Year", ylab = "Number of Accounts")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Renewed Season Ticket Accounts", "New Season Ticket Accounts"))

##Average Account Share for Renewed Season Ticket Accounts
PercentRenewedAccounts = RenewedSeasonTicketAccounts / TotalSeasonTicketAccounts
mean(PercentRenewedAccounts)

##Total Number of Season Tickets Sold per Year
plot(Year, TotalSeasonTickets, type = "l", col = "red", lwd = 3, 
     main = "Total Number of Season Tickets Sold per Year", 
     xlab = "Year", ylab = "Season Tickets Sold")

##Renewed vs. New Season Tickets  Purchased per Year
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(FBSTRR[c("RenewedSeasonTickets", "NewSeasonTickets")]),
        beside = T, 
        ylim = c(0, max(FBSTRR[c("RenewedSeasonTickets", "NewSeasonTickets")])*1.5),
        border = cols, col = cols, names.arg = FBSTRR$Year, 
        main = "Renewed vs. New Season Tickets Purchased per Year",
        xlab = "Year", ylab = "Number of Tickets Sold")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Renewed Season Tickets", "New Season Tickets"))

##Average Ticket Share for Renewed Season Tickets
PercentRenewedTickets <- RenewedSeasonTickets / TotalSeasonTickets
mean(PercentRenewedTickets)

##Total Revenue Earned from Season Ticket Sales per Year
plot(Year, TotalSeasonTicketRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Revenue Earned from Season Tickets", 
     xlab = "Year", ylab = "Revenue ($)")

##Renewed vs. New Season Tickets Revenue per Year
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(FBSTRR[c("RenewedTicketRevenue", "NewTicketRevenue")]),
        beside = T, 
        ylim = c(0, max(FBSTRR[c("RenewedTicketRevenue", "NewTicketRevenue")])*1.5),
        border = cols, col = cols, names.arg = FBSTRR$Year, 
        main = "Renewed vs. New Season Ticket Revenue per Year",
        xlab = "Year", ylab = "Revenue ($)")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Renewed Season Ticket Revenue", "New Season Ticket Revenue"))

##Average Renewed Season Ticket Revenue Share
PercentRenewedRevenue <- RenewedTicketRevenue / TotalSeasonTicketRevenue
mean(PercentRenewedRevenue)

#########################################################################################
#########################################################################################


#########################################################################################
#####Part II#############################################################################
#########################################################################################


##Renewal Rate per Year
plot(Year, RenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Renewal Rate per Year", 
     xlab = "Year", ylab = "Renewal Rate (%)")

##Average Renewal Rate
mean(RenewalRate)

##Renewal Rate per Year in Terms of Season Ticket Sales
TicketRenewalRate <- rep(0, 5)
for (i in 1:4) {
  TicketRenewalRate[i] = (RenewedSeasonTickets[i] / TotalSeasonTickets[i+1]) * 100
}
TicketRenewalRate[5] <- RenewalRate[5]
TicketRenewalRate <- round(TicketRenewalRate, digits = 2)
TicketRenewalRate

##Yearly Season Ticket Renewal Rate
plot(Year, TicketRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Renewal Rate per Year (Season Ticket Sales", 
     xlab = "Year", ylab = "Renewal Rate (%)")

##Difference in Account Renewal Rate and Season Ticket Renewal Rate
DiffRenewalRate <- RenewalRate - TicketRenewalRate
plot(Year, DiffRenewalRate, type = "l" , col = "red", lwd = 3, 
     main = "Difference in Account and Ticket Renewal Rates per Year", 
     xlab = "Year", ylab = "Difference (%)")

#########################################################################################
#########################################################################################


##Total Revenue Earned from Season Tickets and Parking Passes
plot(Year, TotalRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Revenue Earned", 
     xlab = Year, ylab = "Revenue ($)")

##Average Yearly Revenue Growth
mean(RevenueGrowth)

##Season Tickets vs. Parking Passes Revenue Share
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(FBSTRR[c("TotalSeasonTicketRevenue", "TotalParkingRevenue")]),
        beside = T, 
        ylim = c(0, max(FBSTRR[c("TotalSeasonTicketRevenue", "TotalParkingRevenue")])*1.5),
        border = cols, col = cols, names.arg = FBSTRR$Year, 
        main = "Season Tickets vs. Parking Passes Revenue Share per Year",
        xlab = "Year", ylab = "Revenue ($)")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Total Season Ticket Revenue", "Total Parking Revenue"))

##Average Season Ticket Revenue Share
PercentSeasonRevenue <- TotalSeasonTicketRevenue / TotalRevenue
mean(PercentSeasonRevenue)

##Season Tickets Purchased : Parking Passes Purchased

ratio2021 <- TotalSeasonTickets[1] / 429
ratio2021

ratio2019 <- TotalSeasonTickets[2] / 283
ratio2019

ratio2018 <- TotalSeasonTickets[3] / 279
ratio2018

ratio2017 <- TotalSeasonTickets[4] / 236
ratio2017

ratio2016 <- TotalSeasonTickets[5] / 228
ratio2016

##########################################################################################
##########################################################################################



FBPRR <- read.csv("2016_2021FBParkingRevenueReport.csv")
attach(FBPRR)
View(FBPRR)


##Total Parking Account and Pass Numbers (Includes Comp Accounts and Passes)

TotalBaseballParkingAccounts <- c(160, 175, 171, 190, 168)

TotalBaseballParkingPasses <- c(177, 205, 205, 237, 190)

TotalWestLotParkingAccounts <- c(23, 30, 19, 0, 0)

TotalWestLotParkingPasses <- c(34, 48, 73, 0, 0)

TotalFreshmanFieldParkingAccounts <- c(68, 50, 57, 25, 15)

TotalFreshmanFieldParkingPasses <- c(88, 93, 191, 55, 16)

TotalParkingGarageParkingAccounts <- c(126, 86, 102, 5, 20)

TotalParkingGarageParkingPasses <- c(138, 133, 191, 35, 22)

CompBaseballParkingAccounts <- TotalBaseballParkingAccounts - PaidBaseballParkingAccounts

CompBaseballParkingPasses <- TotalBaseballParkingPasses - PaidBaseballParkingPasses

CompWestLotParkingAccounts <- TotalWestLotParkingAccounts - PaidWestLotParkingAccounts

CompWestLotParkingPasses <- TotalWestLotParkingPasses - PaidWestLotParkingPasses

CompFreshmanFieldParkingAccounts <- TotalFreshmanFieldParkingAccounts - PaidFreshmanFieldParkingAccounts

CompFreshmanFieldParkingPasses <- TotalFreshmanFieldParkingPasses - PaidFreshmanFieldParkingPasses

CompParkingGarageParkingAccounts <- TotalParkingGarageParkingAccounts - PaidParkingGarageAccounts

CompParkingGarageParkingPasses <- TotalParkingGarageParkingPasses - PaidParkingGarageParkingPasses

TotalCompParkingAccounts <- CompBaseballParkingAccounts + CompWestLotParkingAccounts + 
  CompFreshmanFieldParkingAccounts + CompParkingGarageParkingAccounts

TotalCompParkingPasses <- CompBaseballParkingPasses + CompWestLotParkingPasses +
  CompFreshmanFieldParkingPasses + CompParkingGarageParkingPasses

TotalParkingAccounts <- TotalCompParkingAccounts + TotalPaidParkingAccounts

TotalParkingPasses <- TotalCompParkingPasses + TotalPaidParkingPasses


#########################################################################################

FBPRR <- data.frame(FBPRR, TotalBaseballParkingAccounts, TotalBaseballParkingPasses, 
                    TotalWestLotParkingAccounts, TotalWestLotParkingPasses, 
                    TotalFreshmanFieldParkingAccounts, TotalFreshmanFieldParkingPasses, 
                    TotalParkingGarageParkingAccounts, TotalParkingGarageParkingPasses, 
                    TotalCompParkingAccounts, TotalCompParkingPasses, 
                    TotalParkingAccounts, TotalParkingPasses)

write.csv(FBPRR, "c://users//lazlo//Desktop/2016_2021FBPRR.csv", row.names = F)


#########################################################################################
######Part 3#############################################################################
#########################################################################################
##Total Number of Parking Accounts
plot(Year, TotalParkingAccounts, type = "l", col = "red", lwd = 3, 
     main = "Total Number of Parking Accounts", 
     xlab = "Year", ylab = "Parking Accounts")

##Average year over year Parking Account Growth
for (i in 1:4) {
  PercentGrowth = TotalParkingAccounts[i] / TotalParkingAccounts[i+1]
}
mean(PercentGrowth)

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Paid Account Share
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBPRR[c("BaseballParkingAccounts", "WestLotAccounts", 
                  "FreshmanFieldAccounts", "ParkingGarageAccounts")]),
        beside = T, 
        ylim = c(0, max(FBPRR[c("BaseballParkingAccounts", "WestLotAccounts", 
                                "FreshmanFieldAccounts", "ParkingGarageAccounts")])*1.5),
        border = cols, col = cols, names.arg = FBPRR$Year, 
        main = "Baseball Lot vs. West Lot vs. Freshman Field vs. Parking Garage Account Share",
        xlab = "Year", ylab = "Number of Accounts")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))

##Average Percent of Baseball Accounts
PercentBaseballAccounts = BaseballParkingAccounts / TotalParkingAccounts
mean(PercentBaseballAccounts)

##Total Number of Paid Parking Passes Each Season
plot(Year, TotalPaidParkingPasses, type = "l", col = "red", lwd = 3, 
     main = "Total Number of Paid Parking Passes", 
     xlab = "Season", ylab = "Parking Passes")

##Average Percent Paid Parking Pass Growth
for (i in 1:4) {
  PercentGrowth = TotalPaidParkingPasses[i] / TotalPaidParkingPasses[i+1]
}
mean(PercentGrowth)

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Parking Pass Share
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBPRR[c("PaidBaseballParkingPasses", "PaidWestLotParkingPasses", 
                  "PaidFreshmanFieldParkingPasses", "PaidParkingGarageParkingPasses")]),
        beside = T, 
        ylim = c(0, max(FBPRR[c("PaidBaseballParkingPasses", 
                                "PaidWestLotParkingPasses",
                                "PaidFreshmanFieldParkingPasses",
                                "PaidParkingGarageParkingPasses")])*1.5),
        border = cols, col = cols, names.arg = FBPRR$Year, 
        main = "Individual Parking Lot Parking Pass Share",
        xlab = "Season", ylab = "Number of Parking Passes")
box()
legend("topright", fill = cols, cex = .75,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))

##Average Baseball Parking Pass Share
PercentBaseballPasses = PaidBaseballParkingPasses / TotalPaidParkingPasses
mean(PercentBaseballPasses)

##Total Revenue Earned from Parking Passes
plot(Year, TotalParkingRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Amount of Revenue Earned from Parking Passes", 
     xlab = "Year", ylab = "Revenue ($)")

##Average Yearly Growth for Parking Revenue
for (i in 1:4) {
  PercentGrowth = TotalParkingRevenue[i] / TotalParkingRevenue[i+1]
}
mean(PercentGrowth)

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Revenue
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBPRR[c("BaseballParkingRevenue", "WestLotParkingRevenue", 
                  "FreshmanFieldParkingRevenue", "ParkingGarageParkingRevenue")]),
        beside = T, 
        ylim = c(0, max(FBPRR[c("BaseballParkingRevenue", "WestLotParkingRevenue", 
                                "FreshmanFieldParkingRevenue", "ParkingGarageParkingRevenue")])*1.5),
        border = cols, col = cols, names.arg = FBPRR$Year, 
        main = "Baseball Lot vs. West Lot vs. Freshman Field vs. Parking Garage Revenue Share",
        xlab = "Year", ylab = "Revenue ($)")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))

##Average Percent of Season Ticket Holders that buy Parking
PercentParking <- TotalParkingAccounts / TotalSeasonTicketAccounts
PercentParking
mean(PercentParking)

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Revenue Share
##Pie Chart 
slices <- c(sum(BaseballParkingRevenue), sum(WestLotParkingRevenue), 
            sum(FreshmanFieldParkingRevenue), sum(ParkingGarageParkingRevenue))
labels <- c("Baseball", "West Lot", "Freshman Field", "Parking Garage")
pie(slices, labels = labels, main = "Individual Parking Lot Revenue Share", 
    col = c("red", "blue", "green", "purple"))

##Average Baseball Parking Revenue Share
PercentBaseballRevenue = BaseballParkingRevenue / TotalParkingRevenue
mean(PercentBaseballRevenue)

##Revenue Share Minus Baseball Parking
##Baseball pseudo-outlier, since costs at least 2x the others
slices <- c(sum(WestLotParkingRevenue), 
            sum(FreshmanFieldParkingRevenue), sum(ParkingGarageParkingRevenue))
labels <- c("West Lot", "Freshman Field", "Parking Garage")
pie(slices, labels = labels, main = "Individual Parking Lot Revenue Share", 
    col = c("blue", "green", "purple"))


###########################################################################################
###########################################################################################
#####Complimentary Parking Passes##########################################################
###########################################################################################

##Revenue Lost per Year in Comp Passes

BaseballRevenueLost <- rep(0, 5)
BaseballRevenueLost[1] <- 100 * CompBaseballParkingPasses[1] 
BaseballRevenueLost[2] <- 50 * CompBaseballParkingPasses[2]
BaseballRevenueLost[3] <- 50 * CompBaseballParkingPasses[3]
BaseballRevenueLost[4] <- 30 * CompBaseballParkingPasses[4]
BaseballRevenueLost[5] <- 25 * CompBaseballParkingPasses[5]

WestLotRevenueLost <- rep(0, 5)
WestLotRevenueLost[1] <- 35 * CompWestLotParkingPasses[1]
WestLotRevenueLost[2] <- 35 * CompWestLotParkingPasses[2]
WestLotRevenueLost[3] <- 35 * CompWestLotParkingPasses[3] 

FreshmanFieldRevenueLost <- rep(0, 5)
FreshmanFieldRevenueLost[1] <- 35 * CompFreshmanFieldParkingPasses[1]
FreshmanFieldRevenueLost[2] <- 25 * CompFreshmanFieldParkingPasses[2]
FreshmanFieldRevenueLost[3] <- 25 * CompFreshmanFieldParkingPasses[3]
FreshmanFieldRevenueLost[4] <- 25 * CompFreshmanFieldParkingPasses[4]
FreshmanFieldRevenueLost[5] <- 25 * CompFreshmanFieldParkingPasses[5]

ParkingGarageRevenueLost <- rep(0, 5)
ParkingGarageRevenueLost[1] <- 40 * CompParkingGarageParkingPasses[1]
ParkingGarageRevenueLost[2] <- 25 * CompParkingGarageParkingPasses[2]
ParkingGarageRevenueLost[3] <- 25 * CompParkingGarageParkingPasses[3]
ParkingGarageRevenueLost[4] <- 25 * CompParkingGarageParkingPasses[4]
ParkingGarageRevenueLost[5] <- 25 * CompParkingGarageParkingPasses[5]

TotalParkingRevenueLost <- rep(0, 5)
for (i in 1:5) {
  TotalParkingRevenueLost[i] <- BaseballRevenueLost[i] + WestLotRevenueLost[i] + 
    FreshmanFieldRevenueLost[i] + ParkingGarageRevenueLost[i]
}

FBCompRR <- data.frame(Year, CompBaseballParkingAccounts, CompBaseballParkingPasses, 
                       CompWestLotParkingAccounts, CompWestLotParkingPasses,
                       CompFreshmanFieldParkingAccounts, CompFreshmanFieldParkingPasses,
                       CompParkingGarageParkingAccounts, CompParkingGarageParkingPasses, 
                       BaseballRevenueLost, WestLotRevenueLost, FreshmanFieldRevenueLost,
                       ParkingGarageRevenueLost, TotalParkingRevenueLost)
attach(FBCompRR)

write.csv(FBCompRR, "c://users//lazlo//Desktop/2016_2021FBComplimentaryParkingRR.csv", row.names = F)

##Total Complimentary Parking Accounts Per Year
plot(Year, TotalCompParkingAccounts, type = "l", col = "red", lwd = 3, 
     main = "Total Complimentary Parking Accounts per Season", 
     xlab = "Season", ylab = "Number of Accounts")

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Comp Account Share
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBCompRR[c("CompBaseballParkingAccounts", 
            "CompWestLotParkingAccounts", 
            "CompFreshmanFieldParkingAccounts", 
            "CompParkingGarageParkingAccounts")]),
        beside = T, 
        ylim = c(min(FBCompRR[c("CompBaseballParkingAccounts", 
                                "CompWestLotParkingAccounts", 
                                "CompFreshmanFieldParkingAccounts", 
                                "CompParkingGarageParkingAccounts")] * 1.1), 
                 max(FBCompRR[c("CompBaseballParkingAccounts", 
                                   "CompWestLotParkingAccounts", 
                                   "CompFreshmanFieldParkingAccounts", 
                                   "CompParkingGarageParkingAccounts")] * 1.1)),
        border = cols, col = cols, names.arg = FBCompRR$Year, 
        main = "Baseball Lot, West Lot, Freshman Field, Parking Garage Comp Account Share",
        xlab = "Season", ylab = "Number of Passes")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))

##Total Complimentary Parking Passes Per Year
plot(Year, TotalCompParkingPasses, 
     main = "Total Complimentary Parking Passes per Season", 
     xlab = "Season", ylab = "Number of Parking Passes")

##Baseball vs. West Lot vs. Freshman Field vs. Parking Garage Comp Parking Passes Share
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBCompRR[c("CompBaseballParkingPasses", 
            "CompWestLotParkingPasses", 
            "CompFreshmanFieldParkingPasses", 
            "CompParkingGarageParkingPasses")]),
        beside = T, 
        ylim = c(0, max(FBCompRR[c("CompBaseballParkingPasses", 
                                   "CompWestLotParkingPasses", 
                                   "CompFreshmanFieldParkingPasses", 
                                   "CompParkingGarageParkingPasses")] * 1.5)),
        border = cols, col = cols, names.arg = FBCompRR$Year, 
        main = "Baseball Lot, West Lot, Freshman Field, Parking Garage Comp Parking Passes Share",
        xlab = "Season", ylab = "Number of Passes")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))


##Total Parking Revenue Lost from Complimentary Passes
plot(Year, TotalParkingRevenueLost, type = "l", col = "red", lwd = 3, 
     main = "Parking Revenue Lost from Complimentary Passes", 
     xlab = "Season", ylab = "Revenue ($)")

##Average Parking Revenue Lost per Season
mean(TotalParkingRevenueLost)

##Total Parking Revenue Lost from Complimentary Passes Individual Lot Breakdown
cols <- c("red", "blue", "green", "purple")
par(lwd = 1)
barplot(t(FBCompRR[c("BaseballRevenueLost", 
            "WestLotRevenueLost", 
            "FreshmanFieldRevenueLost", 
            "ParkingGarageRevenueLost")]),
        beside = T, 
        ylim = c(0, max(FBCompRR[c("BaseballRevenueLost", 
                                   "WestLotRevenueLost", 
                                   "FreshmanFieldRevenueLost", 
                                   "ParkingGarageRevenueLost")]) * 1.5),
        border = cols, col = cols, names.arg = FBCompRR$Year, 
        main = "Indiviual Parking Lot Comp Parking Revenue Lost",
        xlab = "Season", ylab = "Revenue ($)")
box()
legend("topright", fill = cols, cex = .5,
       legend = c("Baseball Lot", "West Lot", "Freshman Field", "Parking Garage"))

##Total Revenue Lost per Parking Lot in past 6 seasons
sum(BaseballRevenueLost)
sum(WestLotRevenueLost)
sum(FreshmanFieldRevenueLost)
sum(ParkingGarageRevenueLost)
