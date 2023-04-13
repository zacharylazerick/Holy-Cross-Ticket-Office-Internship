##Men's Basketball Revenue Analysis
write.csv(MBBRR, "c://users//lazlo//Desktop//2016_2021MBBRR.csv", row.names = F)

##Read in the Data
MBBRR <- read.csv("2016_2021MBBRR.csv")
##Attach for Easy Function Calls
attach(MBBRR)
##Delete Renewal Rate Column from the Data Set
OldRenewalRate <- RenewalRate
AccountRenewalRate <- RenewalRate
TicketRenewalRate <- RenewalRate
MBBRR <- MBBRR[, -c(5)]

View(MBBRR)

###########################################################################################
#######Section I: Season Ticket Analysis###################################################
###########################################################################################

###########################################################################################
#######Part I: Total Season Ticket Metrics#################################################
###########Renewed vs. New Breakdowns######################################################
###########################################################################################

##Total Season Ticket Accounts each Season
plot(Season, TotalSeasonTicketAccounts, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Accounts each Season", 
     xlab = "Season", ylab = "Accounts")

##Percent Decrease in Total Season Ticket Accounts
PercentDrop <- rep(0, 4)
for (i in 1:4) { 
  PercentDrop[i] = TotalSeasonTicketAccounts[i] / TotalSeasonTicketAccounts[i+1]
}
1 - mean(PercentDrop)

##Renewed vs. New Season Ticket Accounts
cols = c("red", "blue")
par(lwd = 1)
barplot(t(MBBRR[c("SeasonTicketAccountsRenewed", "NewSeasonTicketAccounts")]),
        beside = T, 
        ylim = c(0, max(MBBRR[c("SeasonTicketAccountsRenewed", 
                                "NewSeasonTicketAccounts")]) * 1.5), 
        border = cols, col = cols, names.arg = MBBRR$Season, 
        main = "Renewed vs. New Season Ticket Accounts each Season", 
        xlab = "Season", ylab = "Accounts")
box()
legend("topright", fill = cols, cex = .75, 
       legend = c("Renewed Accounts", "New Accounts"))

##Average Renewed Season Ticket Accounts Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = SeasonTicketAccountsRenewed[i] / SeasonTicketAccountsRenewed[i+1]
}
1 - mean(PercentDrop)

##Average New Season Ticket Accounts Percent Increase
PercentGrowth <- rep(0, 4)
for (i in 1:4) { 
  PercentGrowth[i] = NewSeasonTicketAccounts[i] / NewSeasonTicketAccounts[i+1]
}
mean(PercentGrowth)

##Total Season Tickets Purchased each Season
plot(Season, TotalSeasonTickets, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Purchased each Season", 
     xlab = "Season", ylab = "Tickets Purchased")

##Average Percent Drop of Total Season Ticket Sales
for (i in 1:4) { 
  PercentDrop[i] = TotalSeasonTickets[i] / TotalSeasonTickets[i+1]
}
1 - mean(PercentDrop)

##Renewed vs. New Season Tickets Purchased
cols = c("red", "blue")
par(lwd = 1)
barplot(t(MBBRR[c("SeasonTicketsRenewed", "NewSeasonTickets")]),
        beside = T, 
        ylim = c(0, max(MBBRR[c("SeasonTicketsRenewed", "NewSeasonTickets")]) * 1.5), 
        border = cols, col = cols, names.arg = MBBRR$Season, 
        main = "Renewed vs. New Season Tickets Purchased each Season", 
        xlab = "Season", ylab = "Tickets Purchased")
box()
legend("topright", fill = cols, cex = .75, 
       legend = c("Renewed Tickets", "New Tickets"))

##Average Renewed Season Tickets Sold Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = SeasonTicketsRenewed[i] / SeasonTicketsRenewed[i+1]
}
1 - mean(PercentDrop)

##Average New Season Tickets Sold Percent Growth
for (i in 1:4) { 
  PercentGrowth[i] = NewSeasonTickets[i] / NewSeasonTickets[i+1]
}
mean(PercentGrowth)

##Total Season Ticket Revenue each Season
plot(Season, TotalSeasonTicketRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Revenue each Season", 
     xlab = "Season", ylab = "Revenue ($)")

##Average Total Season Ticket Revenue Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = TotalSeasonTicketRevenue[i] / TotalSeasonTicketRevenue[i+1]
}
1 - mean(PercentDrop)

##Renewed vs. New Season Ticket Revenue
cols = c("red", "blue")
par(lwd = 1)
barplot(t(MBBRR[c("RenewedTicketRevenue", "NewTicketRevenue")]),
        beside = T, 
        ylim = c(0, max(MBBRR[c("RenewedTicketRevenue", "NewTicketRevenue")]) * 1.5), 
        border = cols, col = cols, names.arg = MBBRR$Season, 
        main = "Renewed vs. New Season Ticket Revenue each Season", 
        xlab = "Season", ylab = "Revenue ($)")
box()
legend("topright", fill = cols, cex = .75, 
       legend = c("Renewed Revenue", "New Revenue"))

##Average Renewed Season Ticket Revenue Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = RenewedTicketRevenue[i] / RenewedTicketRevenue[i+1]
}
1 - mean(PercentDrop)

##Average New Season Ticket Revenue Percent Growth
for (i in 1:4) { 
  PercentGrowth[i] = NewTicketRevenue[i] / NewTicketRevenue[i+1]
}
mean(PercentGrowth)

############################################################################################
#######Part II: Renewal Rate################################################################
#######Accounts and Tickets#################################################################
############################################################################################


##Reported Renewal Rate
OldRenewalRate

##Recalculate Account Renewal Rate
for (i in 1:4) {
  AccountRenewalRate[i] <- SeasonTicketAccountsRenewed[i] / TotalSeasonTicketAccounts[i+1]
  AccountRenewalRate[i] <- AccountRenewalRate[i] * 100
}
AccountRenewalRate <- round(AccountRenewalRate, digits = 2)

##Account Renewal Rate
plot(Season, AccountRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Season Ticket Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

##Recalculate Ticket Renewal Rate
for (i in 1:4) {
  TicketRenewalRate[i] <- SeasonTicketsRenewed[i] / TotalSeasonTickets[i+1]
  TicketRenewalRate[i] <- TicketRenewalRate[i] * 100
}
TicketRenewalRate <- round(TicketRenewalRate, digits = 2)

##Ticket Renewal Rate
plot(Season, TicketRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Season Ticket Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

##Difference in Account and Ticket Renewal Rate
DiffRenewalRate = AccountRenewalRate - TicketRenewalRate

##Difference between Account and Ticket Renewal Rate each Season
plot(Season, DiffRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Difference in Account and Ticket Renewal Rate",
     xlab = "Season", ylab = "Difference (%)")

############################################################################################
#######Part III: Crusader Combo Breakdown###################################################
############################################################################################

##Crusader Combo Accounts each Season
plot(Season, CrusaderComboAccounts, type = "l", col = "red", lwd = 3, 
     main = "Crusader Combo Accounts each Season", 
     xlab = "Season", ylab = "Accounts")

##Average Crusader Combo Accounts Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = CrusaderComboAccounts[i] / CrusaderComboAccounts[i+1]
}
1 - mean(PercentDrop)

##Crusader Combo Tickets Purchased each Season
plot(Season, CrusaderComboTickets, type = "l", col = "red", lwd = 3, 
     main = "Crusader Combo Tickets Purchased each Season", 
     xlab = "Season", ylab = "Tickets Purchased")

##Average Crusader Combo Tickets Percent Drop
for (i in 1:4) { 
  PercentDrop[i] = CrusaderComboTickets[i] / CrusaderComboTickets[i+1]
}
1 - mean(PercentDrop)
