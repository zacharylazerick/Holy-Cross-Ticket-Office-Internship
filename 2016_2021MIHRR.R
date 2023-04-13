##2016-2021 Men's Ice Hockey Revenue Report

####################################################################################
####Data Initialization#############################################################
####################################################################################

##Write to .csv
write.csv(MIHRR, "C://users//lazlo//Desktop//2016_2021MIHRR//2016_2021MIHRR.csv", 
          row.names = FALSE)

##Read in Data
MIHRR <- read.csv("2016_2021MIHRR.csv")

attach(MIHRR)

View(MIHRR)

####################################################################################
####################################################################################
####################################################################################

####################################################################################
####Part I: Totals##################################################################
####################################################################################

##Total Accounts
plot(Season, TotalAccounts, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Accounts per Season", 
     xlab = "Season", ylab = "Accounts")

##Barplot Renewed vs. New Accounts
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(MIHRR[c("RenewedAccounts", "NewAccounts")]), 
        beside = T, 
        ylim = c(0, max(MIHRR[c("RenewedAccounts", "NewAccounts")]) * 1.2), 
        border = cols, col = cols, names.arg = MIHRR$Season, 
        main = "Renewed vs. New Accounts", 
        xlab = "Season", ylab = "Accounts")


##Total Tickets
plot(Season, TotalTickets, type = "l", col = "red", lwd = 3, 
     main = "Total Season Tickets Sold per Season", 
     xlab = "Season", ylab = "Tickets")

##Barplot Renewed vs. New Tickets
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(MIHRR[c("RenewedTickets", "NewTickets")]), 
        beside = T, 
        ylim = c(0, max(MIHRR[c("RenewedTickets", "NewTickets")]) * 1.2), 
        border = cols, col = cols, names.arg = MIHRR$Season, 
        main = "Renewed vs. New Tickets", 
        xlab = "Season", ylab = "Tickets")


##Total Revenue
plot(Season, TotalRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Revenue Earned per Season", 
     xlab = "Season", ylab = "Revenue ($)")

##Barplot Renewed vs. New Revenue
cols <- c("red", "blue")
par(lwd = 1)
barplot(t(MIHRR[c("RenewedRevenue", "NewRevenue")]), 
        beside = T, 
        ylim = c(0, max(MIHRR[c("RenewedRevenue", "NewRevenue")]) * 1.2), 
        border = cols, col = cols, names.arg = MIHRR$Season, 
        main = "Renewed vs. New Revenue", 
        xlab = "Season", ylab = "Revenue")

####################################################################################
####################################################################################
####################################################################################

####################################################################################
####Part II: Renewal Rate###########################################################
####################################################################################

##Create New Renewal Rate Variables
AccountRenewalRate <- RenewalRate
TicketRenewalRate <- RenewalRate

for (i in 1:4) {
  AccountRenewalRate[i] <- (RenewedAccounts[i] / TotalAccounts[i+1]) * 100
  TicketRenewalRate[i] <- (RenewedTickets[i] / TotalTickets[i+1]) * 100
}

AccountRenewalRate <- round(AccountRenewalRate, digits = 2)
TicketRenewalRate <- round(TicketRenewalRate, digits = 2)

##Account Renewal Rate Plot
plot(Season, AccountRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Account Renewal Rate per Season", 
     xlab = "Season", ylab = "Rate (%)")

##Ticket Renewal Rate Plot
plot(Season, TicketRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Ticket Renewal Rate per Season", 
     xlab = "Season", ylab = "Rate (%)")

##Create Diff. Renewal Rate Variable
DiffRenewalRate <- AccountRenewalRate - TicketRenewalRate

plot(Season, DiffRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Difference in Renewal Rates per Season", 
     xlab = "Season", ylab = "Rate (%)")

####################################################################################
####################################################################################
####################################################################################
