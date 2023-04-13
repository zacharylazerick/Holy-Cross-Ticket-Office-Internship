##2016-2021 Women's Basketball Revenue Report

#########################################################################################
####Data Setup###########################################################################
#########################################################################################

##Write to .csv to fix the i.. issue
write.csv(WBRR, "C://users//lazlo//Desktop//2016_2021WBBRR.csv", row.names = F)

##Read in the Data
WBRR <- read.csv("2016_2021WBBRR.csv")

##Attach for easier function calls
attach(WBRR)

View(WBRR)

##Calculate totals
TotalAccounts <- RenewedAccounts + NewAccounts
TotalTickets <- RenewedTickets + NewTickets
TotalRevenue <- RenewedRevenue + NewRevenue

#########################################################################################
#########################################################################################


#########################################################################################
####Part I: Summary Plots################################################################
#########################################################################################

##Season vs. TotalAccounts
plot(Season, TotalAccounts, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Accounts per Season", 
     xlab = "Season", ylab = "Accounts")


##Season vs. TotalTickets
plot(Season, TotalTickets, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Sold per Season", 
     xlab = "Season", ylab = "Tickets")


##Season vs. TotalRevenue
plot(Season, TotalRevenue, type = "l", col = "red", lwd = 3, 
     main = "Total Season Ticket Revenue per Season", 
     xlab = "Season", ylab = "Revenue ($)")


########################################################################################

##Set Parameters
cols <- c("red", "blue")
par(lwd = 1)

##Renewed vs. New Accounts
barplot(t(WBRR[c("RenewedAccounts", "NewAccounts")]), 
        beside = T, 
        ylim = c(0, max(WBRR[c("RenewedAccounts", "NewAccounts")]) * 1.2), 
        border = cols, col = cols, names.arg = WBRR$Season, 
        main = "Renewed vs. New Accounts", 
        xlab = "Season", ylab = "Accounts")
box()
legend("topright", cex = .75, fill = cols, 
       legend = c("Renewed Accounts", "New Accounts"))


##Renewed vs. New Tickets
barplot(t(WBRR[c("RenewedTickets", "NewTickets")]), 
        beside = T, 
        ylim = c(0, max(WBRR[c("RenewedTickets", "NewTickets")]) * 1.2), 
        border = cols, col = cols, names.arg = WBRR$Season, 
        main = "Renewed vs. New Tickets", 
        xlab = "Season", ylab = "Tickets")
box()
legend("topright", cex = .75, fill = cols, 
       legend = c("Renewed Tickets", "New Tickets"))


##Renewed vs. New Revenue
barplot(t(WBRR[c("RenewedRevenue", "NewRevenue")]), 
        beside = T, 
        ylim = c(0, max(WBRR[c("RenewedRevenue", "NewRevenue")]) * 1.2), 
        border = cols, col = cols, names.arg = WBRR$Season, 
        main = "Renewed vs. New Revenue", 
        xlab = "Season", ylab = "Revenue ($)")
box()
legend("topright", cex = .75, fill = cols, 
       legend = c("Renewed Revenue", "New Revenue"))

#######################################################################################
#######################################################################################


#######################################################################################
####Part II: Renewal Rate##############################################################
#######################################################################################

##Account Renewal Rate per Season
plot(Season, AccountRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Account Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

##Ticket Renewal Rate per Season
plot(Season, TicketRenewalRate, type = "l", col = "red", lwd = 3, 
     main = "Ticket Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

########################################################################################
########################################################################################


