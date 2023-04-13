##2022 Football Season Projections

###########################################################################################
####Data Set Up############################################################################
###########################################################################################

write.csv(STH, "C://Users//lazlo//Desktop//FB22STHList_Master.csv", row.names = F)

##Read in Data
STH <- read.csv("FB22STHList_Master.csv")
##Removes 2 Comp Observations From Data Set
STH <- STH[3:nrow(STH), ]
attach(STH)

View(STH)

###########################################################################################
###########################################################################################

##Check Grand Totals
Total <- (Tickets * PricePerTicket) + (ParkingPasses * PricePerPass)
isSame <- Total == GrandTotal
count = 0
for (i in 1:nrow(STH)) {
  if (isSame[i] == FALSE) {
    count <- count + 1
  }
}
count

##Create a Payment Hour from Payment Time
PaymentHour <- STH$PaymentTime
##Deletes Last 2 Characters from String
PaymentHour <- substr(PaymentHour, 1, nchar(PaymentHour)-2) 
##Appends "00 on to the End of the String
PaymentHour <- paste(PaymentHour, "00", collapse = NULL)
##Removes the Space in the Middle of the String
PaymentHour <- gsub(" ", '', PaymentHour)

##Set Payment Hour as an Ordered Factor
PaymentHour <- as.factor(PaymentHour)
PaymentHour <- ordered(PaymentHour, levels = c("0:00", "1:00", "2:00", "3:00",
                                                       "4:00", "5:00", "6:00", "7:00", 
                                                       "8:00", "9:00", "10:00", "11:00",
                                                       "12:00", "13:00", "14:00", "15:00",
                                                       "16:00", "17:00", "18:00", "19:00",
                                                       "20:00", "21:00", "22:00", "23:00"))

##Set Payment Day of Week as an Ordered Factor
STH$PaymentDoW <- as.factor(STH$PaymentDoW)
STH$PaymentDoW <- ordered(STH$PaymentDoW, levels = c("Sunday", "Monday", "Tuesday", 
                                                     "Wednesday", "Thursday",
                                                     "Friday", "Saturday"))

##Create Order Month Variable
OrderMonth <- PaymentCreateDate
OrderMonth <- substr(OrderMonth, 1, 1)
for (i in 1:nrow(STH)) {
  if (OrderMonth[i] == "4") {
    OrderMonth[i] <- "April"
  }
  else if (OrderMonth[i] == "5") {
    OrderMonth[i] <- "May"
  }
  else if (OrderMonth[i] == "6") {
    OrderMonth[i] <- "June"
  }
  else if (OrderMonth[i] == "7") {
    OrderMonth[i] <- "July"
  }
  else if (OrderMonth[i] == "8") {
    OrderMonth[i] <- "August"
  }
}

##Set Order Month as an Ordered Factor
OrderMonth <- as.factor(OrderMonth)
OrderMonth <- ordered(OrderMonth, levels = c("April", "May", "June",
                                             "July", "August"))

##Set Ticket Type as an Ordered Factor
STH$TicketType <- as.factor(STH$TicketType)
STH$TicketType <- ordered(TicketType, levels = c("Reserved Cushion", "Reserved Bleacher", 
                                                 "General Admission", "Fitton 4", "None"))

##Set Parking Location as an Ordered Factor
STH$ParkingLocation <- as.factor(STH$ParkingLocation)
STH$ParkingLocation <- ordered(ParkingLocation, levels = c("Baseball Lot", "Freshman Field", 
                                                           "Parking Garage", "West Lot", 
                                                           "Fitton 4", "None"))

##Set Delivery Method as an Ordered Factor
STH$DeliveryMethod <- as.factor(STH$DeliveryMethod)
STH$DeliveryMethod <- ordered(DeliveryMethod, levels = c("Mobile Delivery", 
                                                         "Print at Home", "Mail", 
                                                         "Subscription", "Email"))
###########################################################################################
###########################################################################################
###########################################################################################


###########################################################################################
####Part II: Contextualizing the 2022 Football Season######################################
###########################################################################################

##Read in Data For Graphs
STRR <- read.csv("2016_2021FBSeasonTicketRevenueReport.csv")
attach(STRR)

View(STRR)

PRR <- read.csv("2016_2021FBParkingRevenueReport.csv")
attach(PRR)

View(PRR)

###########################################################################################

##Recalculate Renewal Rates
AccountRenewalRate <- RenewalRate
for (i in 1:5) {
  AccountRenewalRate[i] <- RenewedSeasonTicketAccounts[i] / TotalSeasonTicketAccounts[i+1]
  AccountRenewalRate[i] <- AccountRenewalRate[i] * 100
}
AccountRenewalRate <- round(AccountRenewalRate, digits = 2)
AccountRenewalRate

TicketRenewalRate <- RenewalRate
for (i in 1:4) {
  TicketRenewalRate[i] <- RenewedSeasonTickets[i] / TotalSeasonTickets[i+1]
  TicketRenewalRate[i] <- TicketRenewalRate[i] * 100
}
TicketRenewalRate <- round(TicketRenewalRate, digits = 2)
TicketRenewalRate

###########################################################################################

##Yearly Plots

##Season Ticket Accounts per Season
plot(Year, TotalSeasonTicketAccounts, type = "l", lwd = 3, col = "red", 
     main = "Total Season Ticket Accounts each Season", 
     xlab = "Season", ylab = "Accounts")

##Season Tickets per Season
plot(Year, TotalSeasonTickets, type = "l", lwd = 3, col = "red", 
     main = "Total Season Tickets Sold each Season", 
     xlab = "Season", ylab = "Tickets Sold")

##Season Ticket Revenue per Season
plot(Year, TotalSeasonTicketRevenue, type = "l", lwd = 3, col = "red", 
     main = "Total Season Ticket Revenue each Season", 
     xlab = "Season", ylab = "Revenue ($)")

##Account Renewal Rate per Season
plot(Year, AccountRenewalRate, type = "l", lwd = 3, col = "red", 
     main = "Account Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

##Ticket Renewal Rate per Season
plot(Year, TicketRenewalRate, type = "l", lwd = 3, col = "red", 
     main = "Ticket Renewal Rate each Season", 
     xlab = "Season", ylab = "Renewal Rate (%)")

##Total Parking Accounts per Season
plot(Year, TotalPaidParkingAccounts, type = "l", lwd = 3, col = "red", 
     main = "Total Parking Accounts each Season", 
     xlab = "Season", ylab = "Accounts")

##Total Parking Passes per Season
plot(Year, TotalPaidParkingPasses, type = "l", lwd = 3, col = "red", 
     main = "Total Parking Passes each Season", 
     xlab = "Season", ylab = "Passes Sold")

##Total Parking Revenue per Season
plot(Year, TotalParkingRevenue, type = "l", lwd = 3, col = "red", 
     main = "Total Parking Revenue each Season", 
     xlab = "Season", ylab = "Revenue ($)")

##Total Revenue per Season
plot(Year, TotalRevenue, type = "l", lwd = 3, col = "red", 
     main = "Total Revenue each Season", 
     xlab = "Season", ylab = "Revenue ($)")

###########################################################################################
###########################################################################################
###########################################################################################



###########################################################################################
####Part III: Bar Plots####################################################################
###########################################################################################

##Barplot of Orders by Day of the Week
counts <- table(STH$PaymentDoW)
counts
barplot(counts, main = "Season Ticket Orders by Day of Week", 
        xlab = "Day of Week", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = rainbow(length(counts)))
box()
legend("topright", fill = rainbow(length(counts)), cex = .5, 
       legend = levels(factor(STH$PaymentDoW)))

##Create Data subset, not including spring football sales
STH_SF <- STH[PaymentCreateDate != "4/9/2022", ]

##Barplot of Orders by Day of the Week minus Spring Football
counts <- table(STH_SF$PaymentDoW)
counts
barplot(counts, main = "Season Ticket Orders by Day of Week", 
        xlab = "Day of Week", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = rainbow(length(counts)))
box()
legend("topright", fill = rainbow(length(counts)), cex = .5, 
       legend = levels(factor(STH$PaymentDoW)))

##Barplot of Orders by Time of Day
counts <- table(PaymentHour)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Time of Day", 
        xlab = "Time of Day", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = .40, 
       legend = levels(factor(PaymentHour)))

##Create Ordered Factor for Payment Hour for STH_SF
PaymentHour <- STH_SF$PaymentTime
##Deletes Last 2 Characters from String
PaymentHour <- substr(PaymentHour, 1, nchar(PaymentHour)-2) 
##Appends "00 on to the End of the String
PaymentHour <- paste(PaymentHour, "00", collapse = NULL)
##Removes the Space in the Middle of the String
PaymentHour <- gsub(" ", '', PaymentHour)

##Set Payment Hour as an Ordered Factor
PaymentHour <- as.factor(PaymentHour)
PaymentHour <- ordered(PaymentHour, levels = c("0:00", "1:00", "2:00", "3:00",
                                               "4:00", "5:00", "6:00", "7:00", 
                                               "8:00", "9:00", "10:00", "11:00",
                                               "12:00", "13:00", "14:00", "15:00",
                                               "16:00", "17:00", "18:00", "19:00",
                                               "20:00", "21:00", "22:00", "23:00"))

##Barplot of Orders by Time of Day (minus Spring Football)
counts <- table(PaymentHour)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Time of Day (minus Spring Football)", 
        xlab = "Time of Day", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = .40, 
       legend = levels(factor(PaymentHour)))



##Barplot of Orders by Month
counts <- table(OrderMonth)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Month", 
        xlab = "Month", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = 1, 
       legend = levels(factor(OrderMonth)))

##Orders by Ticket Type
counts <- table(STH$TicketType)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Ticket Type", 
        xlab = "Ticket Type", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = .75, 
       legend = levels(factor(STH$TicketType)))

##Orders by Parking Location
counts <- table(STH$ParkingLocation)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Parking Location", 
        xlab = "Parking Location", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = .75, 
       legend = levels(factor(STH$ParkingLocation)))

##Orders by Delivery Method
counts <- table(STH$DeliveryMethod)
counts
cols <- rainbow(length(counts))
barplot(counts, main = "Season Ticket Orders by Delivery Method", 
        xlab = "Delivery Method", ylab = "Frequency", 
        ylim = c(0, max(counts) * 1.2), 
        col = cols)
box()
legend("topright", fill = cols, cex = .75, 
       legend = levels(factor(STH$DeliveryMethod)))

