##Read in the Data
WTR <- read.csv("FB22WTRList_Master.csv")
attach(WTR)

View(WTR)

TicketType <- as.factor(TicketType)

##Create Price Variables
PricePerTicket <- rep(0, nrow(WTR))
PricePerPass <- rep(0, nrow(WTR))

##Set Prices
for (i in 1:nrow(WTR)) {
  if (TicketType[i] == "GA") {
    PricePerTicket[i] <- 65
  }
  else if (TicketType[i] == "Reserved Bleacher") {
    PricePerTicket[i] <- 90
  }
  else if (TicketType[i] == "Reserved Cushion") {
    PricePerTicket[i] <- 110
  }
  else {
   PricePerTicket[i] = 0
  }
}

for (i in 1:nrow(WTR)) {
  if (ParkingLocation[i] == "Baseball") {
    PricePerPass[i] <- 150
  }
  else if (ParkingLocation[i] == "West Lot") {
    PricePerPass[i] <- 35 
  }
  else if (TicketType[i] == "Freshman Field") {
    PricePerPass[i] <- 35
  }
  else if (ParkingLocation[i] == "Parking Garage") {
    PricePerPass[i] <- 45
  }
  else if (ParkingLocation[i] == "None") {
    PricePerPass[i] <- 0
  }
}

##Calculate Grand Totals
GrandTotal <- (Tickets * PricePerTicket) + (ParkingPasses * PricePerPass)
sum(GrandTotal)

##Sum Tickets
sum(Tickets)

##Calculate Non-Renewal Breakdowns
##Tickets
RCT <- 0
RCA <- 0 

RBT <- 0
RBA <- 0

GAT <- 0 
GAA <- 0

countT <- 0

for (i in 1:nrow(WTR)) {
  if (TicketType[i] == "GA") {
    GAT <- GAT + Tickets[i]
    GAA <- GAA + 1
  }
  else if (TicketType[i] == "Reserved Bleacher") {
    RBT <- RBT + Tickets[i]
    RBA <- RBA + 1
  }
  else if (TicketType[i] == "Reserved Cushion") {
    RCT <- RCT + Tickets[i]
    RCA <- RCA + 1
  } 
  else {
    countT <- countT + Tickets[i]
  }
}

##Parking
BLA <- 0
BLP <- 0

WLA <- 0
WLP <- 0

FFA <- 0
FFP <- 0

PGA <- 0
PGP <- 0

NOA <- 0

for (i in 1:nrow(WTR)) {
  if (ParkingLocation[i] == "Baseball") {
    BLA <- BLA + 1
    BLP <- BLP + ParkingPasses[i]
  }
  else if (ParkingLocation[i] == "West Lot") {
    WLA <- WLA + 1
    WLP <- WLP + ParkingPasses[i]
  }
  else if (ParkingLocation[i] == "Freshman Field") {
    FFA <- FFA + 1
    FFP <- FFP + ParkingPasses[i]
  }
  else if (ParkingLocation[i] == "Parking Garage") {
    PGA <- PGA + 1
    PGP <- PGP + ParkingPasses[i]
  }
  else if (ParkingLocation[i] == "None") {
    NOA <- NOA + 1
  }
}
