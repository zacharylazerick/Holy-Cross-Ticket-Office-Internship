##FB22 STH Counting

###########################################################################################
####Data Set Up############################################################################
###########################################################################################

write.csv(STH, "C://Users//lazlo//Desktop//FB22_STH.csv", row.names = F)

##Read in Data
STH <- read.csv("FB22_STH.csv")

attach(STH)

View(STH)

###########################################################################################
####Part I: Calculations###################################################################
###########################################################################################

##Calculate Season Ticket Account Metrics
RA = 0
RT = 0
RR = 0

SA = 0
ST = 0
SR = 0

RA4 = 0
R4T = 0
R4R = 0

S4A = 0
S4T= 0
S4R= 0

for (i in 1:nrow(STH)) {
  if (OrderType[i] == "R" && BundleName[i] == "FB22") { 
    RA <- RA + 1
    RT <- RT + Tickets[i]
    RR <- RR + (Tickets[i] * PricePerTicket[i])
  }
  else if (OrderType[i] == "S" && BundleName[i] == "FB22") {
    SA <- SA + 1
    ST <- ST + Tickets[i]
    SR <- SR + (Tickets[i] * PricePerTicket[i])
  }
  else if (OrderType[i] == "R" && BundleName[i] == "Fitton 4 Pack") {
    RA4 <- RA4 + 1
    R4T <- R4T + Tickets[i]
    R4R <- R4R + (Tickets[i] * PricePerTicket[i])
  }
  else if (OrderType[i] == "S" && BundleName[i] == "Fitton 4 Pack") {
    S4A <- S4A + 1
    S4T <- S4T + Tickets[i]
    S4R <- S4R + (Tickets[i] * PricePerTicket[i])
  }
}

##Calculate Season Ticket Type Metrics
RCA = 0
RCT = 0
RCR = 0

RBA = 0
RBT = 0
RBR = 0

GAA = 0
GAT = 0
GAR = 0

F4A = 0
F4T = 0
F4R = 0

for (i in 1:nrow(STH)) {
  if (TicketType[i] == "Reserved Cushion") { 
    RCA <- RCA + 1
    RCT <- RCT + Tickets[i]
    RCR <- RCR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "Reserved Bleacher") {
    RBA <- RBA + 1
    RBT <- RBT + Tickets[i]
    RBR <- RBR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "General Admission") {
    GAA <- GAA + 1
    GAT <- GAT + Tickets[i]
    GAR <- GAR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "Fitton 4") {
    F4A <- F4A + 1
    F4T <- F4T + Tickets[i]
    F4R <- F4R + (Tickets[i] * PricePerTicket[i])
  }
}

##Calculate Parking Metrics
BP = 0
BPP = 0
BPR = 0

PG = 0
PGP = 0
PGR = 0

FF = 0
FFP = 0
FFR = 0

WL = 0
WLP = 0
WLR = 0

F4P = 0
F4PP = 0
F4PR = 0

NOP = 0

for (i in 1:nrow(STH)) {
  if (ParkingLocation[i] == "Baseball Lot") { 
    BP <- BP + 1
    BPP <- BPP + ParkingPasses[i]
    BPR <- BPR + (ParkingPasses[i] * PricePerPass[i])
  }
  else if (ParkingLocation[i] == "Parking Garage") {
    PG <- PG + 1
    PGP <- PGP + ParkingPasses[i]
    PGR <- PGR + (ParkingPasses[i] * PricePerPass[i])
  }
  else if (ParkingLocation[i] == "Freshman Field") {
    FF <- FF + 1
    FFP <- FFP + ParkingPasses[i]
    FFR <- FFR + (ParkingPasses[i] * PricePerPass[i])
  }
  else if (ParkingLocation[i] == "West Lot") {
    WL <- WL + 1
    WLP <- WLP + ParkingPasses[i]
    WLR <- WLR + (ParkingPasses[i] * PricePerPass[i])
  }
  else if (ParkingLocation[i] == "Fitton 4") {
    F4P <- F4P + 1
    F4PP <- F4PP + ParkingPasses[i]
    F4PR <- F4PR + (ParkingPasses[i] * PricePerPass[i])
  }
  else {
    NOP <- NOP + 1
  }
}
