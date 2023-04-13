##MB22 STH Counting

#########################################################################################
####Data Setup###########################################################################
#########################################################################################

##Read in the Data
MBSTH <- read.csv("MB22_STH.csv")

attach(MBSTH)

View(MBSTH)

##########################################################################################
##########################################################################################
##########################################################################################


##########################################################################################
####Counting##############################################################################
##########################################################################################

##Initialize Values
RA = 0
RT = 0
RR = 0

SA = 0
ST = 0
SR = 0

CCA = 0
CCT = 0
CCR = 0

##Count Values
for (i in 1:nrow(MBSTH)) {
  if (OrderType[i] == "R" && BundleName[i] == "MB22") {
    RA <- RA + 1
    RT <- RT + Tickets[i]
    RR <- RR + (Tickets[i] * PricePerTicket[i])
  }
  else if (OrderType[i] == "S" && BundleName[i] == "MB22") {
    SA <- SA + 1
    ST <- ST + Tickets[i]
    SR <- SR + (Tickets[i] * PricePerTicket[i])
  }
  else if (BundleName[i] == "Crusader Combo") {
    CCA <- CCA + 1
    CCT <- CCT + Tickets[i]
    CCR <- CCR + (Tickets[i] * PricePerTicket[i])
  }
}

##Seating Ticket Type Breakdown
FSA = 0
FST = 0
FSR = 0

CBA = 0
CBT = 0
CBR = 0

RBA = 0
RBT = 0
RBR = 0

for (i in 1:nrow(MBSTH)) {
  if (TicketType[i] == "Floor") {
    FSA <- FSA + 1
    FST <- FST + Tickets[i]
    FSR <- FSR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "Chairback") {
    CBA <- CBA + 1
    CBT <- CBT + Tickets[i]
    CBR <- CBR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "Reserved Bleacher") {
    RBA <- RBA + 1
    RBT <- RBT + Tickets[i]
    RBR <- RBR + (Tickets[i] * PricePerTicket[i])
  }
}
