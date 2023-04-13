##MIH22 STH Counting

#########################################################################################
####Data Setup###########################################################################
#########################################################################################

##Read in the Data
MIHSTH <- read.csv("MIH22_STH.csv")

attach(MIHSTH)

View(MIHSTH)

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


##Count Values
for (i in 1:nrow(MIHSTH)) {
  if (OrderType[i] == "R" && BundleName[i] == "MIH22") {
    RA <- RA + 1
    RT <- RT + Tickets[i]
    RR <- RR + (Tickets[i] * PricePerTicket[i])
  }
  else if (OrderType[i] == "S" && BundleName[i] == "MIH22") {
    SA <- SA + 1
    ST <- ST + Tickets[i]
    SR <- SR + (Tickets[i] * PricePerTicket[i])
  }
}

##Seating Metrics
GAA = 0
GAT = 0
GAR = 0

RCA = 0
RCT = 0
RCR = 0 

for (i in 1:nrow(MIHSTH)) {
  if (TicketType[i] == "General Admission") {
    GAA <- GAA + 1
    GAT <- GAT + Tickets[i]
    GAR <- GAR + (Tickets[i] * PricePerTicket[i])
  }
  else if (TicketType[i] == "Reserved Cushion") {
    RCA <- RCA + 1
    RCT <- RCT + Tickets[i]
    RCR <- RCR + (Tickets[i] * PricePerTicket[i])
  }
}

