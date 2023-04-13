##2021 vs 2022 Football Single-Game Ticket Sales
##Want to track week by week sales to understand if we are underperforming this year

#####################################################################################
####Document Setup###################################################################
#####################################################################################

##Write to .csv 
write.csv(FB2205, "c://users//lazlo//Desktop//2021_2022SingleGameTickets//FB2205_Tickets.csv", 
          row.names = F)

##Read in Data
FB2204 <- read.csv("FB2204_Tickets.csv")

attach(FB2204)

#####################################################################################
#####################################################################################


#####################################################################################
####Data Manipulation################################################################
####Part I: Setup .csv###############################################################
#####################################################################################

##Remove Students from Data Set
FB2204 <- subset(FB2204, Price.Type.Name != "Student Tickets")

##Separate Payment Create Date into Date and Time Variables
temp <- strsplit(FB2204$Order.Active.Date, split = " ")
##Unlist to get into Correct State
temp <- unlist(temp)

##Define New Variables for Date and Time
PaymentTime <- rep("", length(temp) / 2)
PaymentDate <- rep("", length(temp) / 2)

j = 1
k = 1

##Fill in Date and Time Variables
for (i in 1:length(temp)) {
  if (i %% 2 == 0) {
    PaymentTime[j] <- temp[i]
    j <- j + 1
  }
  else if (i %% 2 == 1) {
    PaymentDate[k] <- temp[i]
    k <- k + 1
  }
}

##Omit missing values if needed
PaymentDate <- na.omit(PaymentDate)
PaymentTime <- na.omit(PaymentTime)

##Delete Order Active Date from Data Frame
FB2204 <- FB2204[, -c(11)]
##Append Payment Date and Payment Time Variables to Data Frame
FB2204 <- data.frame(FB2204, PaymentDate, PaymentTime)

######################################################################################
######################################################################################




######################################################################################
####Data Setup########################################################################
######################################################################################

##Read in the Data
FB2205 <- read.csv("FB2205_Tickets.csv")

attach(FB2101)

View(FB2101)

######################################################################################
######################################################################################



######################################################################################
####Part II: Week Var#################################################################
######################################################################################

##Count the Number of Purchases by Week
counts <- table(FB2103$PaymentWeek)
counts 

##Number of Purchases by Week as a percentage
counts2 <- round(counts / nrow(FB2103) * 100, digits = 2)
counts2

29.44######################################################################################
######################################################################################





######################################################################################
####Part III: Comparison##############################################################
######################################################################################

##Write .csv Command
write.csv(FB, "c://users//lazlo//Desktop//2021_2022FootballEventSales.csv", row.names = F)

##Read in Data
FB <- read.csv("2021_2022FootballEventSales.csv") 

attach(FB)

View(FB)

######################################################################################
######################################################################################

##Subtraction Time

##Game 1
Week0Sales[6] - Week0Sales[1]
Week1Sales[6] - Week1Sales[1]
Week2Sales[6] - Week2Sales[1]
Week3Sales[6] - Week3Sales[1]

##Game 2
Week0Sales[7] - Week0Sales[2]
Week1Sales[7] - Week1Sales[2]
Week2Sales[7] - Week2Sales[2]
Week3Sales[7] - Week3Sales[2]

##Game 3
Week0Sales[8] - Week0Sales[3]
Week1Sales[8] - Week1Sales[3]
Week2Sales[8] - Week2Sales[3]
Week3Sales[8] - Week3Sales[3]

##Game 4
Week0Sales[9] - Week0Sales[4]
Week1Sales[9] - Week1Sales[4]
Week2Sales[9] - Week2Sales[4]
Week3Sales[9] - Week3Sales[4]

##Game 5
Week0Sales[10] - Week0Sales[5]
Week1Sales[10] - Week1Sales[5]
Week2Sales[10] - Week2Sales[5]
Week3Sales[10] - Week3Sales[5]

##Polar Park Game
Week0Sales[6] - Week0Sales[4]
Week1Sales[6] - Week1Sales[4]
Week2Sales[6] - Week2Sales[4]
Week3Sales[6] - Week3Sales[4]


######################################################################################
######################################################################################



######################################################################################
####Comps by Week#####################################################################
######################################################################################

FB2205 <- read.csv("FB2205_Tickets.csv")

counts <- table(FB2205$PaymentWeek, FB2205$Grand.Total)
counts[1:nrow(counts), 1]
