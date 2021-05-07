library(readxl)
SuppTable <- read_excel("SuppTable07_05.xlsx")
length(which(SuppTable$Sex == "F")) #57
length(which(SuppTable$Sex == "M")) #36
length(which(SuppTable$score == 3.0)) #45
#Info1 K and M are the populations in the south, NG are the populations in the north. 
Population <- SuppTable$`Info 1`
Population[104] <- "NG"
K <- which(Population == "K")
Population[K] <- "South"
M <- which(Population == "M")
Population[M] <- "South"
NG <- which(Population == "NG")
Population[NG] <- "North"
SuppTable$Population <- Population
North <- SuppTable[which(SuppTable$Population == "North"),]
South <- SuppTable[which(SuppTable$Population == "South"),]
Samples <- c(length(which(SuppTable$Population == "North")), length(which(SuppTable$Population == "South")), sum(length(which(SuppTable$Population == "North")) + length(which(SuppTable$Population == "South"))))
Female <- c(length(which(North$Sex == "F")), length(which(South$Sex == "F")), sum(length(which(North$Sex == "F"))+length(which(South$Sex == "F"))))
Male <- c(length(which(North$Sex == "M")), length(which(South$Sex == "M")), sum(length(which(North$Sex == "M"))+length(which(South$Sex == "M"))))
Mature <- c(length(which(North$score == "3")), length(which(South$score == "3")), sum(length(which(North$Sex == "3"))+length(which(South$score == "3"))))
Summary <- data.frame(Samples, Male, Female, Mature, row.names = c("North", "South", "Total"))
chisq.test(Summary) #p-value 0.8059
SexSumm <- data.frame(Male[1:2], Female[1:2], row.names = c("North", "South"))
chisq.test(SexSumm) #expected above 5 so don't use fishers exact #p=1

Population2 <- SuppTable$`Info 1`
Population2[104] <- "NG"
K <- which(Population2 == "K")
Population2[K] <- "South1"
M <- which(Population2 == "M")
Population2[M] <- "South2"
NG <- which(Population2 == "NG")
Population2[NG] <- "North"
SuppTable$Population2 <- Population2
North <- SuppTable[which(SuppTable$Population2 == "North"),]
South1 <- SuppTable[which(SuppTable$Population2 == "South1"),]
South2 <- SuppTable[which(SuppTable$Population2 == "South2"),]

Samples2 <- c(length(which(SuppTable$Population2 == "North")), length(which(SuppTable$Population2 == "South1")),  length(which(SuppTable$Population2 == "South2")))
Female2 <- c(length(which(North$Sex == "F")), length(which(South1$Sex == "F")), length(which(South2$Sex == "F")))
Male2 <- c(length(which(North$Sex == "M")), length(which(South1$Sex == "M")), length(which(South2$Sex == "M")))

Mature2 <- c(length(which(North$score == "3")), length(which(South1$score == "3")), length(which(South2$score == "3")))

Summary2 <- data.frame(Female2, Male2, row.names = c("North", "South1", "South2"))
chisq.test(Summary2) #p=0.2134
Summary2wMat <- data.frame(Male2, Female2, Mature2, row.names = c("North", "South1", "South2"))

