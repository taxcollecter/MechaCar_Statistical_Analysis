#3
library(dplyr)

#4
mecha_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#5
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,mecha_table) #create linear model

#6
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,mecha_table)) #create linear model





#Deliverable 2

#2
coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#3
total_summary <- coil_table %>% summarize(Mean=mean(PSI), Median=median(PSI), Var=var(PSI), SD=sd(PSI)) #create summary table

#4
lot_summary <- coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Var=var(PSI), SD=sd(PSI)) #create summary table


#Deliverable 3

#1
SummaryTTest <- t.test(coil_table$PSI,mu=mean(coil_table$PSI)) #compare sample versus population means

#2
coil_lot1 <- coil_table %>% filter(Manufacturing_Lot =="Lot1")
coil_lot2 <- coil_table %>% filter(Manufacturing_Lot =="Lot2")
coil_lot3 <- coil_table %>% filter(Manufacturing_Lot =="Lot3")

Lot1TTest <- t.test(coil_lot1$PSI ,mu=mean(coil_table$PSI)) #compare sample versus population means
Lot2TTest <- t.test(coil_lot2$PSI ,mu=mean(coil_table$PSI)) #compare sample versus population means
Lot3TTest <- t.test(coil_lot3$PSI ,mu=mean(coil_table$PSI)) #compare sample versus population means


