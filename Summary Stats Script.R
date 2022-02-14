#Richa Bhattacharya
#Dr. Cabana 

#Set working directory
code.directory = "UCSF"
setwd = code.directory

#Load Libaries 
library(ggplot2)
library(dplyr)

#NAValues <- data.frame(NA.count = colSums(is.na(cabana)))

#cabana$Duration1_sec <- cabana$`Duration (m)..5` * 60 + cabana$`Duration (s)..6`
#cabana$Duration2_sec <- cabana$`Duration (m)..9` * 60 + cabana$`Duration (s)..10`
#cabana$Duration3_sec <- cabana$`Duration (m)..13` * 60 + cabana$`Duration (s)..14`
#cabana$Duration4_sec <- cabana$`Duration (m)..17` * 60 + cabana$`Duration (s)..18`

#Standardization 
#NOTE - YOU ALREADY STANDARDIZED ON EXCEL DO YOU STILL NEED THIS? RESTART 
#cabana$Duration1_sec <- cabana$Duration1_sec / 2
#cabana$Duration2_sec <- cabana$Duration2_sec / 2
#cabana$Duration3_sec <- cabana$Duration3_sec / 2

#getting rid of the extra lines at the bottom
#cabana <- cabana[-c(21,22,23,24,25,26,27,28,29,30), ]

#cabana$`WeeksOld1` <- cabana$`WeeksOld1` / 7 + 1
#cabana$`WeeksOld2` <- cabana$`WeeksOld2` / 7 + 1
#cabana$`WeeksOld3` <- cabana$`WeeksOld3` / 7 + 1
#cabana$`WeeksOld4` <- cabana$`WeeksOld4` / 7 + 1

#How to change a Column Name
#colnames(cabana)[colnames(cabana)=="WeeksOld #1"] <- "WeeksOld1"

#Truncating Integer Values Appropriately
#cabana$WeeksOld1 <- trunc(cabana$`WeeksOld1`)
#cabana$WeeksOld2 <- trunc(cabana$`WeeksOld2`)
#cabana$WeeksOld3 <- trunc(cabana$`WeeksOld3`)
#cabana$WeeksOld4 <- trunc(cabana$`WeeksOld4`)

#total average - take each id find cumulative time 
#To Do: run the t tests on all of the data, confidence 95, double sided


#Tables for Birth by week
week1 <- weeksnoblanks[ , c("ID.1","Start date #1", "1.week","Duration1_secs", "FirstTime1", "MomAge1","Gender1","Birth1", "Adults1")]
week1 <- na.omit(week1)
week1.average <- sum(week1$Duration1_secs) / nrow(week1)
week2 <- weeksnoblanks[ , c("ID.2","Start date #2", "2.week","Duration2_secs","FirstTime2", "MomAge2","Gender2","Birth2", "Adults2")]
week2 <- na.omit(week2)
week2.average <- sum(week2$Duration2_secs) / nrow(week2)
week3 <- weeksnoblanks[ , c("ID.3","Start date #3", "3.week","Duration3_secs","FirstTime3", "MomAge3","Gender3","Birth3", "Adults3")]
week3 <- na.omit(week3)
week3.average <- sum(week3$Duration3_secs) / nrow(week3)
week4 <- weeksnoblanks[ , c("ID.4","Start date #4", "4.week","Duration4_secs","FirstTime4", "MomAge4","Gender4","Birth4", "Adults4")]
week4 <- na.omit(week4)
week4.average <- sum(week4$Duration4_secs) / nrow(week4)
week5 <- weeksnoblanks[ , c("ID.5","Start date #5", "5.week","Duration5_secs","FirstTime5", "MomAge5","Gender5","Birth5", "Adults5")]
week5 <- na.omit(week5)
week5.average <- sum(week5$Duration5_secs) / nrow(week5)
week6 <- weeksnoblanks[ , c("ID.6","Start date #6", "6.week","Duration6_secs", "FirstTime6", "MomAge6","Gender6","Birth6", "Adults6")]
week6<- na.omit(week6)
week6.average <- sum(week6$Duration6_secs) / nrow(week6)
week7 <- weeksnoblanks[ , c("ID.7","Start date #7", "7.week","Duration7_secs","FirstTime7", "MomAge7","Gender7","Birth7", "Adults7")]
week7 <- na.omit(week7)
week7.average <- sum(week7$Duration7_secs) / nrow(week7)

#By Gender
week1.girl <- sum((filter(week1,Gender1 =="1"))$Duration1_secs / nrow(week1))
week1.boy <- sum((filter(week1,Gender1 =="0"))$Duration1_secs / nrow(week1))
week2.girl <- sum((filter(week2,Gender2 =="1"))$Duration2_secs / nrow(week2))
week2.boy <- sum((filter(week2,Gender2 =="0"))$Duration2_secs / nrow(week2))
week3.girl <- sum((filter(week3,Gender3 =="1"))$Duration3_secs / nrow(week3))
week3.boy<- sum((filter(week3,Gender3 =="0"))$Duration3_secs / nrow(week3))
week4.girl <- sum((filter(week4,Gender4 =="1"))$Duration4_secs / nrow(week4))
week4.boy <- sum((filter(week4,Gender4 =="0"))$Duration4_secs / nrow(week4))
week5.girl <- sum((filter(week5,Gender5 =="1"))$Duration5_secs / nrow(week5))
week5.boy <- sum((filter(week5,Gender5 =="0"))$Duration5_secs / nrow(week5))
week6.girl <- sum((filter(week6,Gender6 =="1"))$Duration6_secs / nrow(week6))
week6.boy <- sum((filter(week6,Gender6 =="0"))$Duration6_secs / nrow(week6))
week7.girl <- sum((filter(week7,Gender7 =="1"))$Duration7_secs / nrow(week7))
week7.boy <- sum((filter(week7,Gender7 =="0"))$Duration7_secs / nrow(week7))

#Number of Adults in House
week1.adult.less <- sum((filter(week1,Adults1 <= 2))$Duration1_secs / nrow(week1))
week1.adult.more <- sum((filter(week1,Adults1 > 2))$Duration1_secs / nrow(week1))
week2.adult.less <- sum((filter(week2,Adults2 <= 2))$Duration2_secs / nrow(week2))
week2.adult.more <- sum((filter(week2,Adults2 > 2))$Duration2_secs / nrow(week2))
week3.adult.less <- sum((filter(week3,Adults3 <= 2))$Duration3_secs / nrow(week3))
week3.adult.more <- sum((filter(week3,Adults3 > 2))$Duration3_secs / nrow(week3))
week4.adult.less <- sum((filter(week4,Adults4 <= 2))$Duration4_secs / nrow(week4))
week4.adult.more <- sum((filter(week4,Adults4 > 2))$Duration4_secs / nrow(week4))
week5.adult.less <- sum((filter(week5,Adults5 <= 2))$Duration5_secs / nrow(week5))
week5.adult.more <- sum((filter(week5,Adults5 > 2))$Duration5_secs / nrow(week5))
week6.adult.less <- sum((filter(week6,Adults6 <= 2))$Duration6_secs / nrow(week6))
week6.adult.more <- sum((filter(week6,Adults6 > 2))$Duration6_secs / nrow(week6))
week7.adult.less <- sum((filter(week7,Adults7 <= 2))$Duration7_secs / nrow(week7))
week7.adult.more <- sum((filter(week7,Adults7 > 2))$Duration7_secs / nrow(week7))

#Type of Birth 
week1.birth.v <- sum((filter(week1,Birth1 == "0"))$Duration1_secs / nrow(week1))
week1.birth.c <- sum((filter(week1,Birth1 == "1"))$Duration1_secs / nrow(week1))
week2.birth.v <- sum((filter(week2,Birth2 == "0"))$Duration2_secs / nrow(week2))
week2.birth.c <- sum((filter(week2,Birth2 == "1"))$Duration2_secs / nrow(week2))
week3.birth.v <- sum((filter(week3,Birth3 == "0"))$Duration3_secs / nrow(week3))
week3.birth.c <- sum((filter(week3,Birth3 == "1"))$Duration3_secs / nrow(week3))
week4.birth.v <- sum((filter(week4,Birth4 == "0"))$Duration4_secs / nrow(week4))
week4.birth.c <- sum((filter(week4,Birth4 == "1"))$Duration4_secs / nrow(week4))
week5.birth.v <- sum((filter(week5,Birth5 == "0"))$Duration5_secs / nrow(week5))
week5.birth.c <- sum((filter(week5,Birth5 == "1"))$Duration5_secs / nrow(week5))
week6.birth.v <- sum((filter(week6,Birth6 == "0"))$Duration6_secs / nrow(week6))
week6.birth.c <- sum((filter(week6,Birth6 == "1"))$Duration6_secs / nrow(week6))
week7.birth.v <- sum((filter(week7,Birth7 == "0"))$Duration7_secs / nrow(week7))
week7.birth.c <- sum((filter(week7,Birth7 == "1"))$Duration7_secs / nrow(week7))

#First Time / Experienced Mothers
week1.first <- sum((filter(week1,Birth1 == "0"))$Duration1_secs / nrow(week1))
week1.experienced <- sum((filter(week1,Birth1 == "1"))$Duration1_secs / nrow(week1))
week2.first <- sum((filter(week2,Birth2 == "0"))$Duration2_secs / nrow(week2))
week2.experienced <- sum((filter(week2,Birth2 == "1"))$Duration2_secs / nrow(week2))
week3.first <- sum((filter(week3,Birth3 == "0"))$Duration3_secs / nrow(week3))
week3.experienced <- sum((filter(week3,Birth3 == "1"))$Duration3_secs / nrow(week3))
week4.first <- sum((filter(week4,Birth4 == "0"))$Duration4_secs / nrow(week4))
week4.experienced <- sum((filter(week4,Birth4 == "1"))$Duration4_secs / nrow(week4))
week5.first <- sum((filter(week5,Birth5 == "0"))$Duration5_secs / nrow(week5))
week5.experienced <- sum((filter(week5,Birth5 == "1"))$Duration5_secs / nrow(week5))
week6.first <- sum((filter(week6,Birth6 == "0"))$Duration6_secs / nrow(week6))
week6.experienced <- sum((filter(week6,Birth6 == "1"))$Duration6_secs / nrow(week6))
week7.first <- sum((filter(week7,Birth7 == "0"))$Duration7_secs / nrow(week7))
week7.experienced <- sum((filter(week7,Birth7 == "1"))$Duration7_secs / nrow(week7))

#MomAge
week1.mom.under35 <- sum((filter(week1,MomAge1 <= 35))$Duration1_secs / nrow(week1))
week1.mom.above35 <- sum((filter(week1,MomAge1 > 35))$Duration1_secs / nrow(week1))
week2.mom.under35 <- sum((filter(week2,MomAge2 <= 35))$Duration2_secs / nrow(week2))
week2.mom.above35 <- sum((filter(week2,MomAge2 > 35))$Duration2_secs / nrow(week2))
week3.mom.under35 <- sum((filter(week3,MomAge3 <= 35))$Duration3_secs / nrow(week3))
week3.mom.above35 <- sum((filter(week3,MomAge3 > 35))$Duration3_secs / nrow(week3))
week4.mom.under35 <- sum((filter(week4,MomAge4 <= 35))$Duration4_secs / nrow(week4))
week4.mom.above35 <- sum((filter(week4,MomAge4 > 35))$Duration4_secs / nrow(week4))
week5.mom.under35 <- sum((filter(week5,MomAge5 <= 35))$Duration5_secs / nrow(week5))
week5.mom.above35 <- sum((filter(week5,MomAge5 > 35))$Duration5_secs / nrow(week5))
week6.mom.under35 <- sum((filter(week6,MomAge6 <= 35))$Duration6_secs / nrow(week6))
week6.mom.above35 <- sum((filter(week6,MomAge6 > 35))$Duration6_secs / nrow(week6))
week7.mom.under35 <- sum((filter(week7,MomAge7 <= 35))$Duration7_secs / nrow(week7))
week7.mom.above35 <- sum((filter(week7,MomAge7 > 35))$Duration7_secs / nrow(week7))


#Plot Average Gender
plot(1:7,c(week1.girl,week2.girl, week3.girl, week4.girl, week5.girl, week6.girl, week7.girl) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time for Girls By Week of Birth")
plot(1:7,c(week1.boy,week2.boy, week3.boy, week4.boy, week5.boy, week6.boy, week7.boy) ,type = "l", xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time for Boys By Week of Birth")
#Plot by Number of Adults in House
plot(1:7,c(week1.adult.less,week2.adult.less, week3.adult.less, week4.adult.less, week5.adult.less, week6.adult.less, week7.adult.less) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth For Households with 2 or less Adults")
plot(1:7,c(week1.adult.more,week2.adult.more, week3.adult.more, week4.adult.more, week5.adult.more, week6.adult.more, week7.adult.more) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth For Households with more than 2 Adults")
#Plot Type of Birth
plot(1:7,c(week1.birth.c,week2.birth.c, week3.birth.c, week4.birth.c, week5.birth.c, week6.birth.c, week7.birth.c) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth For C-section delivery")
plot(1:7,c(week1.birth.v,week2.birth.v, week3.birth.v, week4.birth.v, week5.birth.v, week6.birth.v, week7.birth.v) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth For Vaginal delivery ")
#Plot First Time / Experienced 
plot(1:7,c(week1.first,week2.first, week3.first, week4.first, week5.first, week6.first, week7.first) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth with First Time Parents")
plot(1:7,c(week1.experienced,week2.experienced, week3.experienced, week4.experienced, week5.experienced, week6.experienced, week7.experienced) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth with Experienced Parents")
#Plot Mother's Age
plot(1:7,c(week1.mom.under35,week2.mom.under35, week3.mom.under35, week4.mom.under35, week5.mom.under35, week6.mom.under35, week7.mom.under35) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth with Mother under 35yrs")
plot(1:7,c(week1.mom.above35,week2.mom.above35, week3.mom.above35, week4.mom.above35, week5.mom.above35, week6.mom.above35, week7.mom.above35) ,type = "l",xlab = "Week", ylab = "Average Crying Time (s)", main = "Avg. Crying Time By Week of Birth with Mother over 35yrs")








