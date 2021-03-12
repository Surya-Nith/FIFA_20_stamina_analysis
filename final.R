library("dplyr")
dffifa<- read.csv("FIFA_20.csv")
x<- dffifa$position
y<- dffifa$stamina
boxplot(y~x,main="Stamina Vs Position of Players in FIFA 20",xlab="Position",ylab="Stamina ( % )",pch=19,frame=T,col="orange",border="brown")
h<-hist(y,6,main="Stamina Frequency",xlab="Stamina",ylab="Frequency",col="azure")
x_perc<- seq(0,100,1)
mn<-mean(y)
std<-sd(y)
yn<- dnorm(x_perc,mean=mn,sd=std)
box.size<- diff(h$mids[1:2])*length(y)
yn<-yn*box.size
lines(x_perc,yn,col="blue")

#Creating Histograms for 3 Groups separately
hist1df <- filter(dffifa, position == "Defender")
hist(hist1df$stamina,6,main="Stamina Frequency For Defender",xlab="Stamina",ylab="Frequency",col="azure")
hist2df <- filter(dffifa, position == "Forward")
hist(hist2df$stamina,6,main="Stamina Frequency For Forward",xlab="Stamina",ylab="Frequency",col="azure")
hist3df <- filter(dffifa, position == "Midfielder")
hist(hist3df$stamina,6,main="Stamina Frequency For Midfielder",xlab="Stamina",ylab="Frequency",col="azure")

# Calculating the medians for 3 groups
library(dplyr)
group_by(dffifa, position) %>% 
  summarise(
    count = n(), 
    mean = mean(stamina, na.rm = TRUE),
    median=median(stamina,na.rm = TRUE),
    sd = sd(stamina, na.rm = TRUE),
    InterQRange=IQR(stamina, na.rm = TRUE)
  )

#Performing wilcox test among Defender & Forward groups with stamina in iterations
result <- filter(dffifa, position == "Defender" | position =="Forward" )
u<-result$position
v<-result$stamina
res <- wilcox.test(round(as.numeric(v))~u) # Pvalue is 4.819e-11
res

#Performing wilcox test among Defender & midfielder groups with stamina  in iterationss
result <- filter(dffifa, position == "Defender" | position =="Midfielder" )
u<-result$position
v<-result$stamina
res <- wilcox.test(round(as.numeric(v))~u) # Pvalue is < 2.166e-10
res

#Performing wilcox test among Midfielder & Forward with stamina groups in iterations
result <- filter(dffifa, position == "Midfielder" | position =="Forward" )
u<-result$position
v<-result$stamina
res <- wilcox.test(round(as.numeric(v))~u) # Pvalue is 2.166e-10
res

#performing the anova test to compare the result
atest<- aov(y~x,data=dffifa)
summary(atest)


