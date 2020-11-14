                     #----installing some useful packages-----#

install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("factoextra")
install.packages("NbClust")
install.packages("permute")
install.packages("lattice")
install.packages("vegan")
library(vegan)
library(permute)
library(lattice)
library(NbClust)
library(factoextra)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


                      #----Setting working directory----#

setwd('C:/Users/bhati/Desktop')
getwd()

class <- read.csv('iris.csv')


                     #----Getting the view of the csv(data) file----#

View(class)


                  #----displaying the internal structure of dataset----#

str(class)

              #----Generic Function to produce result summaries of dataset----#

summary(class)



mydata <- select(class,c(1,2,3,4))



          #----scatter plot Basic Visualization before Clustering----#

scatter <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) 
res <- scatter + geom_point(aes(color=Species, shape=Species)) +
  theme_bw()+
  xlab("Petal Length") +  ylab("Petal Width") +
  ggtitle("Petal Length-Width")+
  theme(plot.title = element_text(hjust = 0.5))
res


  #----Within group Sum of Squares (WSS) plot to determine the optimal no of clusters----#

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", col = "red")


      #----NbClust funtion for determining the best number of clusters----#

par(mar = c(2,2,2,2))
irisData <- iris[,1:4]
totalwSS<-c()
nb <- NbClust(irisData, method = "kmeans")


#----histogram approach denoting various indices with different number of clusters voted----#

hist(nb$Best.nc[1,], breaks = 15, main="Number of Clusters",col = c("blue", "red", "gold", "green")) 



          #----Calinski-Harabasz index the Variance Ratio Criterion
          #It is the ratio of inter-cluster dispersion for all clusters
          #Higher the score , the better the performances----#


modelData <- cascadeKM(irisData, 1, 15, iter = 100)
plot(modelData, sortg = TRUE)
modelData$results[2,]

# To determine which of these values is maximum? BC/WC should be as large as possible

which.max(modelData$results[2,])


       #----------------------------THE END-------------------------------------#

