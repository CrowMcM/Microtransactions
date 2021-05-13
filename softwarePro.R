library()

install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))
install.packages("Amelia")
install.packages(c("reshape"))
install.packages("devtools")
install.packages("ggpubr")

library(readr)
library(Amelia)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(mice)
library(reshape)
library(ggpubr)
library(C50) # for the C5.0 model
library(rpart) # for the recursive partitioning model
library(rpart.plot) # for plotting recursive partitioning models
library(RColorBrewer) # for colour palettes
library(caret) # for the confusion matrix and model metrics
library(ROCR) # for the ROC curve
library(rattle) # for fancy RPart plot
library(adabag)

setwd("C:/Users/eoinm/Documents/school/College/software project/data")

SteamGames <-read.csv("steam.csv",header=T, na.strings=c(""),stringsAsFactors = T)
vgSales <-read.csv("vgsales.csv",header=T, na.strings=c(""),stringsAsFactors = T)
clashData <-read.csv("clash-of-clans.csv",header=T, na.strings=c(""),stringsAsFactors = T)
fifaData <-read.csv("CompleteDataset.csv",header=T, na.strings=c(""),stringsAsFactors = T)
fullFifa <-read.csv("FullData.csv",header=T, na.strings=c(""),stringsAsFactors = T)

library(htmltab)
install.packages(c("htmltab"))
urlMeta <- "https://www.metacritic.com/browse/games/score/metascore/all/psvita/filtered/"
topGames <- htmltab(doc=url)

urlRuined <- "https://www.cinemablend.com/games/11-Games-Ruined-By-Microtransactions-84117.html/"
microGames <- htmltab(doc=url)

url <- "https://www.svg.com/153347/the-worst-microtransactions-of-2019-so-far/"
worst2019 <- htmltab(doc=url)

boxplot(vgSales)
boxplot(SteamGames)
boxplot(clashData)
boxplot(fifaData)
boxplot(fullFifa)

table(SteamGames$name)
table(SteamGames$positive_ratings)
table(SteamGames$negative_ratings)
table(SteamGames$price)

table(vgSales$Name)
table(vgSales$Global_Sales)
table(vgSales$Rank)
table(vgSales$Platform)

table(clashData$Name)
table(clashData$Rating)
table(clashData$Content)

table(fifaData$Name)
table(fifaData$Overall)
table(fifaData$Value)
table(fifaData$Wage)

table(fullFifa$Name)
table(fullFifa$Club)
table(fullFifa$Rating)

sum(is.na(clashData$Rating))
sum(is.na(fifaData$Value))
sum(is.na(fullFifa$Rating))
sum(is.na(SteamGames$positive_ratings))
sum(is.na(vgSales$Global_Sales))

attributes(SteamGames)
attributes(vgSales)
attributes(clashData)
attributes(fifaData)
attributes(fullFifa)

clashData['Name']
clashData['Rating']
clashData['Content']

fifaData['Name']
fifaData['Overall']
fifaData['Value']

fullFifa['Name']
fullFifa['Club']
fullFifa['Rating']

SteamGames['name']
SteamGames['price']
SteamGames['positive_ratings']

vgSales['Name']
vgSales['Global_Sales']
vgSales['Platform']

barplot(table(fifaData$Value),xlab ="Name", ylab="value")
barplot(table(vgSales$Global_Sales),xlab ="Name", ylab="Global_Sales")
barplot(table(SteamGames$positive_ratings),xlab ="Name", ylab="positive_ratings")
barplot(table(fullFifa$Rating),xlab ="Name", ylab="rating")
barplot(table(clashData$Rating),xlab ="Name", ylab="Rating")

max(SteamGames$negative_ratings)
max(SteamGames$positive_ratings)
max(SteamGames$price)

min(SteamGames$negative_ratings)
min(SteamGames$positive_ratings)
min(SteamGames$price)

max(vgSales$Global_Sales)
max(vgSales$Rank)
max(vgSales$NA_Sales)
min(vgSales$Global_Sales)
min(vgSales$Rank)
min(vgSales$NA_Sales)

max(fullFifa$Rating)
min(fullFifa$Rating)

max(fifaData$Age)
max(fifaData$Value)
max(fifaData$Wage)
min(fifaData$Age)
min(fifaData$Value)
min(fifaData$Wage)

max(clashData$Rating)
min(clashData$Rating)

hist(SteamGames$negative_ratings,
     breaks=20,
     xlab="negative reviews",
     main="hist of negative reviews",
     ylim = c(0,80))

hist(SteamGames$positive_ratings,
     breaks=20,
     xlab="positive reviews",
     main="hist of positive reviews",
     ylim = c(0,80))

hist(SteamGames$price,
     breaks=20,
     xlab="price",
     main="hist of prices",
     ylim = c(0,80))

hist(vgSales$Global_Sales,
     breaks=20,
     xlab="global sales",
     main="hist of global sales",
     ylim = c(0,80))

hist(clashData$Rating,
     breaks=20,
     xlab="rating",
     main="hist of ratings",
     ylim = c(0,80))

plot(SteamGames$price, SteamGames$positive_ratings, type = "l",lty="dashed") 

plot(vgSales$Rank, vgSales$Global_Sales, type = "l",lty="dashed")

plot(clashData$Date, clashData$Rating, type = "l",lty="dashed")

plot(fifaData$Value, fifaData$Wage, type = "l")



#vgsale data stuff

vgSales[vgSales == "N/A"]=NA
vgSales=na.omit(VGData)
attach(vgSales)
vgSales$Name=as.factor(as.character(vgSales$Name))
vgSales$Platform=as.factor(as.character(vgSales$Platform))
vgSales$Year=as.numeric(as.character(vgSales$Year))
vgSales$Genre=as.factor(as.character(vgSales$Genre))
vgSales$Publisher=as.factor(as.character(vgSales$Publisher))
max(vgSales$Year,na.rm=T)
#Histogram of frequency of the game by year
hist(vgSales$Year,col = "blue",xlab = "Year",ylab = "Frequency of the game", main = "Histogram of frequency of the game by year")


library(ggplot2)
#Histogram of Global sales of the game by genre
revenue_by_Genre=aggregate(Global_Sales~Genre,vgSales,sum)
arrange_by_Genre=arrange(revenue_by_Genre,desc(Global_Sales))
arrange_by_Genre$Genre = factor(arrange_by_Genre$Genre, levels = arrange_by_Genre$Genre)
ggplot(arrange_by_Genre,aes(Genre,Global_Sales)) + 
        geom_bar(fill="blue",stat = "identity") +
        ggtitle("Video Game - Global Sales by Genre")

#Histogram of top 10 Publisher by revenue
revenue_of_Publisher=aggregate(Global_Sales~Publisher,vgSales,sum)
arrange_Revenue_of_Publisher_by_Global_Sales=arrange(revenue_of_Publisher,desc(Global_Sales))
top_10=arrange_Revenue_of_Publisher_by_Global_Sales[1:10,]
#plot(factor(top_10$Publisher),top_10$Global_Sales,type='h',lwd = 8,xlab="Publisher",ylab="Global Sales",col = "red",main = "Top 10 Publishers by Revenue")
ggplot(top_10,aes(Publisher,Global_Sales, fill=Publisher))+
        geom_bar(stat = "identity")+
        ggtitle("Top 10 Publisher by Revenue") +
        theme(legend.position = "top")

#Correlation of the sales Factor
num_Sales=vgSales[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
cor(num_Sales)

# Prediction : Support Vector Machines
train =( Year <= 2012)
vgSales.train=vgSales[train,]
vgSales.test = vgSales[!train,]
# Linear classification
y.train=ifelse(vgSales.train$Global_Sales>10.0,1,-1)
dat=data.frame(x=vgSales.train$NA_Sales+vgSales.train$EU_Sales, y=as.factor(y.train))
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
#summary(svmfit)
table(Model=svmfit$fitted , Truth=dat$y)
cat("Model Error = ", mean(svmfit$fitted!=dat$y)*100,"%")
y.test=ifelse(vgSales.test$Global_Sales>10.0,1,-1)
dat.te=data.frame(x=vgSales.test$NA_Sales+vgSales.test$EU_Sales, y=as.factor(y.test))
pred.te=predict(svmfit, newdata=dat.te)
table(Predict=pred.te, Truth=dat.te$y)
cat("Prediction Error = ", mean(pred.te!=dat.te$y)*100,"%")

#Prediction through Decision Trees
num_fact=vgSales[,c("NA_Sales","EU_Sales","Global_Sales")]

High=ifelse(Global_Sales <=10.0,"No","Yes")
dat =data.frame(num_fact,High)

tree.dat=tree(High~.-Global_Sales ,dat,subset=train)
summary(tree.dat)
plot(tree.dat)
text(tree.dat ,pretty =0)

dat.test = vgSales[!train,]
High.test=High[!train]
tree.pred=predict(tree.dat,dat.test,type="class")
table(Predict=tree.pred ,Truth=High.test)
cat("Prediction Error = ", mean(tree.pred!=High.test)*100,"%")

#corelation

#scatter
ggscatter(SteamGames, x="negative_ratings", y="publisher",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab="game rating", ylab = "Game publisher")

ggscatter(SteamGames, x="positive_ratings", y="publisher",
          add="reg.line",conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab="game rating", ylab = "Game publisher")

# Shapiro-Wilk normality test for neg rating
shapiro.test(SteamGames$negative_ratings) # => p = 




SteamGames[SteamGames == "N/A"]=NA
SteamGames=na.omit(steamData)
attach(SteamGames)
SteamGames$name=as.factor(as.character(SteamGames$name))
SteamGames$publisher=as.factor(as.character(SteamGames$publisher))
SteamGames$negative_ratings=as.numeric(as.character(SteamGames$negative_ratings))
SteamGames$positive_ratings=as.factor(as.character(SteamGames$positive_ratings))
SteamGames$genres=as.factor(as.character(SteamGames$genres))
max(SteamGames$negative_ratings,na.rm=T)
#Histogram of frequency of the game by year
hist(SteamGames$negative_ratings,col = "blue",xlab = "ratings",ylab = "Frequency of the game", main = "Histogram of frequency of the game by rating")

#Histogram of negative ratings by publisher
ratings_by_publisher=aggregate(negative_ratings~publisher,SteamGames,sum)
arrange_by_publisher=arrange(ratings_by_publisher,desc(publisher))
arrange_by_publisher$publisher = factor(arrange_by_publisher$publisher, levels = arrange_by_publisher$publisher)
ggplot(arrange_by_publisher,aes(publisher,publisher)) + 
        geom_bar(fill="blue",stat = "identity") +
        ggtitle("Video Game - negative ratings by publisher")


#Histogram of top 10 Publisher by negative ratings
rating_by_publisher=aggregate(negative_ratings~publisher,SteamGames,sum)
arrange_rating_of_Publisher_by_negative_rating=arrange(rating_by_publisher,desc(publisher))
top_10=arrange_rating_of_Publisher_by_negative_rating[1:10,]

ggplot(top_10,aes(publisher,negative_ratings, fill=publisher))+
        geom_bar(stat = "identity")+
        ggtitle("Top 10 Publisher by negative ratings") +
        theme(legend.position = "top")



#Histogram of positive ratings by publisher
ratings_by_publisher=aggregate(positive_ratings~publisher,SteamGames,sum)
arrange_by_publisher=arrange(ratings_by_publisher,desc(publisher))
arrange_by_publisher$publisher = factor(arrange_by_publisher$publisher, levels = arrange_by_publisher$publisher)
ggplot(arrange_by_publisher,aes(publisher,publisher)) + 
        geom_bar(fill="blue",stat = "identity") +
        ggtitle("Video Game - positive ratings by publisher")

#here needs work
#Histogram of top 10 Publisher by positive ratings
rating_by_publisher=aggregate(positive_ratings~publisher,SteamGames,sum)
arrange_rating_of_Publisher_by_positive_rating=arrange(rating_by_publisher,desc(publisher))
top_10=arrange_rating_of_Publisher_by_positive_rating[1:10,]

ggplot(top_10,aes(publisher,positive_ratings, fill=publisher))+
        geom_bar(stat = "identity")+
        ggtitle("Top 10 Publisher by positive ratings") +
        theme(legend.position = "top")


theme_set(theme_bw())

ggplot(SteamGames,aes(x=publisher,y=negative_ratings))+
        geom_bar(stat = "identity",width=.5,
                 fill="tomato3")+
        labs(title="ordered bar chart",
             subtitle = "publisher by negative rating",
             caption="source: steamgames")+
        theme(axis.text.x = element_text(
                angle=65, vjust=0.6))

ggplot(SteamGames,aes(x=positive_ratings,y=publisher))+
        geom_bar(stat = "identity",width=.5,
                 fill="tomato3")+
        labs(title="ordered bar chart",
             subtitle = "publisher by positive rating",
             caption="source: steamgames")+
        theme(axis.text.x = element_text(
                angle=65, vjust=0.6))


attach(SteamGames)
plot(positive_ratings, publisher, main="Scatterplot ",
     xlab="positive rating ", ylab="publisher ", pch=19)

attach(SteamGames)
plot(negative_ratings, genres, main="Scatterplot ",
     xlab="negative rating ", ylab="genre ", pch=19)

#playtime by rating
wilcox.test(SteamGames$average_playtime, SteamGames$positive_ratings, alternative="two.sided")
kruskal.test(SteamGames$average_playtime, SteamGames$positive_ratings, data = merged_Data)
ks.test(SteamGames$average_playtime,SteamGames$positive_ratings)
cor(SteamGames$average_playtime, SteamGames$positive_ratings)

ggplot(SteamGames) +
        aes(x = publisher, y = positive_ratings, color = publisher) +
        geom_jitter() +
        theme(legend.position = "none")

ggplot(SteamGames) +
        aes(x = publisher, y = negative_ratings, color = publisher) +
        geom_jitter() +
        theme(legend.position = "none")

#corelation tests price
cor(SteamGames$price, SteamGames$average_playtime)
cor(SteamGames$price, SteamGames$negative_ratings)

steam.train.index <- createDataPartition(
        SteamGames$negative_ratings,
        p = .75,
        list = FALSE
)

steam.train <- SteamGames[steam.train.index,]
# create the test dataset using all but the indexes from the partitioning above
steam.test <- SteamGames[-steam.train.index,]

ggplot(steam.train, aes(positive_ratings)) + geom_bar()

fit.rpart <- rpart( # train a recursive partitioning model
        positive_ratings ~ ., # with type as the response variable, all others as explanatory
        data = steam.train # and fgl.train as the training dataset
)

prp( # plot the fitted model
        fit.rpart, # the fitted model
        faclen = 0, # the length of the factor level names. 0 = full
        cex = 0.6, # text size
        extra = 1 # extra information printed in the nodes
        # 1 = the number of observations per class that in the node
)

rpart.predict <- predict( # predict using an recursive partitioning model
        fit.rpart, # our rpart model
        newdata = steam.test[,-10], # the test dataset
        type="class" # the type of prediction (classification)
)

rpartConfusionMatrix <- confusionMatrix(
        rpart.predict, # the predicted classes
        steam.test[,10] # the actual classes
)
rpartConfusionMatrix
