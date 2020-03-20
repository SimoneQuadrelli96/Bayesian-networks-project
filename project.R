# Import libraries
library(Rgraphviz)
library(igraph)
library(gRbase)
library(gRim)
library(bnlearn)
library(DAAG)
library(lattice)
library(ggm) 
library(igraph) 

# Read the dataset
setwd("C:/Users/simon/Desktop/Graphical Model/probabilistic_modelling")
data <- read.csv(file = 'heart.csv')
data <- data[c(14,1,2,3,4,5,6,7,8,9,10,11,12,13)]
data$target <- factor(data$target)
data$sex <- factor(data$sex)
data$cp[data$cp>= 1] <- 1
data$cp[data$cp < 1]  <- 0
data$cp <- factor(data$cp)
data$fbs <- factor(data$fbs)
data$restecg <- factor(data$restecg)
data$exang <- factor(data$exang)
data$slope <- factor(data$slope)
data$ca <- factor(data$ca)
data$hyperchol <- data$chol
data$hyperchol[data$hyperchol<= 240 ]  <- 0
data$hyperchol[data$hyperchol> 240]  <- 1
data$hyperchol <- factor(data$hyperchol)
data$thal <- factor(data$thal)
data$class_age <- data$age
data$class_age[data$class_age<45] <- 1
data$class_age[ data$class_age>=45 & data$class_age<=65] <- 2
data$class_age[data$class_age>65] <- 3
data$age <- as.double(data$age)
data$trestbps <- as.double(data$trestbps)
data$chol <- as.double(data$chol)
data$thalach <- as.double(data$thalach)
data$oldpeak <- as.double(data$oldpeak)
head(data)


# Analysis of the distributions of discrete  variables
barplot(prop.table(table(data$target)),
        main="Disease",names.arg=c("No", "Yes") )
barplot(prop.table(table(data$sex)), 
        main="Sex distribution",  
        names.arg=c("Female", "Male"))
barplot(prop.table(table(data$cp)),
        main="Chest pain distribution",  
        names.arg=c("No", "Yes"))
barplot(prop.table(table(data$fbs)), 
        main="Fasting blood sugar > 120 mg/dl distribution",  
        names.arg=c("No", "Yes"))
barplot(prop.table(table(data$restecg)), 
        main="Resting electrocardiographic",  
        names.arg=c("normal", "ST-T ab.","hypertrophy"))
barplot(prop.table(table(data$exang)), 
        main="Exercise induced angina",  
        names.arg=c("No", "Yes"))
barplot(prop.table(table(data$slope)),
        main="Slope of the peak",  
        names.arg=c("upsloping", "flat","downsloping"))
barplot(prop.table(table(data$ca)), 
        main="Major vessels colored")
barplot(prop.table(table(data$thal)), 
        main="Thal distribution")
barplot(prop.table(table(data$hyperchol)), 
        main="Hypercholestrol distribution",  
        names.arg=c("No", "Yes"))
barplot(prop.table(table(data$class_age)),
        main="Age class distribution", 
        names.arg=c("< 45", ">= 45 & <= 65", "> 65") )


data_table <- as.table(ftable(data[c(1,3,4,7,8,10,12,13,14,15,16)]))


m_sat <- dmod(~.^.,data=data_table) 
m1 <- stepwise(m_sat)
m2 <- stepwise(m_sat,k=log(sum(data_table)))
m3  <- stepwise(dmod(~.^1,data=data_table) ,k=log(sum(data_table)),direction="forward",details=1)




igraph.to.graphNEL(as(m_sat,"igraph"))
igraph.to.graphNEL(as(m1,"igraph"))
igraph.to.graphNEL(as(m2,"igraph"))
igraph.to.graphNEL(as(m3,"igraph"))

summary(m3)
m2
summary(m2)

x11()
par(mfrow=c(2,2))
plot(as(m_sat,"igraph"),main="Initial model backward")
plot(as(m3,"igraph"),main="Initial model forward")
plot(as(m1,"igraph"), main="AIC")
plot(as(m2,"igraph"), main="BIC")


bn_data = data[c(1,2,3,4,5,15,7,8,9,10,11,12,13,14)]
classes <- c(2,1,1,3,4,1,1,1,1,3,4,4,4,1)
FrobiddenArcs <- matrix(0, nrow=length(bn_data), ncol=length(bn_data)) 
rownames(FrobiddenArcs)  <- names(bn_data) 
colnames(FrobiddenArcs) <- names(bn_data)
for (b in 2:4){
        FrobiddenArcs[classes==b, classes<b] <- 1 
}

ForbiddenList <- data.frame(get.edgelist(as(FrobiddenArcs, "igraph")))
names(ForbiddenList) <- c("from", "to") 
bn_model <- hc(bn_data, blacklist=ForbiddenList)
x11()
plot(as(amat(bn_model), "graphNEL"))
