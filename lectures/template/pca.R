
data(Carseats,package = "ISLR")
library(ggplot2)
library(dplyr)

t100 = Carseats %>%
    filter(Advertising > 0) %>%
    mutate(Population = Population / 10) %>%
    top_n(100,Population)
ggplot(t100, aes(x = Population, y = Advertising)) + geom_point()


# too difficult
data(Hitters,package = "ISLR")
summary(lm(Salary~., Hitters))
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

library(pls)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE,validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE,
            validation ="CV")
validationplot(pcr.fit,val.type="MSEP")

# pca lab 10.4
states = row.names(USArrests)
apply(USArrests , 2, mean)
apply(USArrests , 2, var)
skimr::skim(USArrests)

# must standardize
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
# loading vectors 
pr.out$rotation
# PC score matrix
dim(pr.out$x)
biplot(pr.out, scale=0)






