#:::::::::::::::::::::::::::;sum1:::::::::::::::::::::::
##dataset my_movies.csv
#importing dataset
movies<-read.csv(file.choose())
View(movies)
str(movies[1:5])
inspect(movies)
summary(movies)

# Finding association rules
library(arules)

#generating rules for all the itemsets of the list.
rules <- apriori(as.matrix(movies[6:15]),parameter = list(minlen=1, maxlen=10,supp=0.002, conf=.8))
inspect(rules)

#generating interesting rules 
rules <- apriori(as.matrix(movies[6:15]),parameter = list(minlen=1, maxlen=3,supp=0.1, conf=.8))
inspect(rules)

#sorting them according to support.
sort.by<-sort(rules, by = "support", decreasing = TRUE)
inspect(sort.by)
quality(sort.by)<-round(quality(sort.by),digits=3)

#pruning redudant rules
redundant<- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="confidence")#sorted by support
inspect(rules.pruned)
rules.pruned1 <- sort(rules.pruned, by="lift")#sorted by lift
inspect(rules.pruned1)

#Visualization of the collected data
library(arulesViz)
windows()
plot(rules.pruned1)
plot(rules.pruned1,method="grouped")
plot(rules.pruned1,method="graph")


#:::::::::::::::::::::::::::::::::sum2::::::::::::::::::::::::::::::::::
##dataset books.csv
#import dataset
book <- read.csv(file.choose())
str(book)
summary(as.matrix(book)) 

# Finding association rules
library(arules)
#generating rules for all the itemsets of the list.
rules <- apriori(as.matrix(book),parameter = list(minlen=2,maxlen=10,supp=0.07, conf=.8))
inspect(rules)

#generating interesting rules for all the itemsets.
rules <- apriori(as.matrix(book),parameter = list(minlen=2,maxlen=4,supp=0.07, conf=.8))
inspect(rules)
sort<-sort(rules, by = "support", decreasing = TRUE)
inspect(sort)

# Finding, eliminating redundant itemsets.
redundant01<-is.redundant(rules, measure="confidence")
which(redundant01)
prunedrul <- rules[!redundant01]
rules.pruned <- sort(prunedrul, by="lift")
inspect(rules.pruned)

#Visualization of the collected data
library(arulesViz)
windows()
plot(rules.pruned)
plot(rules.pruned,method="grouped")
plot(rules.pruned,method="graph")

#::::::::::::::::::::::::::sum3:::::::::::::::::::::::::::::

##Dataset (groceries.csv)

groceries<-read.csv(file.choose())
View(groceries)
inspect(groceries[1:10])
class(groceries)

#Finding association rules
library(arules)

#itemFrequencyPlot for top 20
itemFrequencyPlot(groceries,topN=20)

#generating arules 
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
#generating interesting rules 
rules<-apriori(groceries,parameter = list(support = 0.07,confidence = 0.8,minlen=1,maxlen=4))
inspect(rules)

#finding and eliminating redundant itemset
redund<-is.redundant(rules, measure="confidence")
which(redund)
prunedrul <- rules[!redund]
rules.pruned <- sort(prunedrul, by="lift")#sorting by lift
inspect(rules.pruned)

#Visualization of the collected data
library(arulesViz)
windows()
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
