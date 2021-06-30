# load the relevant libraries
library(bnlearn)
library(gRain)
library(compiler)

# load the dataset
scidat = read.csv("myorigdata.csv")

EUROPE = sort(c("AT", "BE", "BG", "CY", "CZ", "DK", "DE", "EE", "EL",
                "ES", "FR", "GR", "HU", "HR", "IE", "IT", "LV", "LT", "LU",
                "MT", "NL", "PL", "PT", "SE", "SI", "SK", "NO", "UK"))

EUROPE = sort(c("BE", "FR", "NL", "DE", "AT", "IT", "PL")) # select small subset

mem = function(searchstr, vector)
{
    found = FALSE
    len = length(vector)
    i = 1
    while (!found && (i <= len)) {
        if ((!is.na(vector[i]) && searchstr == vector[i])) found = TRUE
        i = i + 1
    }
    return(found)
}

member = cmpfun(mem) # compile the function


occ = function(el, assoclist)
{
    l = assoclist
    found = FALSE
    i = 1
    len = length(l)
    while ((!found) && (len > 0) && (i <= len))
    {
        if (l[[i]] == el) found = TRUE
        i = i + 2
    }
    return(found)
}

occurs = cmpfun(occ)

selecteddata = matrix(nrow = 0, ncol = ncol(scidat))
dimnames(selecteddata) = list(NULL, c("UserLocation",
                                      "FriendsLocation", "scaledsci" ,
                                      "Distancekm",
                                      "Population2017userloc",
                                      "Popiulation2017frloc"))
assoclist = NULL # (LOC . POP)

sumnations = as.vector(matrix(0, nrow = 1, ncol = length(EUROPE)))
names(sumnations) = EUROPE
                         
for (i in 1:nrow(scidat)) {
    userloc = substring(scidat[i,1], first = 1, last = 2)
    frloc   = substring(scidat[i,2], first = 1, last = 2)
    if (member(userloc, EUROPE) && member(frloc, EUROPE)) {
        newrow = matrix(c(userloc, frloc, scidat[i, 3:6]),
                        nrow = 1, ncol = 6)
        if (!occurs(scidat[i,1], assoclist) && (!is.na(scidat[i, 5]))) {
            assoclist = c(assoclist, list(as.character(scidat[i, 1]),
                                          scidat[i, 5]))
            sumnations[[userloc]] = sumnations[[userloc]] +
                as.numeric(scidat[i, 5])
        }
        selecteddata = rbind(selecteddata, newrow)
        
    }
}

sumallnations = sum(sumnations)

newdata = data.frame(selecteddata)

for (j in 1:2) {
    newdata[,j] = factor(unlist(newdata[,j]))
}

for (j in 3:4) {
    newdata[,j] = as.numeric(unlist(newdata[,j]))
}

procdata = newdata
for (j in 3:3) {
    # procdata[c(j)] = discretize(log(procdata[c(j)]),
    # breaks = 5, method = "interval")
    m = mean(unlist(log(procdata[c(j)]))) # transform to standard normal
    s = sd(unlist(log(procdata[c(j)])))
    procdata[c(j)] = (log(procdata[c(j)]) - m) / s
    procdata[c(j)] = discretize(procdata[c(j)], breaks = 6, method = "interval")
    
}

for (j in 4:4) {
    # m = mean(unlist(procdata[c(j)])) # transform to standard normal
    # s = sd(unlist(procdata[c(j)]))
    # procdata[c(j)] = (procdata[c(j)] - m) / s
    procdata[c(j)] = discretize(procdata[c(j)], breaks = 6, method = "interval")
    
}

# model building
# factoring the data and converting to training and
# test set (75%- training set rest 25%- test set)

# trdat = floor(0.75 * nrow(scidat))
# trainind =  sample(seq_len(nrow(scidat)), size = trdat)
trainind = c(1, 2, 3, 4)
train = procdata[trainind]
test = train
# train = scidat[trainind, ]
# test  = scidat[-trainind, ]

# for (j in 1:length(train[1,])) {
#  train[,j] = factor(train[,j])
# }
# for (j in 1:length(test[1,])) {
#  test[,j] = factor(test[,j])
# }

net = tabu(train)

bn = bn.fit(net, train)

write.net("small.net", bn)

nationprob = sumnations / sumallnations
cpt = coef(bn[["UserLocation"]])
cpt[1:length(EUROPE)] = nationprob
bn[["UserLocation"]] = cpt
cpt = coef(bn[["FriendsLocation"]])
cpt[1:length(EUROPE)] = nationprob
bn[["FriendsLocation"]] = cpt
    
write.net("smallmod.net", bn)
bn.pred = predict(bn, node = "scaledsci", data = test, prob = TRUE) 
tabl1 = table(bn.pred, test[, "scaledsci"])
mn = mean(bn.pred == test$scaledsci)

mnet = as.grain(bn)
margprobsci = querygrain(mnet, nodes = c("scaledsci"), type = "marginal")
sumofvaluessci = summary(test$scaledsci)
totalsci = sum(sumofvaluessci)
actualmarginalsci = sumofvaluessci / totalsci

plot(margprobsci$scaledsci, actualmarginalsci, xlab = "Marginal probs scaled SCI BN",
     ylab = "Actual probs from data")

# as UserLocation and FriendLocation are indirected computer from the data
# calibration os expected to be poor
margprobul = querygrain(mnet, nodes = c("UserLocation"),  type = "marginal")
sumofvaluesul = summary(test$UserLocation)
totalul = sum(sumofvaluesul)
actualmarginalul = sumofvaluesul / totalul

plot(margprobul$UserLocation, actualmarginalul, xlab = "Marginal probs scaled UL BN",
     ylab = "Actual probs from data")

margprobdis = querygrain(mnet, nodes = c("Distancekm"),  type = "marginal")
sumofvaluesdis = summary(test$Distancekm)
totaldis = sum(sumofvaluesdis)
actualmarginaldis = sumofvaluesdis / totaldis

plot(margprobdis$Distancekm, actualmarginaldis, xlab = "Marginal probs scaled Distance BN",
     ylab = "Actual probs from data")

#naive Bayes model
# nb = naive.bayes(train, "scaledsci_qr")
# fitted.nb = bn.fit(nb, train)
# write.net("naivebn.net", fitted.nb)
#making predictions
# nb.pred = predict(fitted.nb, test, prob=TRUE) 
# tabl1 = table(nb.pred, test[, "scaledsci_qr"])
#checking the accuracy
# mean(nb.pred == test$scaledsci_qr)

#Tree augmented naive bayes
# tan = tree.bayes(train, "scaledsci_qr")
#fitting the model
# fitted.tan = bn.fit(tan, test)
# write.net("tanbn.net", fitted.tan)
#model prediction
# tan.pred = predict(fitted.tan, test, prob=TRUE)
# tabl2 = table(tan.pred, test[, "scaledsci_qr"])
#model accuracy
# mean(tan.pred == test$scaledsci_qr)

