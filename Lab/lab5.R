# REGRESSIONE LOGISTICA
data("mtcars")
str(mtcars)
c.data <- mtcars[, c("mpg", "vs", "am")]    
c.data
dim(c.data)
is.factor(c.data$am)
class(c.data$am)
# gli diciamo che è un factor
c.data$am <- as.factor(c.data$am)
is.factor(c.data$am)
boxplot(c.data$mpg~c.data$vs) # relazione tra variabili
boxplot(c.data$mpg~c.data$vs*c.data$am) # costruisce i box per con la combinazione 

m.c <- glm(vs~mpg*am, data=c.data, family=binomial) # se non metto family lo setta a "gaussian" che rende il modello lineare semplice
summary(m.c) # Termini non significativi, proviamo a togliere l'interazione

m.c2 <- glm(vs~mpg+am, data=c.data, family=binomial)
summary(m.c2) # Interpretazione qualitativa: all'aumentare di mpg (coefficiente) aumenta la PROBABILITà che vs sia un motore in linea
# p-value di am1 alto ma comunque è basato su un'approssimazione 
# Facciamo un test sulla differenza delle devianze (analogo test f per regressione lineare)

# Residual deviance: confronto tra modello saturo e modello stimato
p <-1-pchisq(20.646, 29) # calcola il p-value
# concludiamo che non possiamo rifiutare l'ipotesi del modello più semplice, posso accettarlo

# Modello più semplice
m.c3 <- glm(vs~mpg, data=c.data, family=binomial)
summary(m.c3)

anova(m.c3, m.c2, test="Chisq") # senza test fa il test F
qchisq(0.95,1) # trova il quantile, gli do in pasto P e ritorna x, pchisq() gli do in pasto x e mi ritorna P

# Controlliamo il comando anova calcolando il p value
1-pchisq(4.887, 1)

# -> il modello m.c2 è quello che viene preferito
estimate<-coef(m.c2) #estraggo i coefficienti
estimate

se <- sqrt(diag(vcov(m.c2))) # estraggo gli standard error dei beta
se
# CI 90%
c(estimate[2]-qnorm(0.95)*se[2], estimate[2]+qnorm(0.95)*se[2]) # intervallo di confidenza basato sull'approssimazione normale
confint(m.c2, level=0.9)

ev <- predict(m.c2)
ev # valori sulla scala logit

# Mi interessa vedere che probabilità mi attribuisce che le macchine abbiano vs =1
ep <- predict(m.c2, type="response")
ep

exp(ev)/(1+exp(ev)) # uguale a ep
plot(c.data$mpg, c.data$vs, pch= 19, col=1+as.numeric(c.data$am))
curve(predict(m.c2, newdata=data.frame(mpg=x, am="0"), type="response"), add=T)
curve(predict(m.c2, newdata=data.frame(mpg=x, am="1"), type="response"), add=T, col=2)

preds <- rep(0, nrow(c.data))
preds[ep>0.5] <- 1
preds
addmargins(table(preds, vs=c.data$vs))
7/32 # traning error


n <- nrow(c.data)
set.seed(222)
sel <- sample(n, 0.6*n, replace=F) # queste osservazioni le usiamo come training set 
sel

tr.s <- c.data[sel,]
te.s <- c.data[-sel,]
m.ct <- glm(vs~mpg+am, data=tr.s, family=binomial)
summary(m.ct)

prob.test <- predict(m.ct, newdara=te.s, type="response")
preds.test <- rep(0, length(prob.test))
preds.test[probs.test>0.5] <- 1
addmargins(table(preds.test, vs=te.s$vs)) # Boh, errore

library(MASS)
m.cl <- lda(vs~mpg+am, data=tr.s)
m.cl

plot(m.cl)
preds.lda <- predict(m.cl, te.s)
p.lda<-rep(0, nrow(te.s))
preds.lda
p.lda[preds.lda$posterior[,2]>0.5]<- 1 #SBAGLIATO
p.lda


p.lda2<-rep(0, nrow(te.s))
p.lda2[preds.lda$posterior[,2]>0.2]<- 1 #SBAGLIATO
p.lda


library(pROC)
v.roc <- roc(te.s$vs, preds.lda$posterior[,2])
#.......
