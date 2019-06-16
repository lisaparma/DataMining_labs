load("movie.Rdata")

summary(movie)

sum(is.na(movie)) # non ci sono dati mancanti

# i tipi delle variabili sono tutti ok
str(movie)

# Trasformo la variabile risposta quantitativa in qualitativa
newBox <- rep(0, nrow(movie))
newBox[movie$box>20000000] <- 1
# movie$boxF <- as.factor(newBox)
movie$box <- as.factor(newBox)

summary(movie)
levels(movie$mprating)


# ci interessano budget, action e cmngsoon
data <- movie[, c("boxF", "budget", "action", "cmngsoon")]

## Distribuzione variabile risposta
barplot(table(data$boxF)) # Ci sono più film che ....
table(data$boxF)

## Relazioni tra Y e variabili quantitative (budget e cmngsoon)
par(mfrow=c(1,2))
boxplot(data$budget~ data$boxF)
boxplot(data$cmngsoon~ data$boxF)
# sembra ci sia una relazione tra budget e boxF in quanto per film con un budget superiore a 700000 mila dollari ci sono solo film con alti guadagni se non per un outlier
# la relazione per l'altra variabile sembra essere più forte, in sembrano esserci più commenti se il film è stato più visto

## Relazione con qualitativa
par(mfrow=c(1,1))
mosaicplot(table(data$box, data$action))
# fun film d'azione ha più probabilità di avere un guadagno maggiore (perchè c'è lo scalino in alto)


# Cerchiamo relazioni significative tra la budget e action e cmngsoon e action
boxplot(data$budget~data$box*data$action)
# guardare se ci sono differenze tra 1-2 e 3-4 

boxplot(data$cmngsoon~data$box*data$action)


## Stimiamo un promo modello di regressione
m1 <-glm(boxF~action*budget + action*cmngsoon, data = data, family=binomial)
summary(m1)
1-pchisq(60.16, 56) # p -value
# Re maggiore dei gradi di libertà (fa schifo, qui appare alta ma facendo un test della devianza si osserva che il modello stimato è comunque una buona seplificazione rispetto al saturo) 


# Proviamo a semplificare il modello eliminando l'interazione tra action e budget che a
# ppare non significativa
m2 <-glm(boxF~ budget + action*cmngsoon, data = data, family=binomial)
summary(m2)
# Dal summary possiamo vedere come l'AIC sia passato da a , quindi è migliorato
# proviamo a confrontare i modelli con il test F
anova(m2, m1, test="Chisq") #test devianze
# da qui vediamo che c'è un p-value alto: possiamo dunque passare al modello più semplice m2

# dal summary il termina action appare non significativo ma appare significativa la sua relazione, quindi il principio di gerarchia non possiamo eliminarlo.

# Ora che il modello non è più semplificabile proviamo a cambiare approccio utilizzando un'analisi discriminativa.
# Partiamo da un'analisi discriminativa lineare includendo l'interazione tra action e cmngsoon

m.lda <- lda(boxF~ budget + action*cmngsoon, data = data)
m.lda
plot(m.lda)
# gli istogrammi si sovrappongono molto, ciò significa che la funzione discriminante non riesce a differenziare bene i due gruppi

# dato che il numero di osservazioni n è pari a 60 mentre il numero di variabili p è solo 4 possiamo provare ad eseguire un'analisi discriminante quadratica
m.qda <- qda(boxF~ budget + action*cmngsoon, data = data)
m.qda

# eseguiamo una hold out cross validation 
# vado a dividere il dataset originale in un training set e in un validation (test) set
# prendendo il 70% dei dati come traing set e il 30% come validation

set.seed(123)
n <- nrow(data)
sample <- sample(n, 0.70*n, replace=FALSE)
tr.set <- data[sample,]
val.set <- data[-sample,]

# ora stimo i modelli utilizzando solo il training set
m22 <-glm(boxF~ budget + action*cmngsoon, data = tr.set, family=binomial)
m.lda2 <- lda(boxF~ budget + action*cmngsoon, data = tr.set)
m.qda2 <- qda(boxF~ budget + action*cmngsoon, data = tr.set)

# calcolo le predizioni sul set di validazione
m22.prob <- predict(m2, newdata=val.set, type="response")
m22.preds <- rep(0, nrow(val.set))
m22.preds[m22.prob>0.5] <- 1


m.lda2.preds <- predict(m.lda2, newdata=val.set)
m.lda2.p <- rep(0, nrow(val.set))
m.lda2.p[m.lda2.preds$posterior[,2]>0.5] <- 1

m.qda2.preds <- predict(m.qda2, newdata=val.set)

# creo la miss classification table
addmargins(table(predictions=m22.preds, Box=val.set$boxF)) #7/19
addmargins(table(predictions=m.lda2.p, Box=val.set$boxF)) # 6/19
addmargins(table(predictions=m.qda2.preds$class, Box=val.set$boxF)) # 6/19


# dunque l'error rate sul validation set, per il modello di regressione logistica è: 0.368 (7/19)
# gli altri ...
# utilizzando dunque l'analisi discriminativa si ottiene un error rate leggermente inferiore, anche se si tratta 
# dato che su tutti e tre i modelli si ottengono comunque error rate abbastanza alti, proviamo a sceglierne uno tra lda e qda

# Confrontiamoli 
install.packages("pROC")
library(pROC)
lda.v.roc = roc(val.set$boxF, m.lda2.preds$posterior[,2])
qda.v.roc = roc(val.set$boxF, m.qda2.preds$posterior[,2])

# meglio AUC maggiore 

plot(lda.v.roc, print.auc=TRUE, auc.polygon=TRUE, legacy.axes=TRUE)

# l'area sotto la curva risulta uguale per entrambi i modelli, sono entrambe abbastanza basse . Visto dunque l'error rate di ciascun modello, visto l'auce la curva roc preferisco il modello lda in quanto più semplice e facilmente interpretabile rispetto ad qda
# In conclusione possiamo anche notare come la il guadagno prodotto da un film dipenda soprattutto dal budget investito su di esso, dal numero di commenti su cooming soon e 
# In particolare abbiamo visto che tende a guadagnare di più un film d'azione con tanti commenti su cooming soon e con un alto budget.

## Seconda parte 
m <- glm(box ~ .,  data=movie, family = "binomial")

library(glmnet)
y = movie$box
X = model.matrix(m)[, -1]
m.ridge <- glmnet(x=X, y=y, alpha=0, family = "binomial")

plot(m.ridge, xvar='lambda')

# Stimo il parametro di penalizzazione tramite k-fold cross validation con k = 10

set.seed(123)
cv_ridge <- cv.glmnet(X, y, alpha=0, family="binomial")

plot(cv_ridge)

best_lambda <- cv_ridge$lambda.min
m_ridge <- glmnet(X, y, alpha=0, lambda = best_lambda, family="binomial")
summary(m_ridge)

cbind(coef(m), coef(m_ridge))

## sono piu vicino allo zero proprio come ci aspettavamo
m.lasso <- glmnet(x=X, y=y, alpha=1, family = "binomial")
plot(m.lasso, xvar='lambda')
set.seed(123)
cv_lasso <- cv.glmnet(X, y, alpha=1, family="binomial")

plot(cv_lasso)
best_lambda <- cv_lasso$lambda.min
new_lambda <- cv_lasso$lambda.1se

m_lasso <- glmnet(X, y, alpha=1, lambda = best_lambda, family="binomial")
coef(m_lasso)

cbind(coef(m_ridge), coef(m_lasso))

m_lasso$dev.ratio
m_ridge$dev.ratio


previsioni.ridge <- predict(m_ridge, newx=X)
previsioni.lasso <- predict(m_lasso, newx=X)

# Utilizziamo la miss classification table per confrontare gli error rate

nY <- movie$box
n1 <- movie$budget
n2 <- movie$cmngsoon
n3 <- movie$fandango
f1 <- movie$action
f2 <- movie$mprating
f3 <- movie$comedy
data <- data.frame(nY, n1, n2, n3, f1, f2, f3)


save(data, file = "data.Rdata")
load("data.Rdata")
summary(data)
