# guardare le variabile soprattutto lse le qualitative sono effettivamente codificate così

dim(cars)

# guardare la variabile risposta 
# Istogramma variabile price 
# se c'entra con un prezzo è normale che sia ci sia una distribuzione asiemmetrica quindi possiamo considerare una trasformazione della variabile risposta, tipo una trasformazione logaritmica cosi diventa più simmetrica
# ovviamente questa trasformazione ha senso se ci sono solo valori positivi
hist(cars$Price)
hist(log(cars$Price))

boxplot(cars$Price)
boxplot(log(cars$Price))

cars$Price <- log(cars$Price) # Trasformata in modo definitivo


# guardare le altre variabili 
# guardare cosa vogliono dire
# in questo caso due continue e due qualitative

# scatterplot tra variabili continue
pairs(cars[,c("Price", "MPG.city", "Horsepower")])
# all'aumentare della cilintrata sembra esserci un aumento lineare del prezzo, ...

# variabili qualitative
# distribuzione di price condizionata dal valore di una variabile qualitativa
boxplot(cars$Price~cars$Origin)
# linea spessa è  mediana  è maggiore in una delle due e la distribuzione è maggiore
boxplot(cars$Price~cars$AirBags)

plot(cars$Price~cars$MPG.city, col = cars$Origin) # indicazione che c'è interazione tra MPG e origine perchè c'è divisione dei colori
plot(cars$Price~cars$MPG.city, col = cars$AirBags)

# Parte dei modelli 
# Partiamo con un modello con tutte le possibili covariate
m <- lm(Price~MPG.city* Origin + MPG.city* AirBags + Horsepower * Origin + Horsepower *AirBags, data= cars)
summary(m)
# le interazioni sono significative guardando il P-value?
# se le interazioni sono significative dobbiamo tenere anche le covariate singole

# modello meno complicato tenendo solo le interazioni che sembrano servire 
m2 <- lm(Price~ Horsepower*Origin + MPG.city*AirBags, data= cars)
summary(m2)

# confronto fra questi due modelli 
# guardiamo R quadro migliore (se più alto è meglio)

anova(m2, m)
# Test f mi restituisce un P vaalue che non è minore di .. . quindi prediligo il modello più piccolo perchè la differenza non è così significativa
#privilegiamo il modello più compatto 

# Analisi dei regidui
par(mfrow=c(2,2))
plot(m2)
# andamento residui buono, se non lineare possiamo intridurre dei polinomi o delle spline
# Q-Q plot non perfetto dato che i dati non sono perfettamente gaussiani (si vede dall'itogramma della variabile risposta)

# Inseriamo una spline di lisciamento della relazione con MPG.city
library(splines)
sp <- smooth.spline(x = cars$MPG.city, y= cars$Price, cv=TRUE)
sp
# scelta la spline migliore (df) 3.87 
install.packages("gam")
library(gam)
m3 <- gam(Price ~ s(MPG.city, 4) + AirBags + Origin + Horsepower, data = cars)
summary(m3)
# è servito

# Spline con interazioni
m4 <- gam(Price ~ s(MPG.city, 4)* AirBags + Origin *Horsepower, data = cars)
anova(m3, m4)

plot(m4, se=TRUE)


# 2 domanda
# regressioni penalizzate
library(glmnet)
m <- lm(Price ~ .,  data=cars)  # si può fare solo se n maggione di p
summary(m)

y <- cars$Price
# trasformare le variabili categoriali
X <- model.matrix(Price ~ .,  data=cars)
# non dobbiamo includere l'intercella
X <- model.matrix(Price ~ . -1,  data=cars)
X <- model.matrix(Price ~ .,  data=cars)[,-1]

m_ridge <- glmnet(X, y, alpha = 0) # ridge, lass = 1
plot(m_ridge)
plot(m_ridge, xvar="lambda")
# come variano i vari coefficienti 
ridge_cv <- cv.

# ... cose

# secondo il modello lasso alcuni di questi coefficienti non influiscono sul prezzo della macchina, per esempio le macchine non usa costano un log 0.1 in più delle macchine USA
