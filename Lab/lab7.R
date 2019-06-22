rm( list=ls() )
## REGOLARIZZAZIONE, LASSO E RIDGE
library(ISLR)
data(Hitters)
dim(Hitters) # 322

# View(Hitters) # ci sono valori mancanti

## Analisi esplorativa
summary(Hitters)
# possiamo osservare quali sono le variabili qualitative e quali quelle quantitative e,
# alla fine dell'output, indica quanti sono i valori mancanti (Salary ne ha 59)

# eliminiamo dunque le osservazioni con dati mancanti
Hitters <- na.omit(Hitters)
dim(Hitters) # 263

# osserviamo com'è la distribuzione di Salary
boxplot(Hitters$Salary) # assimetrica, il basso alto è più pesante del basso basso (TODO: riascoltare questa frase)
# situazione di assimetria, proviamo a mettere il valore sotto logaritmo
hist(Hitters$Salary, breaks=30) # non appare a campana, distribuzione assimetrica

# poniamo Salary sotto forma di logaritmo
hist(log(Hitters$Salary), breaks=30) # breaks indica il numero di celle dell'istogramma
hist(log(Hitters$Salary))
# come si può osservare la distribuzione appare molto più simmetrica,

# Sovrascriviamo Salary con il suo logaritmo
Hitters$Salary <- log(Hitters$Salary)

## Prepariamoci le strutture dati per il nostro modello penalizzato
# vettore di risposta
y <- Hitters$Salary
# creiamoci la matrice delle covariate
X <- Hitters[,-19]
head(X)
# dobbiamo trasformare le variabili qualitative in fattori, model.matrix lo fa in automatico
X <- model.matrix(Salary ~ ., data = Hitters)
head(X)
# il problema è che model.matrix inserisce l'intecetta tra le covariate e per il nostro modello penalizzato glmnet non va bene,
# perchè non ha senso schiacciare verso lo zero anche l'intercetta, non vogliamo MAI eliminare l'intercetta dal modello

# togliamo dunque la prima colonna dalla matrice:
X <- model.matrix(Salary ~ ., data = Hitters)[,-1]
head(X)

# importiamo la libreria glmnet
library(glmnet)

## Iniziamo con il ridge
m_ridge <- glmnet(X, y, alpha=0) # alpha=0 è ridge, alpha=1 è lasso -- di default e' alpha=1
# vengono fittati 100 modelli diversi a diversi livelli di lambda (parametro di penalizzazione)
m_ridge
# per ognuno dei valori di lambda provati (sono 100)
# mostra la percentuale di devianza spiegata dal modello

# possiamo ottenere diverse informazioni dal modello
head(m_ridge$beta)
head(m_ridge$lambda)

names(m_ridge)

# con un plot possiamo però più facilmente visualizzare il tutto:
plot(m_ridge)
# ponendo invece che la norma l1, lambda sull'asse delle x otteniamo un grafico molto più significativo
plot(m_ridge, xvar='lambda')
# ponendo la percentuale di devianza spiegata:
plot(m_ridge, xvar='dev')
# chiaramente più i coefficienti sono distanti dallo zero e più la percentuale di devianza spiegata aumenta

# andiamo ad eseguire una k-fold cross validation
# rendendo il procedimento riproducibile, quindi settando un seed:
set.seed(123)
m_ridge_cv <- cv.glmnet(X, y, alpha=0) # di defaul nfolds=10 ovvero k=10, LOLCV nfolds=263=nrow(Hitters)
# con questa tecnica verrà selezionato il modello con il MSE più basso
# con il parametro type.measure possiamo selezionare un'altra misura per la selezione del modello migliore

m_ridge_cv$lambda
m_ridge_cv$lambda.min # corrisponde al lambda associato al minor MSE ottenuto
m_ridge_cv$lambda.1se # corrisponde al lambda più grande tra quelli che hanno uno standard error pari a 1 dal lambda.min
# lambda.1se serve per prendere modelli più semplici, anche se con ridge è inutile in quando non opera una selezione delle variabili

# con un grafico possiamo visualizzare meglio i valori di MSE ottenuti sulla base del lambda log(Lmabda)
# i numeri in alto sono il numero di variabili incluse in ciascun modello
plot(m_ridge_cv)

# andiamo a stimare il modello migliore (con il minor MSE)
best_lambda <- m_ridge_cv$lambda.min
m_ridge_best <- glmnet(X, y, alpha=0, lambda=best_lambda)
m_ridge_best # covariate incluse nel modello, percentuale di devianza spiegata, lambda utilizzato
coef(m_ridge_best) # coefficienti nel modello

plot(m_ridge_best) # TODO: grafico del nostro modello


# Passiamo alla regolarizzazione Lasso:
m_lasso <- glmnet(X, y, alpha = 1)
plot(m_lasso, xvar='lambda') # allo stesso modo possiamo osservare un grafico per il lasso
# come si può osservare nel lasso lo schiacciamento è più brusco rispetto al ridge
# i numeri riportati sopra al grafico indicano il numero di covariate incluse nel modello a diversi livelli di lambda

# anche rispetto la evianza spiegata possiamo osservare come le componenti vengano
# schiacciate molto più verso il basso rispetto che con il ridge
plot(m_lasso, xvar='dev')

# anche per il lasso andremo a selezionare il valore di lambda tramite la k-fold cross validation
set.seed(123)
m_lasso_cv <- cv.glmnet(X, y, alpha=1)
plot(m_lasso_cv) # possiamo anche in questo casso osservare il MSE ottenuto dai modelli associati a diversi valori di lambda
# come si può osservare non c'e' un solo modello per il quale siano state incluse tutte e 19 le covariate, neanche ai livelli piu' bassi di lambda

m_lasso_cv$lambda.min
m_lasso_cv$lambda.1se

# fittiamo il modello con il lambda migliore
best_lambda <- m_lasso_cv$lambda.min
m_lasso_best <- glmnet(X, y, alpha=1, lambda=best_lambda)
m_lasso_best

# andiamo ad utilizzare il lambda.1se invece che il lambda minimo, ottenendo un MSE leggermente piu' alto
# quindi che spiega leggermente meno la variabilita' dei dati,
# per ottenere pero' un modello piu' semplice e interpretabile
m_lasso_best2 <- glmnet(X, y, alpha=1, lambda=m_lasso_cv$lambda.1se)
m_lasso_best2
# come possiamo vedere include la meta' delle covariate rispetto al modello migliore ma spiega lo 0.1091% di variabilita' dei dati in meno

## Abbiamo anche gli elastic net che ci sollevano dal dover scegliere tra lasso e ridge,
## sono una specie di via di mezzo tra i due,
## incorporando i vantaggi di entrambi
m_net <- glmnet(X, y, alpha=0.5) # il parametro alpha indica quanto usiamo lasso e quanto usiamo ridge,
# anche il parametro alpha potrebbe essere stimato mediante la k-fold cross validation
plot(m_net, xvar='lambda')

set.seed(123)
m_net_cv <- cv.glmnet(X, y, alpha=0.5)
m_net_cv
plot(m_net_cv)