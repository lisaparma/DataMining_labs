rm( list=ls() )
## REGOLARIZZAZIONE, LASSO E RIDGE

library(glmnet)

load("Leukemia.RData")

class(Leukemia) # "list"
# di solito si tratta di "data.frame", in questo caso si tratta di una lista
length(list)
str(list)

# si tratta di una variabile binaria, quindi di un problema di classificazione binario
table(Leukemia$y)

# andremo a stimare un modello di regressione logistica

fit <- glm(Leukemia$y ~ Leukemia$x, family="binomial")
fit$df.null # 71
# ha incluso tutte le variabili

summary(fit)
# il coefficiente di molte variabili non è riuscito a calcolarlo non avendo abbastanza dato a disposizione per farlo

# Dobbiamo quindi utilizzare un modello di regressione logistica penalizzato, ridge o lasso
class(Leukemia$x) # matrix -- OK!
X = Leukemia$x
y = Leukemia$y

# Iniziamo con ridge
#Cerco il lambda migliore attuando la k-fold cross validation
set.seed(123)
m_ridge_cv <- cv.glmnet(X, y, alpha=0, family="binomial") # di defaul nfolds=10 ovvero k=10

m_ridge_cv$lambda
m_ridge_cv$lambda.1se # 6.827698
m_ridge_cv$lambda.min # 4.49217

# adamento del MSE al variare del lambda (log(Lambda))
plot(m_ridge_cv)


# dato che il metodo ridge non opera una selezione delle variabile, scelgo di utilizzare il lambda.min
best_lambda <- m_ridge_cv$lambda.min

m_ridge_best <- glmnet(X, y, alpha=0, lambda=best_lambda, family="binomial")
summary(m_ridge_best)
m_ridge_best # covariate incluse nel modello, percentuale di devianza spiegata, lambda utilizzato
coef(m_ridge_best) # coefficienti inclusi nel modello

head(m_ridge_best$beta)

# Ripetiamo tutto con il lass (molto aggressivo, tende ad ottenere poche covariate)
m_lasso <- glmnet(X, y, alpha = 1, family="binomial")
plot(m_lasso, xvar='lambda')

m_lasso_cv <- cv.glmnet(X, y, alpha=1, family="binomial")
plot(m_lasso_cv)

best_lambda <- m_lasso_cv$lambda.min
m_lasso_best <- glmnet(X, y, alpha=1, lambda=best_lambda, family = "binomial")
head(m_lasso_best$beta)

# Meglio lasso: spiega meglio i dati (spiega meglio la devianza)

# Stimare cosa succede sa arrivano nuovi pazienti:
newx <- Leukemia$x[1:3, sample(1:ncol(Leukemia$x))]
head(newx[,1:3])

pred <-predict.glmnet(m_lasso_best, newx, type="response") # restituisce i coefficienti del modello, con response restituisce la risposta
pred
library(gtools)
gtools::inv.logit(pred)
