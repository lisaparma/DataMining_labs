# Metodi automatici di selezione del modello
# se quantità elevata di covariate non possiamo metterci a confrontare tutti i modelli possibili

library(ISLR)
data("Hitters")
dim(Hitters)
head(Hitters)

# Modello per stimare lo stipendio di un giocatore
# vogliamo fare un modello di regressione lineare non sapendo quali sono le covariate che possono influenzare ciò
# Assunzioni del modello lineare: 
# - errori devono essere a media zero e non correlati
# - deve esserci omosteaticità 
# - dati approssivamente gaussiani
# - 

# Qualìè la distribuzione empirica? tramite boxplot
boxplot(Hitters$Salary)
# distribuzione abbastanza asimmetrica, questo è molto schiacciato verso il basso. 
# in queste situazioni è buono provare a considerare il valore logaritmo
boxplot(log(Hitters$Salary))
# se si parla di salari è facile che si utilizzi un logaritmo

# SOstituiamo il valore di salary con il suo logaritmo
Hitters$Salary <- log(Hitters$Salary) # Soluzione al primo problema

# Secondo problema: dati mancanti
Hitters <- na.omit(Hitters)
dim(Hitters)

library(leaps)
?regsubsets
m_forward <- regsubsets(Salary ~ ., data = Hitters, method = "forward", nvmax= 19)
summary(m_forward)
# trovati 3 modelli migliori
names(summary(m_forward))
summary(m_forward)$rss
summary(m_forward)$adjr2
summary(m_forward)$bic 

which.min(summary(m_forward)$bic ) # Coefficienti del modello 

coef(m_forward, 4)
plot(m_forward)
plot(m_forward, scale = "adjr2")
plot(summary(m_forward))
