cement <-read.table("hald.dat") # in automatico head=true, controllare che non legga l'head come prima riga
cement
colnames(cement)<- c("heat", "ca", "ts", "tf", "ds") # assegno i nomi alle colonne
head(cement)

#fare i grafici di dispersione rapidamente
pairs(cement) # con questi grafici controllo se ci sono delle relazioni tra le variabili (cerco pattern)

m1<- lm(heat~ca, data=cement)
summary(m1)
m2<-lm(heat~ca+ts, data=cement)
summary(m2) # meglio questo modello, R2 cresciuto molto

m3<-lm(heat~ca+ts+tf+ds, data=cement)
summary(m3) # buon R^2, ma quasi nessuna variabile è veramente significativa.. perchè?
# perchè alta relazione lineare tra alcune delle variabili
cor(cement) #matrice delle correlazioni tra le variabili
# bene che siano correlate con heat, ma non bene che ci sia un'alta correlazione tra le x
# tipo tf e ds hanno correlazione -0,97 (range tra -1 e 1) quindi molto correlate
# => problema della multicollinearità
# La soluzione è rimuovere una delle due variabili che sono correlate dato che non mi da nessuna info in più
# ergo il modello migliore è m2
# aggiungo l'interazione se comunque risultano significative da sole, se non lo sono meglio eliminarne una

rm(list=ls()) # rimuovo i dati salvati


library(ISLR)
data(Carseats)
str(Carseats)

m1<-lm(Sales~Price+US+ShelveLoc, data=Carseats)
summary(m1)
# ShelveLoc variabile qualitativa con tre livelli (Bad(livello base), medium, Good)

# Cambiare la categoria di riferimento
new.shelveloc <- Carseats$ShelveLoc
# Modo completo: ha bisogno dei livelli dell'oggetto e del livello base che vogliamo assegnargli
contrasts(new.shelveloc) <- contr.treatment(levels(new.shelveloc), base=which(levels(new.shelveloc) == "Good") )
m2<-lm(Sales~Price+US+new.shelveloc, data=Carseats)
summary(m2)

# Modo più semplice: cambia semplicemente i livelli
new.shelveloc2 <- relevel(Carseats$ShelveLoc, ref="Good")
m3<-lm(Sales~Price+US+new.shelveloc2, data=Carseats)
summary(m3)



# Ho un data set, faccio una regressione lineare. QUando vado a valutare la significatività dei parametri utilizzo t
# che ha questa distribuzione (B1st-B1)/SE(B1st)~tn-p-1, che non dipende da parametri ignoti

# studio di simulazione
set.seed(1234)
X1 <- rnorm(50) # simulo una variabile normale
X2 <- rnorm(50)

# Vogglio simulare tanti dataset da un modello di regressione lineare 
# Costruisco matrici e vettori
b <- se <- matrix(nrow = 1000, ncol = 3)
rse <- rep(NA, 1000) # creato vettore per i residui

for (i in 1:1000) { # creo mille dataset da una reg lineare con solo x1
  Y <- 3 + 1.5 * X1 + rnorm(50, sd=1.2) # modello vero che genera i miei dati (ci ho messo anche l'errore)
  dati <- data.frame(y=Y, x1=X1, x2=X2)
  fit <- lm(y~x1+x2, data=dati)
  b[i,] <- coef(fit)
  se[i,] <- sqrt(diag(vcov(fit)))
  rse[i] <- sd(fit$residuals)*sqrt(49/47)
}

hist((b[,2]-1.5)/se[,2], freq = F, n=25)
curve(dt(x,47), col=2, add=T)
linf <- b[,2]+qt(0.025, 47)*se[,2]
lsup <- b[,2]+qt(0.975, 47)*se[,2]
sum((linf<1.5) & (lsup>1.5))/1000 # livello di confidenza del data set, vedo in quanti dataset c'è il valore giusto
# b2=0 vs b2>0
sum((b[,3]/se[,3])>qt(0.95, 47))/1000


rm(list=ls()) # rimuovo i dati salvati

# REGRESSIONE LOGISTICA
data("mtcars")
str(mtcars)
cars.data <- mtcars[, c("mpg", "vs", "am")]    
cars.data
dim(cars.data)
is.factor(cars.data$am)
class(cars.data$am)
# gli diciamo che è un factor
cars.data$am <- as.factor(cars.data$am)
is.factor(cars.data$am)
boxplot(cars.data$mpg~cars.data$vs) # relazione tra variabili
boxplot(cars.data$mpg~cars.data$vs*cars.data$am) # costruisce i box per con la combinazione 
model <- glm(vs~mpg*am, data=cars.data, family=binomial)
summary(model)
