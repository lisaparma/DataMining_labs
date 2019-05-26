### Libreria
# Della libreria MASS utilizzo i dati "Boston
library(MASS)
data("Boston")

?Boston # Documentazione del dataset
str(Boston) # Elenca i nomi delle varie colonne, i tipi e degli esempi di dato

n <- nrow(Boston) # n. entries dataset


## Estrarre elementi [riga-colonna]
Boston # Tutto il dataset
Boston[1,] # Mostra la prima entry (con tutte le sue colonne)
Boston[1,3] # Mostra esattamente il valore della terza colonna della prima entry

# Valore della colonna "crim" per tutte le entries
Boston[,1]
Boston[,"crim"]
Boston$crim

select <- c("crim", "zn")  # c() compina i valori per creare un vettore
Boston[,select] # ritorna un dataset con solo i valori delle due colonne selezionate



### Plot
summary(Boston$medv) # Riassunto della colonna data, dipende dal tipo di oggetto passatogli

library(ggplot2)
myplot <- ggplot(data=Boston)
myplot

# ase() per settare x e y
myplot + geom_point(aes(x=medv, y=1)) # scatterplot: utile per vedere la relazione tra due variabili continue
myplot + geom_histogram(aes(x=medv))
myplot + geom_point(aes(x=lstat, y=medv))
myplot + geom_boxplot(aes(x=1, y=medv))

### Modello lineare semplice
n # n. righe
covXY <- cov(Boston$medv, Boston$lstat)
vX <- var(Boston$lstat)*(n-1)/n #varianza vera
beta1 <- covXY/vX
beta0 <- mean(Boston$medv) - beta1*mean(Boston$lstat)
beta0
beta1

myplot + geom_point(aes(x=lstat, y=medv)) + geom_smooth(aes(x=lstat, y=medv), method='lm')

mod1 <- lm(formula = medv ~ lstat, data=Boston)
summary(mod1)

names(mod1)
mod1$model
mod1$residuals
