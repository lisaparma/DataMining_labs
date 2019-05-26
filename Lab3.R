dati<-read.csv("dataset/Gender_Discrimination.csv", sep=",") # Lettura dataset da file csv
dim(dati) # dimensione dataframe
head(dati) # vedo le prime 7 righe

dati[1:3,] # sottoselezionare pezzi di dataframe (righe, colonne)

summary(dati) # per ogni variabile un summario
is.factor(dati$Gender) # controllo se una variabile e` categoriale o meno
levels(dati$Gender) # livelli variabile categoriale in ordine
table(dati$Gender) # tabella di contingenza: conta quante variabili cadono in una certa categoria

boxplot(dati$Salary, las=2) #las=2 mette le etichette dritte
pie(table(dati$Gender), labels=c("F", "M"))
boxplot(dati$Salary~dati$Gender, col=c("pink", "blue"), las=2) # 
plot(dati$Experience, dati$Salary, las=2, main="Sal vs Exp",
        xlab="Exp", ylab="Sal", cex.axis=0.7) #cex.axox zoomma le scritte sugli assi
plot(dati$Experience, dati$Salary, las=2, main="Sal vs Exp",
     xlab="Exp", ylab="Sal", cex.axis=0.7, pch=19,
     col=as.numeric(dati$Gender)) #pch come fare i pallini (19 = pieni), passo a col un array di colori tanti quanti sono i dati$Gender
legend("topleft", pch=c(19,19), c("F", "M"), col=c(1,2), bty="n")


# Definiamo un modello lineare
model = lm(Salary~Gender+Experience, data=dati)
summary(model)
names(model)
# aggiungere rette di regressione al grafico
beta <- coef(model)
abline(beta[1], beta[3], col=1)
abline(beta[1]+beta[2], beta[3], col=2)

#Valutiamo se c'e interazione
model2 <- lm(Salary~Gender*Experience, data=dati)
summary(model2)
anova(model, model2)

plot(dati$Experience, dati$Salary, las=2, main="Sal vs Exp",
     xlab='Exp', ylab="Sal", cex.axis=0.7, pch=19,
     col=as.numeric(dati$Gender))
legend('topleft', pch=c(19,19), c('F', 'M'), col=c(1,2), bty="n")
beta2 <- coef(model2)
abline(beta2[1], beta2[3], col=1)
abline(beta2[1]+beta2[2], beta2[3]+beta2[4], col=2)

# Aggiungo variabile al quadrato
model3 <- lm(Salary~Gender+Experience+I(Experience^2)+Gender:Experience, data=dati)
summary(model3)
anova(model2, model3) # meglio il 2

# 
plot(model2)
# Rvs Fitted se i residui hanno andamenti particolari all'andamento di y lo vediamo
# Normal QQ: analizza i quantili, i cerchietti dovrebbero essere sopra la riga (vuol dire che i residui hanno un andamento normale), ma nelle cose e normale che si allontanino un po perche ci sono poche osserazioni
# scale location: come si distribuiscono i residui a seconda di y, se c'e un andamento strano ce

predict(model2, newdata=data.frame(list(Experience=20, Gender="Male")))
