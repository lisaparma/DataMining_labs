rm( list=ls() )
##

library ( rattle.data )
data ( wine )

head(wine)
dim(wine)

plot(wine)

pairs(wine[,-1], col=wine$Type)
pr <- prcomp(wine[,-1], scale. = TRUE)
names(pr)

plot(pr$x[,1:2])
plot(pr$x[,1:2], col=wine$Type)

variance <- pr$sdev^2
# prop_var <- ???
# cumsum(prop_var)
# plot(prop_var, type='b')
# plot(cumsum(prop_var), type='b')

data(gasoline)
?gasoline
dim(gasoline$NIR)
y <- gasoline$octane
x <- gasoline$NIR

pr <- prcomp(x, scale. = TRUE )
dim(x)
length(pr$sdev)

plot(pr$x)

plot(pr$x[,1],y)
plot(pr$x[,2],y)
variance <- pr$sdev^2
prop_var <- variance/sum(variance)
plot(prop_var)
plot(prop_var, type = 'b')
library(pls)
m_pcr <- pcr(y ~ x, ncomp=20, validation = "CV", scale = TRUE)

summary(m_pcr)
validationplot(m_pcr, val.type = "MSEP")
selectNcomp(m_pcr, method = "onesigma")
coefplot(m_pcr, ncomp=1:5)
coefplot(m_pcr, ncomp=1:5, legendpos="bottomleft")
plot(m_pcr)
abline(0,1)

library(glmnet)
cv_lasso <-cv.glmnet(x, y, alpha=1)
cv_lasso$lambda.min
m_lasso <- glmnet(x,y, alpha = 1, lambda = cv_lasso$lambda.min)
cv_lasso$cvm
min(cv_lasso$cvm)
MSEP(m_pcr, ncomp = )