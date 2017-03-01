##########Projet Modeles Lineaires##########


#Importer le jeux de données#
table=read.csv('~/Desktop/M2 Data Science/Regression linéaire /Projet/FW_Donnees_Puits.csv',header=TRUE,sep=";")
attach(table)


#Données de production avec classification expert#
matplot(t(table[c(1:length(table[,1])),-c(1,37,38)]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 700, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
//

#Données de production avec classification CAH#
matplot(t(table[c(1:length(table[,1])),-c(1,37,38)]),type="l",col=Classification.CAH.R,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 700, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 12.33.35.png



####Question 1####


#Polynome de degre 0#
tab=0
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~1)
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 14.17.59.png

#Polynome de degre 1#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~x)
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 357, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 14.28.53.png

#Polynome de degre 2#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~x +I(x^2))
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 405, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 14.30.57.png

#Polynome de degre 3#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~x+I(x^2)+I(x^3))
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 470, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 14.33.16.png

#Polynome de degre 4#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~x+I(x^2)+I(x^3)+I(x^4))
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(30, 540, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-28 à 14.36.08.png




####Question 2####

#Fonction exponetielle à deux paramètres#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=lm(vec~-I(exp(-x)))
    tab=rbind(tab,fitted(model))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas")
legend(-1, -40, c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")
// /Users/hola/Desktop/M2 Data Science/Regression linéaire /Projet/Images/Capture d’écran 2016-12-29 à 09.31.58.png

//Cela marche a peu près, amelioration possibles...//


####Question 3####


p.conf=predict(model,interval="confidence")
matplot(p.conf,type="l")


####Question 5####

#Exemple de lissage avec Loess sur le premier puit en fonction du span#
y=as.numeric(table[1,-c(1,37,38)])
x=seq(1,35,1)
mod1=loess(y~x,span=0.1)
mod2=loess(y~x,span=0.5)
mod3=loess(y~x,span=0.75)
mod4=loess(y~x,span=1)
xfit=seq(from=min(x),to=max(x),length.out=100)
yfit1=predict(mod1,newdata=xfit)
yfit2=predict(mod2,newdata=xfit)
yfit3=predict(mod3,newdata=xfit)
yfit4=predict(mod4,newdata=xfit)
plot(x,y,pch=20,xlab="Mois",ylab="Production de gas",main="Lissage avec Loess en fonction du span")
points(xfit,yfit1,type="l",lwd=2,col="red")
points(xfit,yfit2,type="l",lwd=2,col="blue")
points(xfit,yfit3,type="l",lwd=2,col="forestgreen")
points(xfit,yfit4,type="l",lwd=2,col="lightblue")
legend("topright",c(paste("span=",c(0.1,0.5,0.75,1))), lwd=2,lty=1,col=c("red","blue","forestgreen","lightblue"))



#Lissage avec Loess de 0.1#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=0.1)
    xfit=seq(from=min(x),to=max(x),length.out=100)
    tab=rbind(tab,predict(model,newdata=xfit))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage Loess avec span de 0.1")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")

#Lissage avec Loess de 0.5#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=0.5)
    xfit=seq(from=min(x),to=max(x),length.out=100)
    tab=rbind(tab,predict(model,newdata=xfit))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage Loess avec span de 0.5")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")


#Lissage avec Loess de 0.75#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=0.75)
    xfit=seq(from=min(x),to=max(x),length.out=100)
    tab=rbind(tab,predict(model,newdata=xfit))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage Loess avec span de 0.75")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")


#Lissage avec Loess de 1.5#
tab=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=1.5)
    xfit=seq(from=min(x),to=max(x),length.out=100)
    tab=rbind(tab,predict(model,newdata=xfit))
}
matplot(t(tab[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage Loess avec span de 1.5")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")


#Lissage span 1.5 puis régression polynomiale de degré 3#

tab=0
tabis=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=1.5)
    xfit=seq(from=min(x),to=max(x),length.out=35)
    tab=rbind(tab,predict(model,newdata=xfit))
    vecbis=tab[i+1,]
    modelbis=lm(vecbis~x+I(x^2)+I(x^3))
    tabis=rbind(tabis,fitted(modelbis))
}
matplot(t(tabis[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage de span 1.5 puis régression polynomiale de degré 3")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")

#Lissage span 1.5 puis régression exponentielle#

tab=0
tabis=0
x=seq(1:35)
for (i in 1:length(table[,1])){
    vec=as.numeric(table[i,-c(1,37,38)])
    model=loess(vec~x,span=1.5)
    xfit=seq(from=min(x),to=max(x),length.out=35)
    tab=rbind(tab,predict(model,newdata=xfit))
    vecbis=tab[i+1,]
    a_start <- 80
    b_start <- -2*log(2)/a_start
    modelbis=nls(vec~a*exp(b*x),start=list(a=a_start,b=b_start))
    tabis=rbind(tabis,fitted(modelbis))
}
matplot(t(tabis[-1,]),type="l",col=Classification.Expert,lty=1,xlab="Mois",ylab="Production de gas",main="Lissage de span 1.5 puis régression exponentielle")
legend("topright", c("Bonne", "Moyenne", "Mauvaise"), col = c("red","green","black"), text.col = "black", lty = 1, merge = TRUE, bg = "gray90")




















