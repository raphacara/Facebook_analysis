library(tidyverse)
library(ggplot2)
library(lubridate)
library(naniar)
library(plyr)
library(dplyr)
library(plotly)
library(skimr)
library(FactoMineR)
library(factoextra)
library(broom)

Live_20210128

glimpse(Live_20210128)

data <- Live_20210128[, -c(13:16)] # on supprime les dernières colonnes contenant les données manquantes
data # on visualise le data

ls(data)# nous affiche toute les variables de notre data

f<-skim(data)
f# cette fonction permet de categoriser nos variable en character ou en numérique et d'effectuer une analayse sur nos variable 

data%>%
  select(status_id, num_reactions, num_comments, num_shares, num_likes, num_loves, num_wows, num_hahas, num_sads, num_angrys)%>%
  boxplot(xlab= "Les variables", ylab="Statistiques", main="Statistique descriptive des variables", col="red", lwd=0.5, lty=1)

ggplot(data) + geom_boxplot(aes(y=num_reactions), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de réaction",x="Nombre de réaction")

ggplot(data) + geom_boxplot(aes(y=num_comments), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de commentaire",x="Nombre de commentaire")

ggplot(data) + geom_boxplot(aes(y=num_shares), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de partage",x="Nombre de partage", y = "Status des Id")

ggplot(data) + geom_boxplot(aes(y=num_likes), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de likes",x="Nombre de likes")

ggplot(data) + geom_boxplot(aes(y=num_loves), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de j'adore",x="Nombre de j'adore")

ggplot(data) + geom_boxplot(aes(y=num_wows), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre de Wows",x="Nombre de Wows")

ggplot(data) + geom_boxplot(aes(y=num_hahas), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre d'émoji drôle",x="Nombre d'émoji drôle")

ggplot(data) + geom_boxplot(aes(y=num_sads), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre d'émoji triste",x="Nombre d'émoji triste")

ggplot(data) + geom_boxplot(aes(y=num_angrys), fill="wheat",color="tomato4") +theme_classic() + labs(title="Boxplot du nombre d'émoji énervé",x="Nombre d'émoji énervé")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_loves, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre de j'adore",x="Type de status", y="Nombre de j'adore")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_reactions, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre de réactions",x="Type de status", y="Nombre de réactions")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_comments, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre de commentaire",x="Type de status", y="Nombre de commentaire")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_shares, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre de partage",x="Type de status", y="Nombre de partage")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_wows, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre de Wows",x="Type de status", y="Nombre de Wows")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_hahas, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre d'émoji drôle",x="Type de status", y="Nombre d'émoji drôle")

data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_sads, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre d'émoji triste",x="Type de status", y="Nombre d'émoji triste")
data%>%
  group_by(status_type)%>%
  ggplot() + geom_boxplot(aes(x=status_type, y=num_angrys, color= status_type), width=0.8) + theme_classic() + labs(title="Boxplot du nombre d'émoji énervé",x="Type de status", y="Nombre d'émoji énervé")

plot_ly( data = data,
         x = ~num_reactions,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de réaction"), yaxis = list(title ="Effectif"), title = "Nombre de réaction par publication") 

plot_ly( data = data,
         x = ~num_comments,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de commentaire"), yaxis = list(title ="Effectif"), title = "Nombre de commentaires par publication") 

plot_ly( data = data,
         x = ~num_shares,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de partages"), yaxis = list(title ="Effectif"), title = "Nombre de partages par publication") 

plot_ly( data = data,
         x = ~num_likes,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de likes"), yaxis = list(title ="Effectif"), title = "Nombre de likes par publication") 

plot_ly( data = data,
         x = ~num_loves,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de j'adore"), yaxis = list(title ="Effectif"), title = "Nombre de j'adores par publication") 

plot_ly( data = data,
         x = ~num_wows,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre de wows"), yaxis = list(title ="Effectif"), title = "Nombre de wows par publication") 

plot_ly( data = data,
         x = ~num_hahas,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre d'émoji drôle"), yaxis = list(title ="Effectif"), title = "Nombre d'émoji drôle par publication") 

plot_ly( data = data,
         x = ~num_sads,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre d'émoji triste"), yaxis = list(title ="Effectif"), title = "Nombre d'émoji triste par publication") 

plot_ly( data = data,
         x = ~num_angrys,
         type = 'histogram',
         marker = list(color = 'rgb(219,112,147)'),
         width = 1)%>% 
layout( xaxis = list(title = "Histogram du nombre d'émoji énervé"), yaxis = list(title ="Effectif"), title = "Nombre d'émoji énervé par publication") 

var(data$num_comments)
var(data$num_shares)
var(data$num_likes)
var(data$num_loves)

data%>%
  select(num_comments,num_shares, num_likes, num_loves)%>%
  scale()%>% # centrer reduire les données
  prcomp() -> acp
acp$rotation

summary(acp)

library(tidyr)
library(tidyverse)
acp %>%
  tidy("pcs")%>%
  ggplot(aes(x=PC, y=percent))+
  geom_col(fill="lightblue", alpha=0.7) +
  geom_point(size=2) +
  geom_line(color="blue", size=1.2)+
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Variance explained",
       title="Scree plot") + theme_classic()

acp %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=cumulative))+
  geom_line(color="lightblue", size=2)+
  geom_point(color= "red", size=3) +
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+ theme_minimal() +
  labs(y= "Cumulative Variance explained",
       title="Scree plot")

biplot(acp)

var <- get_pca_var(acp)

fviz_pca_biplot(acp, geom.ind = "point", 
                pointshape= 20,
                pointsize=2,# Montre les points seulement (mais pas le "text")
             col.ind = data$status_type, # colorer by groups
             palette = "jco",
             addEllipses = TRUE,
             col.var= "black",
             label= "var",
             repel= TRUE,
             legend.title = "Type de statuts")

head(var$coord,4) # coordonnées des variables 
fviz_pca_var(acp, col.var="cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

head(var$contrib, 4)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Fonction pareto
pareto = function(x, bar.col="cyan", line.col="red", pch=16, h=80, h.lty=3,main="",xlab="Défauts",ylab="Fréquence (%)", names.arg=c(), ylab2="Cumul",mar=c(5,4,3,4)) {
if (length(names.arg)>0) {names.arg=names.arg[order(x, decreasing = TRUE)]}
x = sort(x,decreasing=T); x = x*100/sum(x);
cumul = (cumsum(x)/sum(x))*100
simulation = barplot(x,col=bar.col) ; simulation
plot.new()
par(mar=mar)
barplot(x,col=bar.col,axes=F,ylim=c(0,100),main=main,xlab=xlab,ylab="",names.arg=names.arg)
#par(new=TRUE)
points(simulation,cumul,pch=pch,col=line.col,xlab="",ylab="",type="o")
abline(h=h,lty=h.lty) ; box()
axis(2) ; axis(4,c(0,20,40,60,80,100),col.axis=line.col,col=line.col)
mtext(ylab,side=2,line=2,cex=1.2) ; mtext(ylab2,side=4,col="red",line=2,cex=1.2)
result = c(x , cumul) ; result = matrix(result,nc=length(x), byrow=T)
if (length(names.arg)>0) {colnames(result) = names.arg } 
rownames(result) = c("frequency","cumul")
return(result)}

data%>%
  select(status_id, num_reactions, num_comments, num_shares, num_likes, num_loves, num_wows, num_hahas, num_sads, num_angrys)-> dat
cp <- PCA(dat, scale.unit= T, ncp=5, graph=T)
pareto(cp$eig[,2], h=95) # h = 95 : afficher un seuil de cumul de la variance à 95%
eig <- get_eigenvalue(cp)
eig

dat%>%
  scale()%>%
  prcomp() -> apc
summary(apc)
apc$rotation
apc%>%
  tidy("pcs")

apc %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=percent))+
  geom_col(fill="dodgerblue", alpha=0.7) +
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Variance explained",
       title="Scree plot")

apc %>%
  tidy("pcs") %>%
  ggplot(aes(x=PC, y=cumulative))+
  geom_point(size=4) +
  geom_line(color="red", size=2)+
  scale_y_continuous(labels=scales::label_percent(),
                     breaks = scales::breaks_pretty(n=6))+
  labs(y= "Cumulative Variance explained",
       title="Scree plot")

data%>%
  select(num_shares, num_comments, num_likes, num_loves, num_hahas, num_wows, num_sads, num_angrys)%>%
  cor()

library(car)
dat%>%
  scale() -> d
scatterplot(num_shares~num_loves, data=d)

regression = lm(num_shares~ num_loves, data=data) #on definit la régression linéaire
summary(regression) # on affiche les résultats et le coefficient, les significativé et le R2

coef(regression) #extraction des coeffcients estimés 

confint(regression) #Intervalle de confiance (à 95%) des coefficients

summary(regression)

summary(regression)

library(leaps)

data%>%
  select(num_shares, num_comments, num_likes, num_loves, num_hahas, num_wows, num_sads, num_angrys) -> da
models <- regsubsets(num_shares~., data=da)
summary(models)

mod <-summary(models)
mod$rsq #visualisation des r2 
adj= which.max(mod$adjr2)
plot(mod$rsq, xlab = "Nombre de variables", ylab = "R2", type = "l", col= "blue")
title("Courbe de R2 en fonciton du nombre de variables")
points(adj, mod$adjr2[adj], col ="red", cex = 2, pch = 20)

which.max(mod$adjr2) # model avec le meilleur R2 ajusté

plot(models, scale="adjr2")

plot(models, scale="r2")

coef(models, 7)

regr = lm(num_shares~ num_loves + num_comments+ num_hahas +num_wows +num_sads, data=data) #on definit la régression linéaire
summary(regr) # on affiche les résultats et le coefficient, les significativé et le R2

coef(regr)

summary(regr)
