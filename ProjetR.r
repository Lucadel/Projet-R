
library('ggplot2')
library('dplyr')
library('tidyr')
library('naniar')
library('GGally')
library('ggthemes')
library('gridExtra')
library('RColorBrewer')
library('caret')
library('e1071')
library('devtools')
library('caret')

#I.Loading dataset
df <- read.csv('Assurance.csv', stringsAsFactors = F)

#II.Replacement by Na Values
NAN=df$Date.de.Soumission[1]
NAN2=df$Pathologie[1]
df<- df %>% replace_with_na_all(condition = ~.x %in% c(NAN,NAN2))

#III.Mode conversion
df$Taux.Escalation<-as.numeric(df$Taux.Escalation)
df$Capital<-as.numeric(df$Capital)
df$Pourcentage.Percu<-as.numeric(df$Pourcentage.Percu)
df$Chargement<-as.numeric(df$Chargement)
df$Salaire<-as.numeric(df$Salaire)
df$Generation<-as.numeric(df$Generation)
df$Categorie.Professionnelle<-as.numeric(df$Categorie.Professionnelle)


#IV.Character to numeric
df$Produit<-as.numeric(factor(df$Produit))

#V. Pathologies non renseignées
df$Pathologie<- factor(sapply(df$Pathologie, function(x) (if (is.na(x)) {x="Non-renseigné"} else {x=x})  ))

#VI.Feature Creation
réf<-as.Date("01/01/2018", format="%d/%m/%Y")
df$age<- as.numeric((réf - as.Date(df$Date.de.Naissance, format="%d/%m/%Y"))/365)
df$maladie_ancienneté<- as.numeric((réf - as.Date(df$Debut.de.Maladie, format="%d/%m/%Y"))/365)
df$retreat_time<- as.numeric((as.Date(df$Date.de.Retraite, format="%d/%m/%Y")-réf)/365)
df$police_duration<- as.numeric((réf-as.Date(df$Date.de.Facturation, format="%d/%m/%Y"))/365)

#Target Creation
df$target<- factor(as.numeric(sapply(df$Fin.de.Maladie, function(x) if (is.na(x)) {x=0} else {x=1} )))

#I.Analyse de la dépendance à la target
#A.Exploration des données catégorielles
Target_Dependance_1<-function (df,feature,txtsize){
    ggplot(df,aes(x=as.factor(df[[feature]]),color=target))+
    geom_bar(alpha=0.4,position='dodge',fill='white')+
    theme(text = element_text(size=txtsize))+
    scale_x_discrete(name =feature)+
    theme(legend.position = "none")
}

#Target_Dependance_1_NAout=>ggplot(df[!is.na(df[[feature]]),],aes(x=as.factor(df[!is.na(df[[feature]]),][[feature]]),color=target))

plot1<-Target_Dependance_1(df,"Generation",8)
plot2<-Target_Dependance_1(df,"Categorie.Professionnelle",8)
plot3<-Target_Dependance_1(df,"Produit",8)
plot4<-Target_Dependance_1(df,"Canal.de.Vente",8)
plot5<-Target_Dependance_1(df,"Fumeur",8)
plot6<-Target_Dependance_1(df,"Genre",8)
dx<-df[df$Periode.Attente %in% c(0,4,8,13,26,52),]
plot7<-Target_Dependance_1(dx,"Periode.Attente",8)

options(repr.plot.width=8, repr.plot.height=6)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol=2)

#Pathologies                          
options(repr.plot.width=7, repr.plot.height=2.5)
daz<-df[df$Pathologie!="Non-renseigné",]
Target_Dependance_1(daz,"Pathologie",8)+ 
theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust=0))

#District
options(repr.plot.width=14, repr.plot.height=2.5)
Target_Dependance_1(df,"District",8)+ 
theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0))
options(repr.plot.width=14, repr.plot.height=2.5)                     

#Traitement: regroupement des catégories
D1<-c("B ","BB ","NG ","YO ","OL ","IV ","L ")
df$District<-factor(sapply(df$District, function(x) if (x %in% D1) {x=x} else {x="Autres"}))
#Plot                          
dy<-df[df$District!="Autres",]
options(repr.plot.width=14, repr.plot.height=2.5)
Target_Dependance_1(dy,"District",8)+ 
theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0))     
     

#Occupation.code
dz<-df[df$Occupation.code<9960,]
options(repr.plot.width=14, repr.plot.height=2.5)
Target_Dependance_1(dz,"Occupation.code",8)+ theme(axis.text.x=element_blank())

df<-df %>% drop_na(Occupation.code)

#Traitement: regroupement des catégories
Professions<-c(42,45,374,472,708,737,1403,2213,2468,4309,4558,5262,5270,5277,5299,6678)
df$Occupation.code<-factor(sapply(df$Occupation.code, function(x) if (x %in% Professions) {x=x} else if (x>9960) {x="Non-renseigné"} else {x="Autres"}))
#Plot
options(repr.plot.width=14, repr.plot.height=2.5)
Target_Dependance_1(df,"Occupation.code",8)+ 
theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0))     
     

#B.Exploration des données continues
Target_Dependance_2<-function (feature,xl){
    ggplot(df, aes(x=df[[feature]], color=target))+
    geom_density(fill="white", alpha=0.5, position="identity")+
    theme(text = element_text(size=8))+
    theme(legend.position = "none")+
    scale_x_continuous(name = paste("Dependence ",feature), limits = xl)
}

plot10<-Target_Dependance_2("Salaire",xl=c(0,100000))
plot20<-Target_Dependance_2("Monthly.benefit.amount",xl=c(-100,1500))
plot30<-Target_Dependance_2("Capital",xl=c(-100,1500))
plot40<-Target_Dependance_2("Chargement",xl=c(-100,300))
plot50<-Target_Dependance_2("Pourcentage.Percu",xl=c(30,130))
plot60<-Target_Dependance_2("Taux.Escalation",xl=c(-0.5,0.5))
plot100<-Target_Dependance_2("age",xl=c(30,100))
plot200<-Target_Dependance_2("maladie_ancienneté",xl=c(10,28))
plot300<-Target_Dependance_2("retreat_time",xl=c(-30,30))
plot400<-Target_Dependance_2("police_duration",xl=c(10,60))

options(repr.plot.width=10, repr.plot.height=6)
grid.arrange(plot10, plot20, plot30, plot40, plot50, plot60, plot100, plot200, plot300, plot400, ncol=2)

#II.Analyse des correlations
#A.Variables numériques
numeric_col <- c()
for (col in names(df)) {
    if (is.numeric(df[[col]])){numeric_col<-c(numeric_col,col)}
}

removal<-c('Numero.de.Police','Nombre.Assures','Chargement')
numeric_col<-numeric_col[!(numeric_col %in% removal)]
numeric_col

dg<- na.omit( df [c("age","retreat_time","police_duration","Capital","maladie_ancienneté","Canal.de.Vente","Produit","Generation","Categorie.Professionnelle","Pourcentage.Percu","Periode.Attente","Salaire","Monthly.benefit.amount")])
cor(dg)

Violin_plot<-function (df,categorical,continuous,yl){
    ggplot(df,aes(x=factor(df[[categorical]]),y=df[[continuous]], fill=factor(df[[categorical]])))+
    geom_violin(trim=FALSE)+ 
    scale_fill_brewer(palette="Blues")+
    xlab(categorical)+
    ylab(continuous)+
    ylim(yl)+theme(text = element_text(size=8))+theme(legend.position = "none")
}

#Generation/age: cor=0.33
options(repr.plot.width=3, repr.plot.height=2)
Violin_plot(df,"Generation","age",c(30,90))

#Produit/Police_duration: cor=0.52
dr<-df[df$Produit %in% c(1,4,5),]
plotb<-Violin_plot(dr,"Produit","police_duration",c(10,40))
plota<-Violin_plot(dr,"Generation","police_duration",c(10,40))
#Produit/Canal de Vente: cor=0.59
plotc<-ggplot(df, aes(x=factor(Produit), y=factor(Canal.de.Vente)) ) +geom_bin2d(bins = 4) +
  scale_fill_continuous(type = "viridis") +xlab("Produit")+ylab("Canal de Vente")
#Produit/Generation: cor=0.50
plotd<-ggplot(df, aes(x=factor(Produit), y=factor(Generation)) ) +geom_bin2d(bins = 4) +
  scale_fill_continuous(type = "viridis") +xlab("Produit")+ylab("Generation")

options(repr.plot.width=14, repr.plot.height=5)
grid.arrange(plota, plotb, plotc, plotd, ncol=2)

#Categorie/Salaire: cor=0.07
options(repr.plot.width=8, repr.plot.height=2)
Violin_plot(df,"Categorie.Professionnelle","Salaire",c(-1000,80000))

#Capital/Monthly.Benefit cor=0.04
plotA1<-ggplot(df, aes(x=Monthly.benefit.amount, y=Capital) ) +geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +xlim(c(0,10000))+ylim(c(0,5000))
#Salaire/Monthly.Benefit cor=0.00
plotA2<-ggplot(df, aes(x=Monthly.benefit.amount, y=Salaire) ) +geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +xlim(c(-100,3000))+ylim(c(-2000,100000))

options(repr.plot.width=14, repr.plot.height=5)
grid.arrange(plotA1, plotA2, ncol=2)

#Capital/police_duration
ggplot(df,aes(x=Capital,y=police_duration))+
geom_point(alpha=0.2)+
ylim(c(10,50))+xlim(c(0,5000))

#Salaire/Pourcentage.Percu
ggplot(df,aes(x=Pourcentage.Percu,y=police_duration))+
geom_point(alpha=0.2)+
ylim(c(10,50))+xlim(c(0,200))

#Feature Selection
Featureselected<-c("target","Pathologie","District","Occupation.code","Fumeur","Genre","age","retreat_time","police_duration","maladie_ancienneté","Periode.Attente","Salaire","Capital","Monthly.benefit.amount","Pourcentage.Percu")
df<-df[Featureselected]

#I.Traitement des valeurs manquantes
sapply(df,function (x) sum(is.na(x)))

#Traitement des valeurs manquantes pour le salaire
Data2<-df %>% 
group_by(cut(age,6))%>%
group_by(Occupation.code)%>%
mutate(Salairebygroup = mean(Salaire,na.rm = TRUE))%>% 
mutate(Salaire  = ifelse(is.na(Salaire), Salairebygroup, Salaire))
df$Salaire<-Data2$Salaire
#Pour les autres variables
df<- na.omit(df)

#II.Suppression des Outliers
sum(df$Salaire>200000)
sum(df$Monthly.benefit.amount>10000|df$Monthly.benefit.amount<(-5000))
sum(df$Pourcentage.Percu>200|df$Pourcentage.Percu<0)
sum(df$Periode.Attente>100)
df<-df[df$Salaire<200000,]
df<-df[df$Monthly.benefit.amount<10000&&df$Monthly.benefit.amount>(-5000),]
df<-df[df$Pourcentage.Percu<200&&df$Pourcentage.Percu>0,]
df<-df[df$Periode.Attente<100,]


#III.Label Encoding des variables binaires
df$Genre<-as.numeric(factor(df$Genre))
df$Fumeur<-as.numeric(factor(df$Fumeur))
df$Occupation.code<-as.factor(df$Occupation.code)
Databinary<-df[c("Genre","Fumeur")] 

#IV. One hot encoding pour les variables District, Patholgie & Occupation.code
dmy1 <- dummyVars(" ~ District", data = df, fullRank=T)
dmy2 <- dummyVars(" ~ Pathologie", data = df, fullrank=T)
dmy3 <- dummyVars(" ~ Occupation.code", data = df, fullrank=T)
District_onehot<- data.frame(predict(dmy1, df))
Pathologie_onehot<- data.frame(predict(dmy2, df))
Occupation_onehot<- data.frame(predict(dmy3, df))

#V.Partie numérique=>Minmax scaler
Numeric_Features<-c("age","retreat_time","police_duration","maladie_ancienneté","Periode.Attente","Salaire","Capital","Monthly.benefit.amount","Pourcentage.Percu")
Datanumeric=df[Numeric_Features]
head(Datanumeric)

#VI.Compilation des variables dummifiées, numériques, binaires et de la target
target=df$target
X<-cbind(Pathologie_onehot,District_onehot,Occupation_onehot,Datanumeric,Databinary,target)
str(X)

head(X)

#VIII.Extraction pour les Machine Learner du groupe
#write.csv(X, file = "DataforML.csv")

#Shuffle dataset
df <- df[sample(nrow(df)),]

# Random sample indexes
train_index <- sample(1:nrow(X), 0.75 * nrow(X))
test_index <- setdiff(1:nrow(X), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- X[train_index, -55]
y_train <- X[train_index, "target"]

X_test <- X[test_index, -55]
y_test <- X[test_index, "target"]

#Fit Standard scaler for numeric features on X_train
pp = preProcess(X_train[Numeric_Features], method = "scale")

Numeric_train<-predict(pp,X_train[Numeric_Features])
X_train<-cbind(Numeric_train, select(X_train, -Numeric_Features))

Numeric_test<-predict(pp,X_test[Numeric_Features])
X_test<- cbind(Numeric_test,select(X_test, -Numeric_Features))


head(X_train)

set.seed(123)
library(randomForest)
fit <- randomForest(y_train ~ ., data = X_train,ntree = 2000,mtry=30)

#Performance:Training set
y_pred_train = predict(fit, newdata = X_train)
confusion_matrix=table(y_train, y_pred_train)
accuracy_train<-sum(diag(confusion_matrix)/sum(confusion_matrix))
print(confusion_matrix)
cat("\n","Accuracy sur l'échantillon de train: ",accuracy_train*100,"%")

#Performance: Test set
y_pred_test = predict(fit, newdata = X_test)
confusion_matrix=table(y_test, y_pred_test)
accuracy_test<-sum(diag(confusion_matrix)/sum(confusion_matrix))
print(confusion_matrix)
cat("\n","Accuracy sur l'échantillon de test: ",accuracy_test*100,"%")

options(repr.plot.width=8, repr.plot.height=7)
varImpPlot(fit)


