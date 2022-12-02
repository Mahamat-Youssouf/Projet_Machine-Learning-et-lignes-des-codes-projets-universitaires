---
  title: "MON MEMOIRE SUR RSTUDIO"
author: "Youssouf"
date: '2022-06-19'
---
  ```{r}
#### 1. Charger la base de donnes de l'echantillon complet
#base<-read_xlsx("Base  des données de panel  pays.xlsx")
#base<-read_excel("Base  des données de panel  pays.xlsx",col_names = TRUE)
base<-read.csv("D:/Cours M1 _EA _Semestre2_2021-2022/Mon memoire M1/Base_des_données_de_panel_pays.csv",
               fileEncoding = "Latin1",check.names = F,sep = ";",dec = ".")
attach(base)
#ON declare que ce sont des données de panel
Pbase<-pdata.frame(base,index=c("id","year"))
attach(Pbase)
```



```{r}
#Creation des log des variables base total
Pbase$lInfra<-log(Infra+1)
Pbase$lDevefi<-log(Devfi+1)
Pbase$lTch<-log(Tch)
Pbase$lCphu<-log(Cphu)
```


```{r}
#Blundell and Bond (1998) en Systeme(onestep)
#l'echantillon complet(42 pays)
diff_on <- pgmm(IDE ~ lag(IDE, 1:1) + PIBh + lInfra + IPC + 
                  Rnat + lTch + Bco + lDevefi + lCphu + FBCF 
                + Depu + Copu + Stap | lag(IDE, 2:99),
                data = Pbase,
                effect = "individual",
                model = "twosteps", 
                collapse = TRUE,
                transformation = "ld",
                fsm = "full",
                subset =sample==1)
summary(diff_on,robust = TRUE)
```






```{r}
#Blundell and Bond (1998) en SYTEME(onestep)
#l'echantillon complet(42 pays)
diff_on1 <- pgmm(IDE ~ lag(IDE, 1:1) + PIBh + lInfra + IPC + 
                   Rnat + lTch + Bco + lDevefi + lCphu + FBCF 
                 + Depu + Copu +Corr | lag(IDE, 2:99),
                 data = Pbase,
                 effect = "individual",
                 model = "twosteps", 
                 collapse = TRUE,
                 transformation = "ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on1,robust = TRUE)
```




```{r}
#Blundell and Bond (1998) en SYTEME(onestep)
#l'echantillon complet(42 pays)
diff_on2 <- pgmm(IDE ~ lag(IDE, 1:1) + PIBh + lInfra + IPC + 
                   Rnat + lTch + Bco + lDevefi + lCphu + FBCF 
                 + Depu + Copu + Demo | lag(IDE, 2:99)+lag(Demo, 2:99),
                 data = Pbase,
                 effect = "individual",
                 model = "twosteps", 
                 collapse = TRUE,
                 transformation = "ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on2,robust = TRUE)
```




```{r}
#Blundell and Bond (1998) en SYTEME(onestep)
#l'echantillon complet(42 pays)
diff_on3 <- pgmm(IDE ~ lag(IDE, 1:1) + PIBh + lInfra + IPC + 
                   Rnat + lTch + Bco + lDevefi + lCphu + FBCF 
                 + Depu + Copu + Qure | lag(IDE, 2:99)+lag(Demo, 2:99),
                 data = Pbase,
                 effect = "individual",
                 model = "twosteps", 
                 collapse = TRUE,
                 transformation = "ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on3,robust = TRUE)
```




```{r}
#Blundell and Bond (1998) en SYTEME(onestep)
#l'echantillon complet(42 pays)
diff_on4<- pgmm(IDE ~ lag(IDE, 1:1) + PIBh + lInfra + IPC + 
                  Rnat + lTch + Bco + lDevefi + lCphu + FBCF 
                + Depu + Copu +Egpu| lag(IDE, 2:99),
                data = Pbase,
                effect = "individual",
                model = "twosteps", 
                collapse = TRUE,
                transformation = "ld",
                fsm = "full",
                subset =sample==1)
summary(diff_on4,robust = TRUE)
```





```{r}
#Lien pgmm:https://rdrr.io/rforge/plm/man/pgmm.html
#Blundell and Bond (1991) en diff(onestep)
#l'echantillon complet(42 pays)
diff_on5 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo+Qure
                 +Egpu+Etdr|lag(IDE, 2:99),
                 data=Pbase,
                 model="twosteps",
                 effect="individual",
                 collapse = TRUE,
                 transformation="ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on5,robust = TRUE)
```



library(stargazer)
stargazer(diff_on, diff_on1,diff_on2,diff_on3,diff_on4,diff_on5,
          type="text",
          title="Tableau:Regression Results",
          out="Effet indivSystwosteps.doc",
          dep.var.labels = c("Investissement directs étrangers"),
          covariate.labels = c("IDE retardé","Taille du marché","Infrastructures(log)",
                               "Inflation","Ressources naturelles","Taux de change(log)",
                               "Balance commerciale","Développement financier(log)",
                               "Capital humain(log)","Formation Brute de capital fixe",
                               "Dette publique","Consommation publique",
                               "Stabilité politique","Corruption","Démocratie",
                               "Qualité de la régulation","Efficacité de la gouvernance publique",
                               "Etat de droit"),
          align = TRUE,single.row=FALSE,digits = 3,
          no.space = TRUE,
          intercept.bottom = T,
          column.labels = c("Modèle1", "Modèle2", "Modèle3","Modèle4", "Modèle5", "Modèle6"),
          column.separate = c(1,1,1),
          t.auto = T,
          p.auto = F,
          font.size = "small",
          report = "vcs*",
          header = FALSE,
          notes.align = "l",
          notes = c("les valeurs entre parenthèses", "en dessous des coéfficients","estimés représentent les écart-types"),
          notes.append = TRUE,
          notes.label = "niveaux de significativité:")

```






```{r}
#Test  autocorreltion Arrellano
mtest(diff_on, order = 2)
mtest(diff_on1, order = 2)
mtest(diff_on2, order = 2)
mtest(diff_on3, order = 2)
mtest(diff_on4, order = 2)
mtest(diff_on5, order = 2)
```
```{r}
#Teste de Hansen
library(pdynmc)
jtest(diff_on)

```


Estimation GMM en diff  par sous regions

```{r}
#### 1. Charger la base de donnes de l'echantillon complet
#base<-read_xlsx("Base  des données de panel  pays.xlsx")
#base<-read_excel("Base  des données de panel  pays.xlsx",col_names = TRUE)
base<-read.csv("D:/Cours M1 _EA _Semestre2_2021-2022/Mon memoire M1/Base_des_données_de_panel_pays.csv",
               fileEncoding = "Latin1",check.names = F,sep = ";",dec = ".")
attach(base)
#ON declare que ce sont des données de panel
Pbase<-pdata.frame(base,index=c("id","year"))
attach(Pbase)
```



```{r}
#Creation des log des variables base total
Pbase$lInfra<-log(Infra+1)
Pbase$lDevefi<-log(Devfi+1)
Pbase$lTch<-log(Tch)
Pbase$lCphu<-log(Cphu)
```






```{r}
#Arellano and Bond (1991) en diff(twostep)
#Sous-echantillon Afrique 
diff_on <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                +lDevefi+lCphu+FBCF+Depu+            Stap
                |lag(IDE, 2:99)+lag(lInfra,2:99),
                data=Pbase,
                model="twosteps",
                effect="individual",
                collapse = TRUE,
                transformation="d",
                fsm = "G",
                subset =sample==1)
summary(diff_on,robust = FALSE)
```





```{r}
#Arellano and Bond (1991) en diff(onestep)
#Sous-echantillon Afrique occidentale
diff_on1 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr
                 |lag(IDE, 2:99)+lag(lInfra,2:99),
                 data=Pbase,
                 model="onestep",
                 effect="individual",
                 collapse = TRUE,
                 transformation="ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on1,robust = TRUE)
```




```{r}
#Arellano and Bond (1991) en diff(onestep)
##Sous-echantillon Afrique occidentale
diff_on2 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo
                 |lag(IDE, 2:99)+lag(lInfra,2:99),
                 data=Pbase,
                 model="onestep",
                 effect="individual",
                 collapse = TRUE,
                 transformation="ld",
                 fsm = "full",
                 subset =sample==1)
summary(diff_on2,robust = TRUE)
```





```{r}
#Arellano and Bond (1991) en diff(onestep)
#Sous-echantillon Afrique occidentale
diff_on3 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo+Qure
                 |lag(IDE, 2:99)+lag(lInfra,2:99),
                 data=Pbase,
                 model="onestep",
                 effect="individual",
                 collapse = TRUE,
                 transformation="d",
                 fsm = "G",
                 subset =sample==1)
summary(diff_on3,robust = FALSE)
```





```{r}
#Arellano and Bond (1991) en diff(onestep)
#Sous-echantillon Afrique occidentale
diff_on4 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo+Qure
                 +Egpu
                 |lag(IDE, 2:99)+lag(lInfra,2:99),
                 data=Pbase,
                 model="onestep",
                 effect="individual",
                 collapse = TRUE,
                 transformation="d",
                 fsm = "G",
                 subset =sample==1)
summary(diff_on4,robust = FALSE)
```






```{r}
#Lien pgmm:https://rdrr.io/rforge/plm/man/pgmm.html
#Arellano and Bond (1991) en diff(onestep)
#Sous-echantillon Afrique occidentale
diff_on5 <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                 +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo+Qure
                 +Egpu+Etdr
                 |lag(IDE, 2:99)+lag(lInfra,2:99),
                 data=Pbase,
                 model="onestep",
                 effect="individual",
                 collapse = TRUE,
                 transformation="d",
                 fsm = "G",
                 subset =sample==1)
summary(diff_on5,robust = FALSE)
```






```{r}
# export to doc
library(texreg)
screenreg(list("Mondele1"=diff_on,"Mondele2"=diff_on1,
               "Mondele3"=diff_on2,"Mondele4"=diff_on3,
               "Mondele5"=diff_on4,"Mondele5"=diff_on5),
          digits = 3,no.space = TRUE)
```





```{r}
library(stargazer)
stargazer(diff_on, diff_on1,diff_on2,diff_on3,diff_on4,diff_on5,
          type="text",
          title="Tableau:Regression Results",
          out="CompletDifftwostep.doc",
          dep.var.labels = c("Investissement directs étrangers"),
          covariate.labels = c("IDE retardé","Taille du marché","Infrastructures(log)",
                               "Inflation","Ressources naturelles","Taux de change(log)",
                               "Balance commerciale","Developpement financier(log)",
                               "Capital humain(log)","Formation Brute de capital fixe",
                               "Dette publique","Consommation publique",
                               "Stabilité politique","Corruption","Démocratie",
                               "Qualité de la regulation","Efficacité de la gouvernance publique",
                               "Etat de droit"),
          align = TRUE,single.row=FALSE,digits = 3,
          no.space = TRUE,
          intercept.bottom = T,
          column.labels = c("Modèle1", "Modèle2", "Modèle3","Modèle4", "Modèle5", "Modèle6"),
          column.separate = c(1,1,1),
          t.auto = F,
          p.auto = F,
          font.size = "small",
          report = "vcs*",
          header = FALSE,
          notes.align = "l",
          notes = c("les valeurs entre parenthèses", "en dessous des coéfficients","estimés représentent les écart-types"),
          notes.append = TRUE,
          notes.label = "niveaux de significativité:")

```




```{r}
#Test  autocorreltion Arrellano
mtest(diff_on, order = 2)
mtest(diff_on1, order = 2)
mtest(diff_on2, order = 2)
mtest(diff_on3, order = 2)
mtest(diff_on4, order = 2)
mtest(diff_on5, order = 2)
```



Estimation par GMM sy two step sur chaque sous région

```{r}
#### 1. Charger la base de donnes de l'echantillon complet
#base<-read_xlsx("Base  des données de panel  pays.xlsx")
#base<-read_excel("Base  des données de panel  pays.xlsx",col_names = TRUE)
base<-read.csv("D:/Cours M1 _EA _Semestre2_2021-2022/Mon memoire M1/Panel-Afrique-occidentale.csv",
               fileEncoding = "Latin1",check.names = F,sep = ";",dec = ".")
attach(base)
#ON declare que ce sont des données de panel
Pbase<-pdata.frame(base,index=c("id","year"))
attach(Pbase)
```



```{r}
#Creation des log des variables base total
Pbase$lInfra<-log(Infra+1)
Pbase$lDevefi<-log(Devfi+1)
Pbase$lTch<-log(Tch)
Pbase$lCphu<-log(Cphu)
```





```{r}
#Afrique occidentale
#Lien pgmm:https://rdrr.io/rforge/plm/man/pgmm.html
#Blundell and Bond (1991) en diff(onestep)
#l'echantillon complet(42 pays)
diff_on <- pgmm(IDE~lag(IDE,1:1)+PIBh+lInfra+IPC+Rnat+lTch+Bco
                +lDevefi+lCphu+FBCF+Depu+Copu+Stap+Corr+Demo+Qure
                +Egpu+Etdr|lag(IDE, 2:99),
                data=Pbase,
                model="twosteps",
                effect="individual",
                collapse = TRUE,
                transformation="ld",
                fsm = "full",
                subset =sample==1)
summary(diff_on,robust = TRUE)
```



