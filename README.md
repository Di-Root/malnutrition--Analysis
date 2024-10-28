# malnutrition--Analysis
Code for Bayesian space-time variation in different forms of malnutrition among children under-five in Nigeria).
keep hw1 b4 bord v012 v106 v101 v445 v190 v025 sstate v130 v005 hw5 hw8 hw11
gen swt = v005/1000000
* Coding the catergorical variables as dummies
* Type of place of residence (taking rural as reference category)
gen urban=.
replace urban=1 if v025==1
replace urban=0 if v025==2

* Educational attainment (taking no education as reference category)
gen prim=.
replace prim=1 if v106==1
replace prim=0 if v106==0
replace prim=0 if v106==2
replace prim=0 if v106==3

gen sec_high=.

replace sec_high=0 if v106==1
replace sec_high=1 if v106==2 
replace sec_high=1 if v106==3

*Household wealth index

gen poorer_middle=.
replace poorer_middle=0 if v190==1
replace poorer_middle=1 if v190==2
replace poorer_middle=1 if v190==3
replace poorer_middle=0 if v190==4
replace poorer_middle=0 if v190==5

gen richer_richest=.
replace richer_richest=0 if v190==1
replace richer_richest=0 if v190==2
replace richer_richest=0 if v190==3
replace richer_richest=1 if v190==4
replace richer_richest=1 if v190==5

recode v130 1=1 2/3=1 4=2 5/6=3

*religion  (taking catholic as reference category) 

gen islam =.
replace islam = 1 if v130 ==2
replace islam = 0 if v130 ==1
replace islam = 0 if v130 ==3

gen traditionalist_other=.
replace traditionalist_other =1 if v130==3
replace traditionalist_other =6 if v130==1
replace traditionalist_other =0 if v130==2


*Birth order, we took the first birth as reference 
gen bord_2_3=.
replace bord_2_3=0 if bord==1
replace bord_2_3=1 if bord==2
replace bord_2_3=1 if bord==3
replace bord_2_3=0 if bord >=4

gen bord_4=.
replace bord_4=0 if bord==1
replace bord_4=0 if bord==2
replace bord_4=0 if bord==3
replace bord_4=1 if bord >=4

* Sex of the child
gen female=.
replace female=0 if b4==1
replace female=1 if b4==2

* Child's age in months considered as a continuous variable hence, not categorized
gen child_age = hw1


* State of residence used as spatial random effect. There coding was done to ensure the code for each state correspond to the code in the boundary file
gen state=.
replace state=21 if sstate==10
replace state=36 if sstate==20
replace state=12 if sstate==30
replace state=25 if sstate==40
replace state=30 if sstate==50
replace state=6 if sstate==60
replace state=8 if sstate==70
replace state=34 if sstate==80
replace state=3 if sstate==90
replace state=11 if sstate==100
replace state=10 if sstate==110
replace state=26 if sstate==120
replace state=15 if sstate==130
replace state=37 if sstate==140
replace state=35 if sstate==150
replace state=19 if sstate==160
replace state=29 if sstate==170
replace state=5 if sstate==180
replace state=27 if sstate==190
replace state=13 if sstate==200
replace state=18 if sstate==210
replace state=28 if sstate==220
replace state=33 if sstate==230
replace state=17 if sstate==240
replace state=4 if sstate==250
replace state=2 if sstate==260
replace state=24 if sstate==270
replace state=32 if sstate==280
replace state=7 if sstate==290
replace state=1 if sstate==300
replace state=22 if sstate==310
replace state=9 if sstate==320
replace state=20 if sstate==330
replace state=31 if sstate==340
replace state=23 if sstate==350
replace state=14 if sstate==360
replace state=16 if sstate==370

* Dropping missing observations from the nutritional variables
drop if hw5==9996
drop if hw5==9997
drop if hw5==9998
drop if hw5==9999
drop if v130==96

drop if hw8==9996
drop if hw8==9997
drop if hw8==9998
drop if hw8==9999


drop if hw11==9996
drop if hw11==9997
drop if hw11==9998
drop if hw11==9999

* mother age 
gen ma_cat=.
replace ma_cat =0 if v012 >20 & v012<=30
replace ma_cat =1 if v012>30& v012 <=40
replace ma_cat =2 if v012  >40 
label define ma_cat 0 "ma_30" 1 "ma_40" 2 "ma_over40"
label values ma_cat ma_cat
*mother age ,taking ma_30 as reference 
gen ma_40=.
replace ma_40=0 if ma_cat==0
replace ma_40=1 if ma_cat ==1
replace ma_40=0 if ma_cat==2

gen ma_over40=.
replace ma_over40=0 if ma_cat==0
replace ma_over40=0 if ma_cat==1
replace ma_over40=1 if ma_cat==2

*Body Mass Index
gen BMI = v445/100

gen bmi_cat=.
replace bmi_cat =0 if BMI < 18.5
replace bmi_cat =1 if BMI >= 18.5 & BMI <25
replace bmi_cat =2 if BMI > 25 & BMI <30
replace bmi_cat =3 if BMI >= 30 & BMI <40
label define bmi_cat 0 "underweight" 1 "normal" 2 "overweight" 3 "obesity"
label values bmi_cat bmi_cat

*BMI ,taking underweight as reference 
gen normal=.
replace normal=1 if bmi_cat==1
replace normal=0 if bmi_cat==2
replace normal=0 if bmi_cat==3

gen overweight_obesity=.
replace overweight=0 if bmi_cat==1
replace overweight=1 if bmi_cat==2 
replace overweight=1 if bmi_cat==3

order hw5 hw8 hw11 swt hw1
br hw5 hw8 hw11
gen haz = hw5/100
gen waz = hw8/100
gen whz = hw11/100
order haz waz whz
order hw5 hw8 hw11 swt hw1
order hw5 hw8  hw11 hw1 swt hw1
br hw5 hw8 hw11 haz waz whz
sum haz waz whz
replace haz =. if haz >6
replace waz =. if waz >6
replace whz =. if whz >6
sum haz waz whz

gen stunting = 1 if haz <-2
replace stunting =0 if haz >= -2
replace stunting =. if haz ==.
tab stunting [ iweight = swt]

gen underweight = 1 if waz <-2
replace underweight =0 if waz >= -2
replace underweight =. if waz ==.
tab underweight [ iweight = swt]

gen wasting = 1 if whz <-2
replace wasting =0 if whz >=-2
replace wasting =. if whz ==.
tab wasting [ iweight = swt]


* Drop missing values before analysing the data
drop if v130==.
drop if ma_cat==.
drop if bmi_cat==.
drop if stunting==.
drop if underweight==.
drop if wasting==.
sum stunting underweight wasting 



* Computing summary statistics for the nutitional variables
by urban, sort: sum stunting  underweight wasting 
by v106,  sort: sum stunting  underweight wasting 
by v190,  sort: sum stunting  underweight wasting 
by female, sort: sum stunting  underweight wasting 
by v130,   sort: sum stunting  underweight wasting 
sum stunting  underweight wasting 

* Computing the percentages of undernuorished children based on the variables considered

by urban, sort: tab stunting
by v106, sort: tab  stunting
by v190, sort: tab  stunting
by female, sort: tab stunting
by v130, sort: tab   stunting
tab stunting

by urban, sort: tab wasting 
by v106, sort: tab  wasting
by v190, sort: tab  wasting
by female, sort: tab wasting 
by v130, sort: tab  wasting
tab wasting 

by urban, sort: tab  underweight
by v106, sort: tab  underweight
by v190, sort: tab  underweight
by female, sort: tab  underweight
by v130, sort: tab  underweight
tab underweight

* Tabulating the proportion of individuals in each categories of the variables considered 
tab urban, m
tab v106, m
tab v190, m
tab female, m
tab ma_cat, m
tab bmi_cat
R CODE
library (spData)
library (sf)
library (ggplot2)
library (spdep)
library (INLA)
library(tidyverse)
library(dplyr)

ma1<-read.csv("c:/Users/HP/Documents/2008.csv", header=T)
ma2<-read.csv("c:/Users/HP/Documents/2013.csv", header=T)
ma3<-read.csv("c:/Users/HP/Documents/2018.csv", header=T)

malnutrion <- rbind(ma1,ma2,ma3)

malnutrion <- malnutrion %>%
  rename("region"= "v101")

malnutrion <- malnutrion %>%
  rename("year"= "v007")

view(malnutrion)

NG_map <- st_read("C:/Users/HP/Documents/shps/sdr_subnational_boundaries2.shp")
class(NG_map)
head (NG_map)
plot(NG_map)

NG_mapsf <- st_as_sf(NG_map)
#ggplot(NG_map) + geom_sf(aes(fill=DHSREGEN)) + theme_bw()
nb<-poly2nb(NG_map)
head(nb)

nb2INLA ("NGR.graph", nb)
NGR.adj <- paste(getwd(),"/NGR.graph",sep="")
#image(inla.graph2matrix(NGR.adj),xlab="",ylab="")

d.add = 1E-4
shyper = list(prec = list(prior = "pcprec", param = c(1,0.01)))

formula <- stunting~-1+f(state,model = "besag", graph = NGR.adj, scale.model=T, diagonal = d.add, hyper = "shyper")+
  urban + prim + sec + high + poorer+ middle + richer + richest + islam  + bord_2_3 + bord_4 + female +
  under_3 + above_3 + under_35 + over_36 + normal + overweight + obesity 

m1 <- inla(formula,family ="binomial",data = malnutrion,Ntrials = 1,)

summary(m1)

library(tmap)
windows(record=T)
NG_map <- st_read("C:/Users/HP/Documents/shps/sdr_subnational_boundaries2.shp")
View(NG_map)
sp1 <- m1$summary.random$state
View(sp1)

sp1$REGCODE <- sp1$ID*10

NG_mapsp1 <- inner_join(NG_map,sp1)

NG_mapsp1$emean <- exp(NG_mapsp1$mean)


#NG_map forstunting

tm_shape(NG_mapsp1)+tm_polygons("emean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","bottom"),text.size = 0.6)


View(m1$summary.fitted.values)

d1 <- data.frame(malnutrion,m1$summary.fitted.values)
View(d1)

e1 <- d1 %>% group_by(state,year)

e1 <- summarise(e1, pmean=mean(mean))
View(e1)                
e1$REGCODE <- e1$state*10

stunting_map <- inner_join(NG_map,e1)

tm_shape(stunting_map)+tm_layout(title = "stunting-Nigeria")+tm_polygons("pmean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","bottom"),text.size = 0.6)+tm_facets(by = "year", nrow =1, free.coords = FALSE)

#underweight prevanlence yearly

d.add = 1E-4
shyper = list(prec = list(prior = "pcprec", param = c(1,0.01)))


formula <- underweight~-1+f(state,model = "besag", graph = NGR.adj, scale.model=T, diagonal = d.add, hyper = "shyper")+
  urban + prim + sec + high + poorer+ middle + richer + richest + islam  + bord_2_3 + bord_4 + female +
  under_3 + above_3 + under_35 + over_36 + normal + overweight + obesity 


m2 <- inla(formula,family ="binomial",data = malnutrion,Ntrials = 1,)

summary(m2)
library(tmap)
NG_map <- st_read("C:/Users/HP/Documents/shps/sdr_subnational_boundaries2.shp")
View(NG_map)
sp2 <- m2$summary.random$state
View(sp2)

sp2$REGCODE <- sp2$ID*10

NG_mapsp2 <- inner_join(NG_map,sp2)

NG_mapsp2$emean <- exp(NG_mapsp2$mean)


#NG_map for underweight
tm_shape(NG_mapsp2)+tm_polygons("emean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","bottom"),text.size = 0.6)


d2 <- data.frame(malnutrion,m2$summary.fitted.values)
View(d2)

e2 <- d2 %>% group_by(state,year)

e2 <- summarise(e2, pmean=mean(mean))
View(e1)                
e2$REGCODE <- e2$state*10

underweight_map <- inner_join(NG_map,e2)

tm_shape(underweight_map)+tm_layout(title = "underweight-Nigeria")+tm_polygons("pmean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","bottom"),text.size = 0.6)+tm_facets(by = "year", nrow =1, free.coords = FALSE)

#wasting prevalence yearly

d.add = 1E-4
shyper = list(prec = list(prior = "pcprec", param = c(1,0.01)))


formula <- wasting~-1+f(state,model = "besag", graph = NGR.adj, scale.model=T, diagonal = d.add, hyper = "shyper")+
  urban + prim + sec + high + poorer+ middle + richer + richest + islam  + bord_2_3 + bord_4 + female +
  under_3 + above_3 + under_35 + over_36 + normal + overweight + obesity 

m3 <- inla(formula,family ="binomial",data = malnutrion,Ntrials = 1,)

summary(m3)
library(tmap)
NG_map <- st_read("C:/Users/HP/Documents/shps/sdr_subnational_boundaries2.shp")
View(NG_map)
sp3 <- m3$summary.random$state
View(sp3)

sp3$REGCODE <- sp3$ID*10

NG_mapsp3 <- inner_join(NG_map,sp3)

NG_mapsp3$emean <- exp(NG_mapsp3$mean)


#NG_map for wasting
tm_shape(NG_mapsp3)+tm_polygons("emean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","top"),text.size = 0.6)


d3 <- data.frame(malnutrion,m3$summary.fitted.values)
View(d3)

e3 <- d3 %>% group_by(state,year)

e3 <- summarise(e3, pmean=mean(mean))
View(e3)                
e3$REGCODE <- e3$state*10

wasting_map <- inner_join(NG_map,e3)

tm_shape(wasting_map)+tm_layout(title = "wasting-Nigeria")+tm_polygons("pmean",id="state",style="quantile", palette= "YlGn",title="Odds ratio")+
  tm_compass(type = "8star",position = c("left","bottom"),text.size = 0.6)+tm_facets(by = "year", nrow =1, free.coords = FALSE)

