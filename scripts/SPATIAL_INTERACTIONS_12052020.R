
setwd('C:\\Users\\Maciek\\Desktop\\spatial_econometrics_project')

Sys.setenv(LANG = "en")

source('PACKAGES_12052020.R')

x <- getURL("https://raw.githubusercontent.com/superHubert/pov_spatial_model/master/data/GUS_all_merged3.csv?token=ALOXGCMMHL2RIER3NKPSOBC6ZPY5A")
base = read.csv(text=x, sep=",")
data = base

variables = c(
  "Kod",
  "rok",
  "BEZROBOCIE_ogolem_w_proc_bezrobotnych_ogolem_proc",                                                                                                                        
  "Budownictwo_mieszkaniowe_wskazniki_przecietna_powierzchnia_uZytkowa_1_mieszkania_m2",                                                                                      
  "Budynki_mieszkalne_w_gminie_ogolem",                                                                                                                                       
  "ludnosc_grupy_ekonomiczne_w_wieku_produkcyjnym_proc",                                                                                                                      
  "Osoby_w_zamachach_samobojczych_liczba_prob_zachowan_samobojczych_na_100_tys_ludnosci_ogolem_osoba",                                                                        
  "Przestepstwa_stwierdzone_przez_Policje_w_zakonczonych_postepowaniach_przygotowawczych_wskaznik_wykrywalnosci_sprawcow_przestepstw_stwierdzonych_przez_Policje_ogolem_proc",
  "Rodziny_ktorym_na_podstawie_decyzji_przyznano_pomoc_wg_przyczyn_bezrobocie",                                                                                               
  "TURYSTYCZNE_OBIEKTY_NOCLEGOWE_I_ICH_WYKORZYSTANIE_CTAB_udzielone_noclegi_turystom_zagranicznym_w_turystycznych_obiektach_noclegowych_na_10_tys_mieszkancow_ogolem",        
  "URODZENIA_ZGONY_przyrost_naturalny_na_ludnosci",                                                                                                                           
  "WYNAGRODZENIA_CTAB_przecietne_miesieczne_wynagrodzenia_brutto_w_relacji_do_sredniej_krajowej_Polska_100_proc",   
  "Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl"
)

# reading maps with rgdal::
pl<-readOGR("data", "Panstwo")
voi<-readOGR("data", "wojewodztwa")
pov<-readOGR("data", "powiaty")

# changing projections
pl<-spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
voi<-spTransform(voi, CRS("+proj=longlat +datum=NAD83"))
pov<-spTransform(pov, CRS("+proj=longlat +datum=NAD83"))

cont.nb<-poly2nb(as(pov, "SpatialPolygons"))
cont.listw<-nb2listw(cont.nb, style="W")
cont.listw 

source("PREPARE_DATA_12052020.R")
X = prepare_data(data,2018,variables)
X = X[,c(1,3:ncol(X))]

colnames(X) = c("Kod",
                "unemployment",                                                                                                                        
                "mean_flat_area",                                                                                      
                "number_of_flats_in_district",                                                                                                                                       
                "people_in_working_age",                                                                                                                      
                "suicides_tried_per100k",                                                                        
                "crimes_rate",
                "social_help_cause_unemployment",                                                                                               
                "tourists_per10k",        
                "births_deaths_rate",                                                                                                                           
                "wages",
                "flats_prices"
)



#merging our data with distances from classes dataset
data_dist = read.csv('data_nts4_2019.csv',sep=';',dec=',')
data_dist = data_dist[,match(c('code_GUS','dist','year'),colnames(data_dist))]
data_dist = data_dist[data_dist$year==2017,]
data_dist = data_dist[,-3]

data_int = merge(X, data_dist, by.x="Kod", by.y="code_GUS", all.x=T, sort=F)



#control plot - were distances merged properly?
var = 'dist'
variable <- data_int[,match(var,colnames(data_int))]

intervals<-9
colors<-brewer.pal(intervals, "BuPu")
classes<-classIntervals(variable, intervals, style="fixed", 
                        fixedBreaks=c(min(variable),
                                      round(as.numeric(quantileCuts(variable,10)),0),
                                      max(variable)))
color.table<-findColours(classes, colors) 

plot(pov, col=color.table)
plot(voi, lwd=2, add=TRUE)
legend("bottomleft", legend=names(attr(color.table, "table")),
       fill=attr(color.table, "palette"), cex=0.8, bty="n")

colnames(data_int)
data_int_model = data_int[,-c(1)]



#first spacial interactions models
data_int_model$variable = data_int_model$flats_prices
data_int_model$variable.m = data_int_model$variable/mean(data_int_model$variable)

#more sophisticated spatial interactions models
colnames(data_int_model)
data_int_model.m = data_int_model
for(i in c(1:(ncol(data_int_model)-2))){
  data_int_model.m[,i] = data_int_model.m[,i]/mean(data_int_model.m[,i])
}

OLS.multi<-glm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                 poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                 poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                 poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                 poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                 poly(wages,4),
               data = data_int_model.m)

GNS.multi<-sacsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                            poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                            poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                            poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                            poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                            poly(wages,4),type="sacmixed",
                          data = data_int_model.m,
                          cont.listw, tol.solve=2e-40)

OLS.power<-lm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                log(tourists_per10k+1)+log(births_deaths_rate+10)+
                log(wages+1),
              data = data_int_model.m)

GNS.power<-sacsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                            log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                            log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                            log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                            log(tourists_per10k+1)+log(births_deaths_rate+10)+
                            log(wages+1), type="sacmixed",
                          data=data_int_model.m, 
                          cont.listw)

OLS.exp<-glm(log(variable.m+1) ~ dist+unemployment+
               mean_flat_area+number_of_flats_in_district+
               people_in_working_age+suicides_tried_per100k+
               crimes_rate+social_help_cause_unemployment+
               tourists_per10k+births_deaths_rate+
               wages,
             data = data_int_model.m)


GNS.exp<-sacsarlm(log(variable.m+1) ~ dist+unemployment+
                          mean_flat_area+number_of_flats_in_district+
                          people_in_working_age+suicides_tried_per100k+
                          crimes_rate+social_help_cause_unemployment+
                          tourists_per10k+births_deaths_rate+
                          wages, type="sacmixed",
                        data = data_int_model.m, cont.listw)

#comparison of the models
screenreg(list(OLS.multi, gns.multi, OLS.power, gns.power, OLS.exp, gns.exp))

SRMSE<-function(model, dataset) { sqrt(sum((model$fitted.values-dataset$variable.m)^2)/ dim(dataset)[1]) / (mean(dataset$variable.m))}

models<-c("OLS.multi", "GNS.multi", "OLS.power", "GNS.power", "OLS.exp", "GNS.exp")
SRMSE.all<-matrix(0)
for(i in 1:6){
  SRMSE.all[i]<-SRMSE(get(models[i]), data_int_model)}
SRMSE.all

SRMSE.df=data.frame(models=models,SRMSE=SRMSE.all)
SRMSE.df$models = factor(SRMSE.df$models,levels=SRMSE.df$models)

ggplot(SRMSE.df) +
  aes(x = models, y = SRMSE,
      fill=factor(ifelse(models=="GNS.multi" | models=="OLS.multi","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='SRMSE for different distances',x="models", y="SRMSE") +
  coord_cartesian(ylim=c(min(SRMSE.df$SRMSE)-0.05,max(SRMSE.df$SRMSE)+0.05)) +
  geom_text(aes(label = round(SRMSE,4)),position = position_stack(1)) +
  theme(legend.position='None')

#fitted values visualisation
par(mfrow=c(3,2))

plot(data_int_model.m$dist, data_int_model.m$variable.m)
points(data_int_model.m$dist, OLS.multi$fitted.values, col="red")
abline(h=1, lty=3)
title(main="a-spatial, multinomial, fitted values")

plot(data_int_model.m$dist, data_int_model.m$variable.m)
points(data_int_model.m$dist, spatial.multi$fitted.values, col="red")
abline(h=1, lty=3)
title(main="spatial, multinomial, fitted values")

plot(log(data_int_model.m$dist+1), log(data_int_model.m$variable.m+1))
points(log(data_int_model.m$dist+1), OLS.power$fitted.values, col="red")
abline(h=0.7, lty=3)
title(main="a-spatial, power, fitted values")

plot(log(data_int_model.m$dist+1), log(data_int_model.m$variable.m+1))
points(log(data_int_model.m$dist+1), spatial.power$fitted.values, col="red")
abline(h=0.7, lty=3)
title(main="spatial, power, fitted values")

plot(data_int_model.m$dist, log(data_int_model.m$variable.m+1))
points(data_int_model.m$dist, OLS.exp$fitted.values, col="red")
abline(h=0.7, lty=3)
title(main="a-spatial, exponential, fitted values")

plot(data_int_model.m$dist, log(data_int_model.m$variable.m+1))
points(data_int_model.m$dist, spatial.exp$fitted.values, col="red")
abline(h=0.7, lty=3)
title(main="spatial, exponential, fitted values")

par(mfrow=c(1,1))



OLS.multi<-glm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                 poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                 poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                 poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                 poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                 poly(wages,4),
             data = data_int_model.m)


GNS.multi<-sacsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                      poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                      poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                      poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                      poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                      poly(wages,4),type="sacmixed",
                  data = data_int_model.m, cont.listw)

SAC.multi<-sacsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                      poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                      poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                      poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                      poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                      poly(wages,4),
                  data = data_int_model.m, cont.listw)

SDEM.multi<-errorsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                         poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                         poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                         poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                         poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                         poly(wages,4), etype="emixed",
                     data = data_int_model.m, cont.listw)

SEM.multi<-errorsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                        poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                        poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                        poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                        poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                        poly(wages,4),
                    data = data_int_model.m, cont.listw)

SDM.multi<-lagsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                      poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                      poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                      poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                      poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                      poly(wages,4), type="mixed",
                  data = data_int_model.m, cont.listw)

SAR.multi<-lagsarlm(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                      poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                      poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                      poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                      poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                      poly(wages,4), 
                  data = data_int_model.m, cont.listw)

SLX.multi<-lmSLX(variable.m ~ poly(dist,4)+poly(unemployment,4)+
                   poly(mean_flat_area,4)+poly(number_of_flats_in_district,4)+
                   poly(people_in_working_age,4)+poly(suicides_tried_per100k,4)+
                   poly(crimes_rate,4)+poly(social_help_cause_unemployment,4)+
                   poly(tourists_per10k,4)+poly(births_deaths_rate,4)+
                   poly(wages,4), 
               data = data_int_model.m, cont.listw)

OLS.power<-glm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                 log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                 log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                 log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                 log(tourists_per10k+1)+log(births_deaths_rate+10)+
                 log(wages+1),
             data = data_int_model.m)


GNS.power<-sacsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                      log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                      log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                      log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                      log(tourists_per10k+1)+log(births_deaths_rate+10)+
                      log(wages+1),type="sacmixed",
                  data = data_int_model.m, cont.listw)

SAC.power<-sacsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                      log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                      log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                      log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                      log(tourists_per10k+1)+log(births_deaths_rate+10)+
                      log(wages+1),
                  data = data_int_model.m, cont.listw)

SDEM.power<-errorsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                         log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                         log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                         log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                         log(tourists_per10k+1)+log(births_deaths_rate+10)+
                         log(wages+1), etype="emixed",
                     data = data_int_model.m, cont.listw)

SEM.power<-errorsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                        log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                        log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                        log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                        log(tourists_per10k+1)+log(births_deaths_rate+10)+
                        log(wages+1),
                    data = data_int_model.m, cont.listw)

SDM.power<-lagsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                      log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                      log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                      log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                      log(tourists_per10k+1)+log(births_deaths_rate+10)+
                      log(wages+1), type="mixed",
                  data = data_int_model.m, cont.listw)

SAR.power<-lagsarlm(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                      log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                      log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                      log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                      log(tourists_per10k+1)+log(births_deaths_rate+10)+
                      log(wages+1), 
                  data = data_int_model.m, cont.listw)

SLX.power<-lmSLX(log(variable.m+1) ~ log(dist+1)+log(unemployment+1)+
                   log(mean_flat_area+1)+log(number_of_flats_in_district+1)+
                   log(people_in_working_age+1)+log(suicides_tried_per100k+1)+
                   log(crimes_rate+1)+log(social_help_cause_unemployment+1)+
                   log(tourists_per10k+1)+log(births_deaths_rate+10)+
                   log(wages+1), 
               data = data_int_model.m, cont.listw)

OLS.exp<-glm(log(variable.m+1) ~ dist+unemployment+
               mean_flat_area+number_of_flats_in_district+
               people_in_working_age+suicides_tried_per100k+
               crimes_rate+social_help_cause_unemployment+
               tourists_per10k+births_deaths_rate+
               wages,
             data = data_int_model.m)


GNS.exp<-sacsarlm(log(variable.m+1) ~ dist+unemployment+
                    mean_flat_area+number_of_flats_in_district+
                    people_in_working_age+suicides_tried_per100k+
                    crimes_rate+social_help_cause_unemployment+
                    tourists_per10k+births_deaths_rate+
                    wages,type="sacmixed",
                  data = data_int_model.m, cont.listw)

SAC.exp<-sacsarlm(log(variable.m+1) ~ dist+unemployment+
                    mean_flat_area+number_of_flats_in_district+
                    people_in_working_age+suicides_tried_per100k+
                    crimes_rate+social_help_cause_unemployment+
                    tourists_per10k+births_deaths_rate+
                    wages,
                  data = data_int_model.m, cont.listw)

SDEM.exp<-errorsarlm(log(variable.m+1) ~ dist+unemployment+
                    mean_flat_area+number_of_flats_in_district+
                    people_in_working_age+suicides_tried_per100k+
                    crimes_rate+social_help_cause_unemployment+
                    tourists_per10k+births_deaths_rate+
                    wages, etype="emixed",
                  data = data_int_model.m, cont.listw)

SEM.exp<-errorsarlm(log(variable.m+1) ~ dist+unemployment+
                       mean_flat_area+number_of_flats_in_district+
                       people_in_working_age+suicides_tried_per100k+
                       crimes_rate+social_help_cause_unemployment+
                       tourists_per10k+births_deaths_rate+
                       wages,
                     data = data_int_model.m, cont.listw)

SDM.exp<-lagsarlm(log(variable.m+1) ~ dist+unemployment+
                      mean_flat_area+number_of_flats_in_district+
                      people_in_working_age+suicides_tried_per100k+
                      crimes_rate+social_help_cause_unemployment+
                      tourists_per10k+births_deaths_rate+
                      wages, type="mixed",
                    data = data_int_model.m, cont.listw)

SAR.exp<-lagsarlm(log(variable.m+1) ~ dist+unemployment+
                    mean_flat_area+number_of_flats_in_district+
                    people_in_working_age+suicides_tried_per100k+
                    crimes_rate+social_help_cause_unemployment+
                    tourists_per10k+births_deaths_rate+
                    wages, 
                  data = data_int_model.m, cont.listw)

SLX.exp<-lmSLX(log(variable.m+1) ~ dist+unemployment+
                    mean_flat_area+number_of_flats_in_district+
                    people_in_working_age+suicides_tried_per100k+
                    crimes_rate+social_help_cause_unemployment+
                    tourists_per10k+births_deaths_rate+
                    wages, 
                  data = data_int_model.m, cont.listw)

#comparison of the models
models<-c("OLS.multi", "GNS.multi", "SDM.multi", "SDEM.multi", "SAC.multi", 
          "SAR.multi", "SEM.multi", "SLX.multi")
SRMSE.all<-matrix(0)
for(i in 1:8){
  SRMSE.all[i]<-SRMSE(get(models[i]), data_int_model)}
SRMSE.all

SRMSE.df=data.frame(models=models,SRMSE=SRMSE.all)
SRMSE.df = SRMSE.df %>% arrange(SRMSE)
SRMSE.df$models = factor(SRMSE.df$models,levels=SRMSE.df$models)

ggplot(SRMSE.df) +
  aes(x = models, y = SRMSE,
      fill=factor(ifelse(models=="GNS.multi","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='SRMSE for different models (multi)',x="models", y="SRMSE") +
  coord_cartesian(ylim=c(min(SRMSE.df$SRMSE)-0.01,max(SRMSE.df$SRMSE)+0.01)) +
  geom_text(aes(label = round(SRMSE,4)),position = position_stack(1)) +
  theme(legend.position='None')

models<-c("OLS.power", "GNS.power", "SDM.power", "SDEM.power", "SAC.power", 
          "SAR.power", "SEM.power", "SLX.power")
SRMSE.all<-matrix(0)
for(i in 1:8){
  SRMSE.all[i]<-SRMSE(get(models[i]), data_int_model)}
SRMSE.all

SRMSE.df=data.frame(models=models,SRMSE=SRMSE.all)
SRMSE.df = SRMSE.df %>% arrange(SRMSE)
SRMSE.df$models = factor(SRMSE.df$models,levels=SRMSE.df$models)

ggplot(SRMSE.df) +
  aes(x = models, y = SRMSE,
      fill=factor(ifelse(models=="GNS.power","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='SRMSE for different models (power)',x="models", y="SRMSE") +
  coord_cartesian(ylim=c(min(SRMSE.df$SRMSE)-0.01,max(SRMSE.df$SRMSE)+0.01)) +
  geom_text(aes(label = round(SRMSE,4)),position = position_stack(1)) +
  theme(legend.position='None')

models<-c("OLS.exp", "GNS.exp", "SDM.exp", "SDEM.exp", "SAC.exp", 
          "SAR.exp", "SEM.exp", "SLX.exp")
SRMSE.all<-matrix(0)
for(i in 1:8){
  SRMSE.all[i]<-SRMSE(get(models[i]), data_int_model)}
SRMSE.all

SRMSE.df=data.frame(models=models,SRMSE=SRMSE.all)
SRMSE.df = SRMSE.df %>% arrange(SRMSE)
SRMSE.df$models = factor(SRMSE.df$models,levels=SRMSE.df$models)

ggplot(SRMSE.df) +
  aes(x = models, y = SRMSE,
      fill=factor(ifelse(models=="GNS.exp","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='SRMSE for different models (exp)',x="models", y="SRMSE") +
  coord_cartesian(ylim=c(min(SRMSE.df$SRMSE)-0.01,max(SRMSE.df$SRMSE)+0.01)) +
  geom_text(aes(label = round(SRMSE,4)),position = position_stack(1)) +
  theme(legend.position='None')
