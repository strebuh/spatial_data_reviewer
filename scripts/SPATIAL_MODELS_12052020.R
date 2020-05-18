
setwd('C:\\Users\\Maciek\\Desktop\\spatial_econometrics_project')

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
X = X[,c(3:ncol(X))]

colnames(X) = c("unemployment",                                                                                                                        
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



source('HISTOGRAM.R')
gs=list()
for(i in c(1:ncol(X))){
  plot = nice_histogram(X,i)
  gs = list.append(gs,plot)
}
grid.arrange(grobs = gs, nrow=round(sqrt(ncol(X))),0)


#OLS model
OLS_1<-lm(flats_prices ~., 
        data=X)

#MANSKI MODEL
GNS_1<-sacsarlm(flats_prices ~., 
                data=X, 
                listw=cont.listw, 
                type="sacmixed") #activates spatial lag  
summary(GNS_1)

#SAC/SARAR MODEL
SAC_1<-sacsarlm(flats_prices ~., 
                data=X, 
                listw=cont.listw)
summary(SAC_1)

#SEM & SDM - spatial error model
#with spatial lags of X (SDEM)
SDEM_1 <- errorsarlm(flats_prices ~., 
                     data=X, 
                     listw=cont.listw, 
                     etype="emixed") 
summary(SDEM_1)

# no spatial lags of X (SEM)
SEM_1 <- errorsarlm(flats_prices ~., 
                    data=X, 
                    listw=cont.listw)
summary(SEM_1)

#SAR / SDM - spatial lag model
#with spatial lags of X (SDM)
SDM_1 <- lagsarlm(flats_prices ~., 
                  data=X, 
                  listw=cont.listw, 
                  type="mixed") 
summary(SDM_1)

#no spatial lags of X (SAR)
SAR_1 <- lagsarlm(flats_prices ~., 
                  data=X, 
                  listw=cont.listw) 
summary(SAR_1)

#model augmented with the spatially lagged RHS variables
SLX_1 <- lmSLX(flats_prices ~., 
               data=X, 
               listw=cont.listw)
summary(SLX_1)

#COMPARING DIFFERENT MODELS #####################################################

#comparison of models with AIC
aic = AIC(OLS_1, GNS_1, SDM_1, SDEM_1, SAC_1, SAR_1, SEM_1, SLX_1)
aic$model = c("OLS", "GNS", "SDM", "SDEM", "SAC", "SAR", "SEM", "SLX")
aic = aic %>% arrange(AIC)
aic$model <- factor(aic$model,levels=aic$model)

ggplot(aic) +
  aes(x = model, y = AIC, 
      fill=factor(ifelse(model=="GNS","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='AIC for different models',x="models", y="AIC") +
  coord_cartesian(ylim=c(min(aic$AIC)-5,max(aic$AIC)+5)) +
  geom_text(aes(label = round(AIC,2)),position = position_stack(1)) +
  theme(legend.position='None')

#comparison of models with BIC
bic = BIC(OLS_1, GNS_1, SDM_1, SDEM_1, SAC_1, SAR_1, SEM_1, SLX_1)
bic$model = c("OLS", "GNS", "SDM", "SDEM", "SAC", "SAR", "SEM", "SLX")
bic = bic %>% arrange(BIC)
bic$model <- factor(bic$model,levels=bic$model)

ggplot(bic) +
  aes(x = model, y = BIC, 
      fill=factor(ifelse(model=="SAR","Highlighted","Normal"))) +
  geom_col() + 
  labs(title='BIC for different models',x="models", y="BIC") +
  coord_cartesian(ylim=c(min(bic$BIC)-5,max(bic$BIC)+5)) +
  geom_text(aes(label = round(BIC,2)),position = position_stack(1)) +
  theme(legend.position='None')

#Moran test on residuals - are the residuals random in space?
moran.test(OLS_1$residuals, cont.listw)
moran.test(GNS_1$residuals, cont.listw)
moran.test(SDM_1$residuals, cont.listw)
moran.test(SDEM_1$residuals, cont.listw)
moran.test(SAC_1$residuals, cont.listw)
moran.test(SAR_1$residuals, cont.listw)
moran.test(SEM_1$residuals, cont.listw)
moran.test(SLX_1$residuals, cont.listw)

res<-OLS_1$residuals
brks<-c(min(res), mean(res)-sd(res), mean(res), mean(res)+sd(res), max(res))
cols<-c("steelblue4","lightskyblue","thistle1","plum3")

res<-GNS_1$residuals
plot(X, col=cols[findInterval(res,brks)])
plot(X, add=TRUE, lwd=2)
title(main="Residuals from spatial model")
#legend("bottomleft", legend=c("<mean-sd", "(mean-sd, mean)", "(mean, mean+sd)", ">mean+sd"), leglabs(brks1), fill=cols, bty="n")
