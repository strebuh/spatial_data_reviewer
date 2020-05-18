
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
X_2018 = prepare_data(data,2018,variables)
X_2017 = prepare_data(data,2017,variables)
X_2016 = prepare_data(data,2016,variables)

X = rbind(X_2018,X_2017)
X = rbind(X,X_2016)

colnames(X) = c("Kod",
                "year",
                "unemployment",                                                                                                                        
                "mean_flat_area",                                                                                      
                "number_of_flats_in_district",                                                                                                                                       
                "people_in_working_age",                                                                                                                      
                "crimes_rate",
                "social_help_cause_unemployment",                                                                                               
                "tourists_per10k",        
                "births_deaths_rate",                                                                                                                           
                "wages",
                "flats_prices"
)

crds<-coordinates(pov)
#cont.nb
#cont.listw

pov.knn<-knearneigh(crds, k=379)
pov.nb<-knn2nb(pov.knn)
dist<-nbdists(pov.nb, crds)
dist1<-lapply(dist, function(x) 1/x)
invdist.listw<-nb2listw(pov.nb, glist=dist1)

data_dist = read.csv('data_nts4_2019.csv',sep=';',dec=',')
data_dist = data_dist[,match(c('code_GUS','dist','year'),colnames(data_dist))]
data_dist = data_dist[data_dist$year==2017,]
data_dist = data_dist[,-3]

X = merge(X, data_dist, by.x="Kod", by.y="code_GUS", all.x=T, sort=F)

#time-space lags
data_sl = X
data_sl1 = data.frame(matrix(c(1:380),nrow=380))
colnames(data_sl1) = 'lp'

x2018<-na.omit(data_sl[data_sl$year==2018,
                       match(c("Kod","flats_prices"),
                             colnames(data_sl),data_sl)])
x2017<-na.omit(data_sl[data_sl$year==2017,
                       match(c("Kod","flats_prices"),
                             colnames(data_sl),data_sl)])
x2016<-na.omit(data_sl[data_sl$year==2016,
                       match(c("Kod","flats_prices"),
                             colnames(data_sl),data_sl)])

colnames(x2018)=c("kod",'x2018')
colnames(x2017)=c("kod",'x2017')
colnames(x2016)=c("kod",'x2016')

x2018$x2018 = as.numeric(x2018$x2018)
x2017$x2017 = as.numeric(x2017$x2017)
x2016$x2016 = as.numeric(x2016$x2016)

data_sl1 = merge(x2018, x2017, by.x="kod", by.y="kod", all.x=T, sort=F)
data_sl1 = merge(data_sl1, x2016, by.x="kod", by.y="kod", all.x=T, sort=F)
data_sl1 = merge(data_sl1, data_dist, by.x="kod", by.y="code_GUS", all.x=T, sort=F)

data_sl1 = data_sl1 %>% arrange(kod)
data_sl = data_sl %>% arrange(Kod)

data_sl1$cont2017<-lag.listw(cont.listw, na.omit(data_sl[data_sl$year==2017,
                                                         match("flats_prices",
                                                               colnames(data_sl),data_sl)]))
data_sl1$cont2016<-lag.listw(cont.listw, na.omit(data_sl[data_sl$year==2016,
                                                         match("flats_prices",
                                                               colnames(data_sl),data_sl)]))
data_sl1$ind2017<-lag.listw(invdist.listw, na.omit(data_sl[data_sl$year==2017,
                                                           match("flats_prices",
                                                                 colnames(data_sl),data_sl)]))
data_sl1$ind2016<-lag.listw(invdist.listw, na.omit(data_sl[data_sl$year==2016,
                                                           match("flats_prices",
                                                                 colnames(data_sl),data_sl)]))



#GWR model
formula = x2018 ~ x2017 + x2016 + cont2017 + cont2016 + ind2017 + ind2016 + dist

model.ggwr<-ggwr(formula, data=data_sl1, coords=crds, 
                 family=poisson(), longlat=TRUE, 
                 bandwidth=ggwr.sel(formula, 
                                    data=data_sl1, 
                                    coords=crds, 
                                    family=poisson(), 
                                    longlat=TRUE))
model.ggwr

par(mfrow=c(2,2))
choropleth(pov, model.ggwr$SDF$x2017)
choropleth(pov, model.ggwr$SDF$cont2017)
choropleth(pov, model.ggwr$SDF$ind2017)
choropleth(pov, model.ggwr$SDF$dist)
par(mfrow=c(1,1))

#clustering
#best parameter k
fviz_nbclust(as.data.frame(model.ggwr$SDF[,2:9]), FUNcluster=kmeans)

clusters1<-kmeans(as.data.frame(model.ggwr$SDF[,3:5]), 2) # 2 clusters
choropleth(pov, clusters1$cluster) # the second argument is a clustering vector
title(main="2 clusters, results from kmeans()")

clusters2<-eclust(as.data.frame(model.ggwr$SDF[,2:5]), "kmeans", k=2) 
choropleth(pov, clusters2$cluster)
title(main="2 clusters, result from eclust()")
text(clusters2$centers[,5:6], labels=1:2, cex=2, col="green")

fviz_silhouette(clusters2)
fviz_cluster(clusters2, geom="point", ellipse.type="norm")

clusters2$silinfo
clusters2$size

#adding dummy variables describing clusters
data_sl1$clust1<-rep(0, times=dim(data_sl1)[1])
data_sl1$clust1[clusters2$cluster==1]<-1
data_sl1$clust2<-rep(0, times=dim(data_sl1)[1])
data_sl1$clust2[clusters2$cluster==2]<-1

formula1 = x2018 ~ x2017 + x2016 + cont2017 + cont2016 + ind2017 + ind2016 + dist + clust1

# spatial error model
model.sem<-errorsarlm(formula1, data=data_sl1, cont.listw)
summary(model.sem)

model.ols<-lm(formula1, data=data_sl1)
summary(model.ols)

