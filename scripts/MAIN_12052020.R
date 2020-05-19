
setwd('C:\\Users\\Maciek\\Desktop\\spatial_econometrics_project')

source('PACKAGES_12052020.R')

x <- getURL("https://raw.githubusercontent.com/superHubert/pov_spatial_model/master/data/GUS_all_merged3.csv?token=ALOXGCMMHL2RIER3NKPSOBC6ZPY5A")
base = read.csv(text=x, sep=",")
data = base



#choosing reasonable years investigating missings
missings = as.data.frame(matrix(NA, ncol=max(data$rok)-min(data$rok)+1, nrow=ncol(data)))
j=1
for(i in seq(min(data$rok),max(data$rok),1)){
  col  = sapply(data[data$rok==i,], function(x) sum(is.na(x)))
  missings[,j] = col
  j=j+1
}
colnames(missings) = seq(min(data$rok),max(data$rok),1)
missings[c(1:10),c(10:15)]

#visualisation
par(mfrow=c(5,5))
for(i in c(1:25)){
  barplot(missings[,i], main=colnames(missings)[i], 
          col='coral', border='coral')
}
par(mfrow=c(1,1))

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



#function prepares data for analyses
#for now, one year can be used
#here we take all the variables
source('PREPARE_DATA_12052020.R')
data_2018 = prepare_data(data,2018,colnames(data))



#looks for moran scatterplot with most interesting relationship
slopes=c()
for(i in c(5:ncol(data_2018))){
  x<-data_2018[,i]
  zx<-as.data.frame(scale(x)) #standardized variable
  wzx<-lag.listw(cont.listw, zx$V1) #spatial lag
  morlm<-lm(wzx~zx$V1) #linear regression            
  
  slope<-morlm$coefficients[2] #coefficient from regression
  
  slopes = append(slopes,slope)
}
slopes=cbind(slopes,colnames(data_2018)[c(5:ncol(data_2018))])
slopes = as.data.frame(slopes)
slopes$slopes = as.numeric(slopes$slopes)

#top 10 coefficients in linear regression: variable ~ its spatial lag
slopes %>% arrange(desc(abs(slopes))) %>% top_n(10)

morans = as.data.frame(matrix(NA,nrow=(ncol(data_2018)-4),ncol=2))
for(i in c(5:ncol(data_2018))){
  result01<-moran.test(data_2018[,i],cont.listw)
  morans[i-4,]=c(colnames(data_2018)[i],result01$estimate[1])
}
morans %>% arrange(desc(V2)) %>% top_n(10)

source('MAP_12052020.R')
map(data_2018,2018,'Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl')



#global moran test
result01<-moran.test(data_2018$Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl,
                     cont.listw)
result01

#moran scatterplot
x<-data_2018$Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl
zx<-as.data.frame(scale(x)) #standardized variable
wzx<-lag.listw(cont.listw, zx$V1) #spatial lag
morlm<-lm(wzx~zx$V1) #linear regression
slope<-morlm$coefficients[2] 
intercept<-morlm$coefficients[1]

par(mfrow=c(1,1))
plot(zx$V1, wzx, xlab="zx",ylab="spatial lag of zx", pch="*")  
abline(intercept, slope)  # regression line
abline(h=0, lty=2) # supplementary horizontal line y=0
abline(v=0, lty=2) # supplementary vertical line x=0
title('Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl')

#mapping the quarters
cond1<-ifelse(zx>=0 & wzx>=0, 1,0) # I quarter
cond2<-ifelse(zx>=0 & wzx<0, 2,0) # II quarter
cond3<-ifelse(zx<0 & wzx<0, 3,0) # III quarter
cond4<-ifelse(zx<0 & wzx>=0, 4,0) # IV quarter
cond.all<-cond1+cond2+cond3+cond4 # all quarters in one
cond<-as.data.frame(cond.all)

brks<-c(1,2,3,4)
cols<-c("grey25", "grey60", "grey85", "grey45")
plot(pov, col=cols[findInterval(cond$V1, brks)])
legend("bottomleft", legend=c("I Q - HH - high surrounded by high", 
                              "II Q - LH - low surrounded by high", 
                              "III Q - LL - low surrounded by low", 
                              "IV Q - HL - high surrounded by low"), 
       fill=cols, bty="n", cex=0.80)
title(main="Mapping of Moranscatterplot results")



#local moran I statistic
locM<-localmoran(spNamedVec("Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl", 
                            data_2018), cont.listw)
oid1<-order(data_2018$Kod)
locMorMat<-printCoefmat(data.frame(locM[oid1,], 
                                   row.names=data_2018$Kod[oid1]), check.names  = FALSE)

# map of the significance of Moran's local statistics
names(locMorMat)[5]<-"Prob"
brks<-c(min(locMorMat[,5]), 0.05000, 0.95000, max(locMorMat[,5]))
cols<-c("grey30", "grey90", "grey60")

plot(pov, col=cols[findInterval(locMorMat[,5], brks)])
legend("bottomleft", legend=c("surrounded by relatively high values, locM>0", "insignificant", "surrounded by relatively low values, locM<0"), fill=cols, bty="n", cex=0.75)
title(main=" Local Moran statistics ", cex=0.7)
plot(voi, add=TRUE, lwd=2)



#moran in time
moran<-matrix(0, ncol=4, nrow=1)
colnames(moran)<-2015:2018
rownames(moran)<-"Moran's I"

source('PREPARE_DATA_12052020.R')
for(i in 2015:2018){
  temp = prepare_data(data,i,colnames(data))
  variable = temp$Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl
  result01<-moran.test(variable, cont.listw)
  moran[1,i-2014]<-result01$estimate[1]
}

plot(moran[1,])

plot(moran[1,], type="l", axes=FALSE, ylab="", xlab="", 
     xlim=c(0.8,4.2), ylim=c(0.39, 0.44))
axis(1, at=1:4, labels=2015:2018)
axis(2)
abline(h=seq(0.39,0.44,0.005), lty=3, lwd=3, col="grey80")
abline(h=seq(0.39,0.44,0.001), lty=3, lwd=1, col="grey80")
points(moran[1,], bg="red", pch=21, cex=1.5)
text(1:4, moran[1,]+0.0025, labels=round(moran[1,],4))
title(main="Moran's I over years")



colnames(data_2018)

source("FIND_BEST_PREDICTORS_12052020.R")
vars = find_best_predictors(data_2018[,c(5:41,56:82)],
                            "Mediana_cen_za_1_m2_lokali_mieszkalnych_sprzedanych_w_ramach_transakcji_rynkowych_rynek_wtorny_ogolem_zl")
vars

