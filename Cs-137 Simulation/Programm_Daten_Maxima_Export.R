'
#Maxima Export optimieren 
#Ort in kart. Koordinaten (x,y beschreiben Abstände in km von (0°N, 0°O) )
x<-c()
y<-c()
Erdradius_km<-6371 #(jetzt in Km)
for (l in 1:dim(Daten_Maxima)[1]){
  #1. Verschiebung um v (S-N-Richtung) (immergleich, deshalb zuerst)
  x_Nord<-Daten_Maxima[l,3]/360*2*pi*Erdradius_km  #eigentlich falsch von konstantem NS-Winkel auszugehen
  
  #2. Verschiebung um u (W-E_Richtung) (abhängig von Ort in N-S Richtung)
  Radius_Breitengrad<-Erdradius_km*cos(Daten_Maxima[l,3]*pi/180)
  y_Ost<-Daten_Maxima[l,4]/360*2*pi*Radius_Breitengrad
  
  x<-c(x,x_Nord)
  y<-c(y,y_Ost)
  
}
Daten_Maxima_Export<-data.frame(x,y,Daten_Maxima$Aktivität,Daten_Maxima$Zeitdauer_Transport)
View(Daten_Maxima_Export)
#Daten exportieren
for (k in 1:(round(dim(Daten_Maxima_Export)[1]/1000))){
  write.csv(Daten_Maxima_Export[1:1000,],c(paste0( "Daten_Maxima/Daten für Maxima_Export",as.character((k-1)*1000+1),"-",as.character(k*1000),".csv")), row.names = FALSE)
}






# Sachen zum Testen (Koordinatensystem)
x<-c(-180:180)
y<-c(-180:180)
x1<-c()
y1<-c()
Erdradius_km<-6371 #(jetzt in Km)
for (l in 1:360){
  #1. Verschiebung um v (S-N-Richtung) (immergleich, deshalb zuerst)
  x_Nord<-x[l+180]/360*2*pi*Erdradius_km  #eigentlich falsch von konstantem NS-Winkel auszugehen
  
  #2. Verschiebung um u (W-E_Richtung) (abhängig von Ort in N-S Richtung)
  Radius_Breitengrad<-Erdradius_km*cos(x[l+180]*pi/180)
  y_Ost<-y[l-l+180+40]/360*2*pi*Radius_Breitengrad
  
  x1<-c(x1,x_Nord)
  y1<-c(y1,y_Ost)
  
}
Daten_Maxima_Export<-data.frame(x1,y1)
View(Daten_Maxima_Export)
plot(y1,x1,xlim=c(0,100), ylim=c(0,1000))


'
# mit D in Grad^2/3h umgerechnet (Äquator)


#Maxima Export 
#Ort(x,y beschreiben Abstände in km von (0°N, 0°O) )
Latitude_Maxima<-c()
Longitude_Maxima<-c()
Erdradius_km<-6371 #(jetzt in Km)
# D in  Gradzahl umrechnen
D_normal<-1E5*3*3600/1E6 #in km^2 /3h
D_in_Äquatorgrad_hoch2<-D_normal*360^2/(2*pi*Erdradius_km)^2
for (l in 1:dim(Daten_Maxima)[1]){
  #1. Verschiebung um v (S-N-Richtung) (immergleich, deshalb zuerst)
  x_Nord<-Daten_Maxima[l,4]  #eigentlich falsch von konstantem NS-Winkel auszugehen
  
  #2. Verschiebung um u (W-E_Richtung) (abhängig von Ort in N-S Richtung)
  y_Ost<-Daten_Maxima[l,5]
  
  Latitude_Maxima<-c(Latitude_Maxima,x_Nord)
  Longitude_Maxima<-c(Longitude_Maxima,y_Ost)
}
Daten_Maxima_Export<-data.frame(Longitude_Maxima,Latitude_Maxima,Daten_Maxima$Aktivität,Daten_Maxima$Zeitdauer_Transport, Daten_Maxima$Zeit_Auftreffen)
View(Daten_Maxima_Export)
plot(Longitude_Maxima,Latitude_Maxima,xlim=c(ORT_Freisetzung[2]-20,ORT_Freisetzung[2]+20), ylim=c(ORT_Freisetzung[1]-20,ORT_Freisetzung[1]+20))
#Daten exportieren
write.csv(Daten_Maxima_Export,c(paste0( "Daten_Maxima/Daten für Maxima_Export_Gesamt.csv")), row.names = FALSE)
