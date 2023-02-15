'0-Vorbereitung der R-Umgebung'

#Modellierungsordner festlegen
setwd("D:/Studium landau/BA Arbeit/Umsetzung in R")

#Pakete installieren
install.packages("ncdf4") #Öffnen von ncdf4 Dateien
install.packages("lubridate") #Operieren und manipulieren von Zeit Daten
library(ncdf4)
library(lubridate)


'1-Einlesen der METEROLOGISCHEN DATEN'

# 1.WINDEINFLÜSSE
  #Laden der NCDF4 Datei
  nc_fname1 <- "Meterologie_Fukushima/Winddaten.nc"
  Winddaten <- nc_open(nc_fname1)
  var1 <- ncvar_get(Winddaten, "u", collapse_degen=FALSE)
  var2 <- ncvar_get(Winddaten, "v", collapse_degen=FALSE)
  
'  #Anzeigen lassen als DataFrame (übersichtlicher für Kontrolle)
    # Koordinaten extrahieren
    dim_lon <- ncvar_get(Winddaten, "longitude")
    dim_lat <- ncvar_get(Winddaten, "latitude")
    dim_level <- ncvar_get(Winddaten, "level")
    dim_time <- ncvar_get(Winddaten, "time")
    
    #Zeit-Konversion
    t_units<-ncatt_get(Winddaten,"time","units") #Abfrage der Zeiteinheit
    t_ustr<-strsplit(t_units$value," ") #Trennen der Zeiteinheit
    t_dstr<-strsplit(unlist(t_ustr)[3],"-") #Trennen der Jahre Monate und Tage
    date<-ymd(t_dstr)+dhours(dim_time) #Umrechnen in UTC
    
    #Koordinaten festlegen
    coords <- as.matrix(expand.grid(dim_lon, dim_lat, dim_level, date))
    
    #als DataFrame speichern
    nc_df <- data.frame(cbind(coords, var1, var2))
    names(nc_df) <- c("lon", "lat", "level", "time", "var1", "var2")
    head(na.omit(nc_df), 5)  # Display some non-NaN values for a visual check
    View(nc_df)
    
    
    #als .csv speichern
    csv_fname <- paste0("netcdf_Export_",Sys.Date(),".csv")
    write.table(nc_df, csv_fname, row.names=FALSE, sep=";")
'
  
# 2. REGENEINFLÜSSE

  #Laden der NCDF4 Datei 
  nc_fname2 <- "Meterologie_Fukushima/Regendaten.nc"
  Regendaten <- nc_open(nc_fname2)
  var3 <- ncvar_get(Regendaten, "tp", collapse_degen=FALSE)
  Zeitanpassung_Regen<- -6 #Anpassung um 6 Zeiteinheiten nötig, da Regendaten erst am 1.3 18:00 Uhr statt 0 Uhr (wie bei Wind) starten    

' #Anzeigen lassen als DataFrame (übersichtlicher für Kontrolle)
    # Koordinaten extrahieren
    dim_lon2 <- ncvar_get(Regendaten, "longitude")
    dim_lat2 <- ncvar_get(Regendaten, "latitude")
    dim_time2 <- ncvar_get(Regendaten, "time")
    
  #Zeit-Konversion
  t_units<-ncatt_get(Regendaten,"time","units") #Abfrage der Zeiteinheit
  t_ustr<-strsplit(t_units$value," ") #Trennen der Zeiteinheit
  t_dstr<-strsplit(unlist(t_ustr)[3],"-") #Trennen der Jahre Monate und Tage
  date2<-ymd(t_dstr)+dhours(dim_time2) #Umrechnen in UTC
  
  #Koordinaten festlegen
  coords2 <- as.matrix(expand.grid(dim_lon2, dim_lat2, date2))
  
  #als DataFrame speichern
  nc_df2 <- data.frame(cbind(coords2, var3))
  names(nc_df2) <- c("lon", "lat", "time", "var3")
  head(na.omit(nc_df2), 5)  # Display some non-NaN values for a visual check
  View(nc_df2)
  
  
  #als .csv speichern
  csv_fname2 <- paste0("netcdf_Export2_",Sys.Date(),".csv")
  write.table(nc_df2, csv_fname2, row.names=FALSE, sep=";")
'
# 3. VERTIKALE WINDEINFLÜSSE
  
#Laden der NCDF4 Datei 
nc_fname3 <- "Meterologie_Fukushima/vertikale Windkomponente.nc"
vertWindDaten <- nc_open(nc_fname3)
var4 <- ncvar_get(vertWindDaten, "w", collapse_degen=FALSE)

 ' #Anzeigen lassen als DataFrame (übersichtlicher für Kontrolle)

    # Koordinaten extrahieren
    dim_lon3 <- ncvar_get(vertWindDaten, "longitude")
    dim_lat3 <- ncvar_get(vertWindDaten, "latitude")
    dim_level3 <- ncvar_get(vertWindDaten, "level")
    dim_time3 <- ncvar_get(vertWindDaten, "time")
    
    #Zeit-Konversion
    t_units<-ncatt_get(vertWindDaten,"time","units") #Abfrage der Zeiteinheit
    t_ustr<-strsplit(t_units$value," ") #Trennen der Zeiteinheit
    t_dstr<-strsplit(unlist(t_ustr)[3],"-") #Trennen der Jahre Monate und Tage
    date3<-ymd(t_dstr)+dhours(dim_time3) #Umrechnen in UTC
    
    #Koordinaten festlegen
    coords3 <- as.matrix(expand.grid(dim_lon3, dim_lat3, dim_level3, date3))
    
    #als DataFrame speichern
    nc_df3 <- data.frame(cbind(coords, var4))
    names(nc_df3) <- c("lon", "lat", "level", "time", "var4")
    head(na.omit(nc_df3), 5)  # Display some non-NaN values for a visual check
    View(nc_df3)
    
    
    #als .csv speichern
    csv_fname <- paste0("netcdf_Export3_",Sys.Date(),".csv")
    write.table(nc_df3, csv_fname, row.names=FALSE, sep=";")
  '


'2-FESTLEGUNG DER ANFANGSPARAMETER zur Emission'

TESTABLAGERUNG_lat<-c()#testzeug %&%&
TESTABLAGERUNG_lon<-c()#testzeug %&%&
TESTABLAGERUNG_h<-c()#testzeug %&%&
TESTABLAGERUNG_t<-c()#testzeug %&%&2
TESTABLAGERUNG_bel<-c()#testzeug %&%&2

  #Zeitlicher Bereich
  Beginn_Betrachtung_Tag<-12 #am 12.3 Beginn (beachte:1.März als Startpunktnicht möglich, da erst ab 18:00 Werte vorliegen) 
  Beginn_Betrachtung_Index<-(Beginn_Betrachtung_Tag-1)*8+1
  Ende_Betrachtung_Tag<-18 # aktuelle Modellierung soll bis Ende des Tages vom 18.3 (7Tage) gehen
  Ende_Betrachtung_Index<-Ende_Betrachtung_Tag*8
  
  #Räumlicher Bereich (muss an ECMWF Daten angepasst werden)
  Koordinate_N<-60
  Koordinate_W<-120
  Koordinate_S<-30
  Koordinate_E<-180
  
  #Belastung
  Gesamtbelastung<-2E16 #Cäsium-137 Freisetzung in Fukushima

  ORT_Freisetzung<-c(37.316501, 141.025689) #1. Komponente: °Nord, 2.Komponente: °Ost
  #MAX_HÖHE_Freisetzung<- 1000 #ANGEPASST (wenn Höhe unklar)
  
  Höhenverteilung_Freisetzung<-c(Gesamtbelastung/53) #absolute Belastungswerte für versch Höhen eintragbar
  '# Idee
    Zeitverteilung_Freisetzung<- c(25E15,2E15,1E15,4E14,....) #in 3h-Schritten
    Höhenverteilung_Freisetzung<-c(0.5,0.3,0.2) #in 25m Schritten
  Muss definitiv noch umgesetzt werden, nur 20m Höhe und jeweils konstante Emission ist wenig sinnvoll!  
  '
  
'3. Definition Variablen bzw. Vektoren, die in while Schleifen und for-Schleifen gebraucht werden'
Zeit_Auftreffen<- c() #Zeit des Auftreffens am Boden
Zeitdauer_Transport<-c() #Zeitdauer des Transportes
Longitude<-c() #Koordinate Auftreffpunkt Ostrichtung
Latitude<-c() #Koordinate Auftreffpunkt Nordrichtung
Aktivität<-c() #Belastung am Boden
Belastung_unabgelagert<-0
Freisetzung_Kraftwerk<-c()

'jetzt: Beginn Modellierung für alle Zeitschritte'
for (Freisetzungszeitpunkt in Beginn_Betrachtung_Index:(Ende_Betrachtung_Index)){ #läuft 55 Zeiteinheiten durch in meinem Ursprungsbeispiel, direkt auf Eintrag in NCDF4 Datei zugreifbar
  
  for (Freisetzungshöhe in 1: 1){ #nur einfache Durchlaufung
    
'4-Initilaisierung Anfangsparameter für EINE ZEITEINHEIT IN EINER BESTIMMTEN HÖHE'

    #Menge an Radioaktivität
    Belastung<- Höhenverteilung_Freisetzung[Freisetzungshöhe] 
    
    #Höhe (in m) (aktuell anfangs konstant)
    z_ÜEOF<-20 #ANGEPASST(==)
    
    # Definition ursprünglicher Ortvektor zum Zeitpunkt der Emission (in Bogenmaß und Grad, macht nur Sinn eines der beiden dauerhaft zu nutzen)
    Trajektorie1<-c(ORT_Freisetzung*pi/180,z_ÜEOF) #(Angabe: °E (Bogenmaß),°N (Bogenmaß), Höhe über Erdoberfläche (in m) )
    Trajektorie1_Grad<-c(ORT_Freisetzung,z_ÜEOF) #Angabe jetzt in Grad
    
    
    #Dauer des Transport auf 0 setzen
    Dauer<-0
    
    #Ablagerung nasse Deposition schon mal definieren (sonst Fehlermeldung (wenn while schleife nicht durchlaufen wird.))
    Ablagerung_nasse_Deposition<-0
    
'5- Speicherort von Anfangsparameter bei Teilwolken festlegen' 
Freisetzungszeitpunkt_aussen<-c(Freisetzungszeitpunkt)
Entstehungszeitpunkt_aussen<-c(Freisetzungszeitpunkt)
x_aussen<-c(Trajektorie1_Grad[1])
y_aussen<-c(Trajektorie1_Grad[2])
Höhe_aussen<-c(Trajektorie1_Grad[3])
Belastung_aussen<-c(Belastung)
 
Laufvariable<-0
  'while aussen:'


while (Laufvariable<length(x_aussen)){
  Laufvariable<-Laufvariable+1
#Dauer des Transport festlegen
  Dauer<-0
  
  
'6 - Werte aktuelle Teilwolke initialisieren'
Trajektorie1_Grad<-c(x_aussen[Laufvariable],y_aussen[Laufvariable],Höhe_aussen[Laufvariable])
Trajektorie1<-c(Trajektorie1_Grad[1:2]*pi/180,Trajektorie1_Grad[3])
Belastung<-Belastung_aussen[Laufvariable]
Freisetzungszeitpunkt<-Freisetzungszeitpunkt_aussen[Laufvariable]
Entstehungszeitpunkt<-Entstehungszeitpunkt_aussen[Laufvariable]

    while (Trajektorie1[3]>=0 & Belastung>1E11 & (Dauer+Entstehungszeitpunkt)<Ende_Betrachtung_Index & Dauer < 4)
    {                                                                                               #zudem (Zeile drüber): Ende_Betrachtung_Index-1 zu Ende_Betrachtung_Index geändert! (ab: Zeitdauer0 abgeändert) 
      
'7-Zeitpunkt zählen'
      Dauer<-Dauer+1 #bedeutet auch zum Zeitpunkt 0 wurde noch nichts abgelagert, das heißt zu dem Zeitpunkt wurde jeweils abgelagert
      Zeitstep<-Entstehungszeitpunkt+Dauer
'8-Trajektorie runden'
      Koordinaten<-round(Trajektorie1_Grad[1:2]*2)/2#mal genau schauen was unterschied koordinaten und Trajektorie1_Grad eigentlich ist
      
'9-Trockene Deposition (und Neudefinition des Ortes nach Absinken)'
      v_TD<-1.5E-3*3*3600 #Cs-137 
      Trajektorie1[3]<-Trajektorie1[3]-v_TD
      Trajektorie1_Grad[3]<-Trajektorie1_Grad[3]-v_TD

'10-Nasse Deposition'
      Ablagerungsanteil<-5.5E-5*3*3600*(var3[1+2*(Koordinaten[2]-Koordinate_W),1+2*(Koordinate_N-Koordinaten[1]),Entstehungszeitpunkt+Dauer-Zeitanpassung_Regen]*3*1000)^0.8  #pro 3h  
        
      if(Ablagerungsanteil>1){Ablagerungsanteil<-1} # stellt sicher das nicht mehr als Gesamtaktivität der Wolke nass abgelagert wird 
      Ablagerung_nasse_Deposition<-Belastung*Ablagerungsanteil #Zwischenspeicherung nasse Deposition
      Belastung<-Belastung-Ablagerung_nasse_Deposition
      
      #Speichern der Daten von Nasser Deposition in Vektoren für Ausgabe
      if(Ablagerungsanteil>0){ #evt. andere Zahl als 0, sodass sich speichern lohnt ?!
        Zeit_Auftreffen<- c(Zeit_Auftreffen,Entstehungszeitpunkt+Dauer) #Zeit des Auftreffens am Boden
        Zeitdauer_Transport<-c(Zeitdauer_Transport,Dauer) #Zeitdauer des Transportes STAND
        Longitude<-c(Longitude,Trajektorie1_Grad[2]) #Koordinate Auftreffpunkt Ostrichtung,
        Latitude<-c(Latitude,Trajektorie1_Grad[1]) #Koordinate Auftreffpunkt Nordrichtung
        Aktivität<-c(Aktivität,Ablagerung_nasse_Deposition ) #Belastung am Boden
        Freisetzung_Kraftwerk<-c(Freisetzung_Kraftwerk, Freisetzungszeitpunkt) #wann entstand Belastung am Kraftwerk
      }
      
      '------------------------------------------ BIS HIER ZUFRIEDEN 1 ----------------------------------------------------------------------'
      
      

      if(Belastung>0 & Trajektorie1[3] > 0){
'11- WINDVERSCHIEBUNG' 
            #Labeleintrag mit Barometrischer Höhenformel bestimmen
            g <- 9.8
            p0 <- 1.01e5
            rho0<-1.29
            Druck<-p0*exp(-(rho0/p0)*g*Trajektorie1[3])/100
            #print(Druck)
            if (Druck>725){
              Druckgebiet<-Winddaten$dim$level$len-(round((1000-Druck)/25)*25/25) #gibt den Eintag des Druckes an
              #print(Winddaten$dim$level$vals[Druckgebiet])
            }else{
              Druckgebiet<-Winddaten$dim$level$len-10-(round((750-Druck)/50)*50/50) #gibt den Eintag des Druckes an
              #print(Winddaten$dim$level$vals[Druckgebiet])
            }
        #Windverschiebungsvektor
        Wind_u<-var1[1+2*(Koordinaten[2]-Koordinate_W),1+2*(Koordinate_N-Koordinaten[1]),Druckgebiet,Entstehungszeitpunkt+Dauer]*3*3600  #u:ostwärts 
        Wind_v<-var2[1+2*(Koordinaten[2]-Koordinate_W),1+2*(Koordinate_N-Koordinaten[1]),Druckgebiet,Entstehungszeitpunkt+Dauer]*3*3600  #V:nordwärts
        Wind_w<-var4[1+2*(Koordinaten[2]-Koordinate_W),1+2*(Koordinate_N-Koordinaten[1]),Druckgebiet,Entstehungszeitpunkt+Dauer]*3*3600/100 #w: Richtung Boden
        

        #Windverschiebung
        Erdradius<-6371000
        #1. Verschiebung um u (W-E_Richtung)
        Radius_Breitengrad<-Erdradius*cos(Trajektorie1[1])
        Verschiebung_Ost<-Wind_v/(2*pi*Radius_Breitengrad)*360 #vorher: Verschiebung_West<-Verschiebung_Ost<-Wind_v/(2*pi*Radius_Breitengrad)*360
        #2. Verschiebung um v (S-N-Richtung)
        Verschiebung_Nord<-Wind_u/(2*pi*Erdradius)*360  #eigentlich falsch von konstantem NS-Winkel auszugehen
        #3. Verschiebung um w
        Druck_neu<-Druck-Wind_w #Minus wegen Definition in ECMWF (positive values are downwind) 
        Trajektorie1_Grad[3]<- -(log(Druck_neu/(p0/100))*p0)/(rho0*g)
        Trajektorie1[3]<-Trajektorie1_Grad[3]
          
          if (Druck_neu<=675+1){#675,0....01 ist maximaler Wert, sodass beim Runden noch Wert von 700mb genutzt werden kann, ab 675 würde abgerundet werden in R (+1 wegen trockenen Deposition im nächsten Durchgang) / dies entspricht einer Höhe von ca. 3300m
          Belastung_unabgelagert<-Belastung                    
          Belastung<-0
          print(Trajektorie1_Grad) #zeigt direkt an (prüfen ob höher ansetzten! (also mehr Daten herunterladen))
          TESTABLAGERUNG_lat<-c(TESTABLAGERUNG_lat,Trajektorie1_Grad[1])#testzeug %&%&
          TESTABLAGERUNG_lon<-c(TESTABLAGERUNG_lon,Trajektorie1_Grad[2])#testzeug %&%&
          TESTABLAGERUNG_h<-c(TESTABLAGERUNG_h,Trajektorie1_Grad[3])#testzeug %&%&
          TESTABLAGERUNG_t<-c(TESTABLAGERUNG_t,Dauer+Entstehungszeitpunkt_aussen[Laufvariable])#testzeug %&%&2
          TESTABLAGERUNG_bel<-c(TESTABLAGERUNG_bel,Belastung)#testzeug %&%&2
          }
        #Neudefinition des Ortes nach Windverschiebung
        Trajektorie1_Grad<-c(Trajektorie1_Grad[1]+Verschiebung_Nord,Trajektorie1_Grad[2]+Verschiebung_Ost,Trajektorie1_Grad[3])
        Trajektorie1<-c(Trajektorie1_Grad[1:2]*pi/180,Trajektorie1_Grad[3])
        #~~~~~~~~~~~~~~~~~~~~~~~~~


        #        #Koordinaten auf halbe Gradzahlen runden (macht das an diesem Zeitpunkt Sinn? ich glaube ncht, dann müsste ja PRO ZEITSTEP eine Bewegung gemacht werden, die deutlich größer als 0.5 Grad ist! kann da Koordinaten vs. Trajektorie1_Grad vs. Trajektorie helfen ?)
        #       Koordinaten<-round(Trajektorie_Grad_1.2[1:2]*2)/2
        
        
        #        #Wind zu Koordinate finden:
        #        Zeitstep<-1
        #        Wind_u2<-Winddaten_Osten_10NOV_17NOV[2+4941*Zeitstep+(Koordinaten[2]-as.numeric(Winddaten_Osten_10NOV_17NOV[2,2]))*2*81+(Koordinaten[1]-as.numeric(Winddaten_Osten_10NOV_17NOV[2,3]))*2,4]
        #        Wind_v2<-Winddaten_Osten_10NOV_17NOV[2+4941*Zeitstep+(Koordinaten[2]-as.numeric(Winddaten_Osten_10NOV_17NOV[2,2]))*2*81+(Koordinaten[1]-as.numeric(Winddaten_Osten_10NOV_17NOV[2,3]))*2,5]
      }else{
        #        Ablagerung_trockene_Deposition<-Belastung  
      } #Ende if (else kann man ggf. auch löschen)
      'Jetzt kann ich eigentlich wieder ab Zeile 22 das Ganze wiederholen (Dry und Wet Deposition muss jedoch noch beachtet werden!!!!)'
      #        Trajektorie1<- Trajektorie_Grad_1.2 #oder Koordinaten? was ist mit z-Komponente?
      
'12-Prüfe räumliche Betrachtungsgrenzen'
      Koordinaten<-round(Trajektorie1_Grad[1:2]*2)/2 #Rundung nötig für weitere Durchlaufung, hier sinnvoll da so immer leichter Abstand zu Rand 
      if ((Koordinaten[1] < Koordinate_S) | (Koordinaten[1] > Koordinate_N) | (Koordinaten[2] < Koordinate_W) | (Koordinaten[2] > Koordinate_E) ){
        Belastung<-0
        Belastung_unabgelagert<-Belastung_unabgelagert+Belastung
        TESTABLAGERUNG_lat<-c(TESTABLAGERUNG_lat,Trajektorie1_Grad[1])#testzeug %&%&
        TESTABLAGERUNG_lon<-c(TESTABLAGERUNG_lon,Trajektorie1_Grad[2])#testzeug %&%&
        TESTABLAGERUNG_h<-c(TESTABLAGERUNG_h,Trajektorie1_Grad[3])#testzeug %&%&
        TESTABLAGERUNG_t<-c(TESTABLAGERUNG_t,Dauer+Entstehungszeitpunkt_aussen[Laufvariable])#testzeug %&%&2
        TESTABLAGERUNG_bel<-c(TESTABLAGERUNG_bel,Belastung)#testzeug %&%&2
      }
      
"12- Prüfen ob Funktion aufgeteilt werden muss (Dauer ist schon 4?)"
if (Dauer ==4 & Trajektorie1[3]>0){ #Trajektorienprüfung wegen vertikaler Windverschiebung hinzugefügt!
x_aussen<-c(x_aussen, Trajektorie1_Grad[1],Trajektorie1_Grad[1]+1.1,Trajektorie1_Grad[1]+1.1/sqrt(2),Trajektorie1_Grad[1],Trajektorie1_Grad[1]-1.1/sqrt(2),Trajektorie1_Grad[1]-1.1,Trajektorie1_Grad[1]+1.1/sqrt(2),Trajektorie1_Grad[1],Trajektorie1_Grad[1]+1.1/sqrt(2))
y_aussen<-c(y_aussen, Trajektorie1_Grad[2],Trajektorie1_Grad[2],Trajektorie1_Grad[2]+1.1/sqrt(2),Trajektorie1_Grad[2]+1.1,Trajektorie1_Grad[2]+1.1/sqrt(2),Trajektorie1_Grad[2],Trajektorie1_Grad[2]-1.1/sqrt(2),Trajektorie1_Grad[2]-1.1,Trajektorie1_Grad[2]-1.1/sqrt(2))
Höhe_aussen<-c(Höhe_aussen,rep(c(Trajektorie1_Grad[3]),9))
Belastung_aussen<-c(Belastung_aussen, 0.2*Belastung,rep(0.1*Belastung,8)) 
Freisetzungszeitpunkt_aussen<-c(Freisetzungszeitpunkt_aussen, rep(Freisetzungszeitpunkt,9)) #hat dieser Vektor überhaupt einen Sinn? steht eh immer das selbe drin oder ? #()()()()())()
Entstehungszeitpunkt_aussen<-c(Entstehungszeitpunkt_aussen, rep(Entstehungszeitpunkt+Dauer,9)) #()()()()())()
Belastung<-0 #stellt sicher das nichts von diesem Durchgang gespeichert wird, da ja neue Ausgangsdaten gespeichert wurden. 
 } #Ende if 
      
    } #Ende while (inneres )

#Speichern der Daten von übrig gebliebener Trockener Deposition in Vektoren für Ausgabe

if ( (Dauer+Entstehungszeitpunkt_aussen[Laufvariable]<Ende_Betrachtung_Index & Belastung>1E11)){ 
  Zeit_Auftreffen<- c(Zeit_Auftreffen,Entstehungszeitpunkt_aussen[Laufvariable]+Dauer) 
  Zeitdauer_Transport<-c(Zeitdauer_Transport,Dauer) 
  Longitude<-c(Longitude,Trajektorie1_Grad[2]) 
  Latitude<-c(Latitude,Trajektorie1_Grad[1]) 
  Aktivität<-c(Aktivität,Belastung) 
  Freisetzung_Kraftwerk<-c(Freisetzung_Kraftwerk, Freisetzungszeitpunkt) 
}else{
  Belastung_unabgelagert<-Belastung_unabgelagert+Belastung
  TESTABLAGERUNG_lat<-c(TESTABLAGERUNG_lat,Trajektorie1_Grad[1])#testzeug %&%&
  TESTABLAGERUNG_lon<-c(TESTABLAGERUNG_lon,Trajektorie1_Grad[2])#testzeug %&%&
  TESTABLAGERUNG_h<-c(TESTABLAGERUNG_h,Trajektorie1_Grad[3])#testzeug %&%&
  TESTABLAGERUNG_t<-c(TESTABLAGERUNG_t,Dauer+Entstehungszeitpunkt_aussen[Laufvariable])#testzeug %&%&2
  TESTABLAGERUNG_bel<-c(TESTABLAGERUNG_bel,Belastung)#testzeug %&%&2
} #Ende if
}#Ende while aussen    
    
" in äußere while schleife wurde folgendes gezogen: 
#Speichern der Daten von übrig gebliebener Trockener Deposition in Vektoren für Ausgabe
    
    if (Belastung>0 & (Dauer+Freisetzungszeitpunkt<Ende_Betrachtung_Index)){ #macht sinn nur dann was einzutragen wenn es noch etwas gibt! - anderer Wert als 0 sinnvoll?
      Zeit_Auftreffen<- c(Zeit_Auftreffen,Freisetzungszeitpunkt+Dauer) #Zeit des Auftreffens am Boden, PASST
      Zeitdauer_Transport<-c(Zeitdauer_Transport,Dauer) #Zeitdauer des Transportes STAND, PASST
      Longitude<-c(Longitude,Trajektorie1_Grad[2]) #Koordinate Auftreffpunkt Ostrichtung, Welche Koordinate???
      Latitude<-c(Latitude,Trajektorie1_Grad[1]) #Koordinate Auftreffpunkt Nordrichtung, welche Koordinate??? Koordinaten vs. Trajektorie1.2(Grad?) vs. Trajektorie1_Grad
      Aktivität<-c(Aktivität,Belastung) #Belastung am Boden
    }else{Belastung_unabgelagert<-Belastung_unabgelagert+Belastung} #Ende if
"

  } #Ende for
} # Ende for 

#Output (Koordinaten der Auftrittspunkte,B,D,t(?),...  )
#Zeit_Auftreffen<- 'Zeit des Auftreffens am Boden'
#Zeitdauer_Transport<- 'Zeitdauer des Transportes'
#Longitude<- 'Koordinate Auftreffpunkt Ostrichtung'
#Latitude<- 'Koordinate Auftreffpunkt Nordrichtung'
#Aktivität<-'Belastung am Boden'


Daten_Maxima<-data.frame(Freisetzung_Kraftwerk,Zeit_Auftreffen,Zeitdauer_Transport,Latitude,Longitude,Aktivität) #Anmerkung: Immer wenn hier was auf Boden Trifft (else und wet deposition (?!)) muss Vektor um 1 mit jeweiligen Werten verlängert werden! 
View(Daten_Maxima)

#Export als .csv
Dateiname<-c(paste0("Sim_10NovWind_","Ost_",as.character(ORT_Freisetzung[2]),"_Nord_",as.character(ORT_Freisetzung[1]),"_",Sys.Date(),".csv"))

#Daten betrachten
plot(Daten_Maxima[,5],Daten_Maxima[,4],xlim=c(ORT_Freisetzung[2]-20,ORT_Freisetzung[2]+20), ylim=c(ORT_Freisetzung[1]-20,ORT_Freisetzung[1]+20),
     xlab= "Längengrad", ylab="Breitengrad", main = "Auftreffpunkte der Wolken")

'
#Daten exportieren
for (k in 1:(round(dim(Daten_Maxima)[1]/1000))){
  write.csv(Daten_Maxima[1:1000,],c(paste0( "Daten_Maxima/Daten für Maxima",as.character((k-1)*1000+1),"-",as.character(k*1000),".csv")), row.names = FALSE)
}
'

#Gesamtes CSV: write.csv(Daten_Maxima[1:1000,], "Daten für Maxima.csv", row.names = FALSE)
TESTABLAGERUNG<-data.frame(TESTABLAGERUNG_lat,TESTABLAGERUNG_lon,TESTABLAGERUNG_h,TESTABLAGERUNG_t,TESTABLAGERUNG_bel) #testzeug %&%&2
View(TESTABLAGERUNG)
