#Skrip by: robi.muharsyah@gmail.com
#Skrip bertujuan untuk mengubah ch harian menjadi CH bulanan
#Skrip juga mengisi nilai CH bulanan yang kosong dengan Nilai Klimatologi
#Skrip ini menampilkan plot time series dan klimatologi, serta melakukan dekomposisi
#skrip ini menggunakan Fungsi "day2mon_ch.R"
rm(list = ls())
graphics.off()

#******** SESUAIKAN BAGIAN BERIKUT INI *******************************************************
setwd("C:/Users/X1/OneDrive/Work_2024_onedrive/PM4_ITERA/SIMULASI/P4/") #set working direktory
rm(list = ls()) #hapus semua variable di environmen

dirData    <- "C:/Users/X1/OneDrive/Work_2024_onedrive/PM4_ITERA/SIMULASI/P1/data_precipitation_daily/"
dirInfo    <- "C:/Users/X1/OneDrive/Work_2024_onedrive/PM4_ITERA/SIMULASI/P1/data_precipitation_daily/"
dirFungsi  <- "C:/Users/X1/OneDrive/Work_2024_onedrive/PM4_ITERA/SIMULASI/P1/add_function/"
dirHasil   <- "C:/Users/X1/OneDrive/Work_2024_onedrive/PM4_ITERA/SIMULASI/P4/hasil/"
dir.create(dirHasil,recursive = T,showWarnings=F)

fileData <- paste(dirInfo,"out_Data_CH_daily.csv",sep="")
DATA     <- read.csv(fileData,header=F)
fileInfo <- paste(dirInfo,"out_Data_CH_daily_infosta.csv",sep="")
INFO     <- read.csv(fileInfo,header=T)

startYr <- 1996
endYr   <- 2020
nosta   <- 96291 #lihat pilihan stasiun pada file "out_Data_CH_daily_infosta.csv"

#***************** Mulai dari sini tidak perlu diganti ***************
DATA    <- DATA[which(DATA[,1]>=startYr &DATA[,1]<=endYr),] 
idxData <- which(INFO$NoSTA==nosta)
data    <- DATA[,c(1,2,3,idxData+3)]

#fungsi untuk menghitung ch bulanan
source(paste(dirFungsi,"day2mon_precip.R",sep=""))
ch_mon<-day2mon_precip(data)
 
# tambal jika ada NaN
source(paste(dirFungsi,"fill_with_clim.R",sep=""))
ch_mon_ori     <- ch_mon$original
ch_mon_fillmis <- fill_with_clim(ch_mon_ori)
ch_mon_fill    <- ch_mon_fillmis$originalfill
ch_mon_clim    <- ch_mon_fillmis$climatology #Nilai klimatologi atau rata2 

# LANGKAH 1: Siapkan data deret waktu
source(paste(dirFungsi,"ori2tsmon.R",sep=""))
df_ch_mon <- ori2tsmon(ch_mon_fill)

#LANGKAH 2: Tampilkan plot time series - sepanjang waktu
library(ggplot2)
library(scales)
chmon_line <- ggplot(df_ch_mon, aes(TglBln, VAL)) +
  geom_line(na.rm=TRUE) +  
  ggtitle(paste("Curah Hujan Bulanan\n", "Stasiun :",as.character(nosta),sep="") ) +
  xlab("Bulanan") + ylab("Curah Hujan (mm)") +
  scale_x_date(breaks = date_breaks("2 year"),labels = date_format("%b %y")) +
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 15)) +
  theme(text = element_text(size=10))
chmon_line

#LANGKAH 3: tampilkan plot dekompoisii time-series
#Dekompoisisi dg menggunakan fungsi stl() dari library stats
hujan_ts <- ts(data =df_ch_mon$VAL, frequency = 12, start = c(startYr,1))
#Selecting Data 
hujan_ts <- window(hujan_ts, start=c(startYr,1))
decomp   <- stl(hujan_ts, s.window = 'periodic')
#Plot decomposition
autoplot(decomp,range.bars = F) + theme_bw()

#LANGKAH 4: tampilkan seberapa mirip plot klimatologi dan plot seasonal
hedmon <- c("JAN","FEB","MAR","APR","MEI","JUN","JUL","AGT","SEP","OKT","NOV","DES")
df_seas <- decomp$time.series[1:12,1]
ch_clim_seas <- data.frame(y=ch_mon_clim, z=df_seas,x=hedmon)
#plot perbandingan nilai CH selama periode testing
par(mar = c(5, 5, 3, 5))
plot(ch_clim_seas[,"y"], type ="l", ylab = "CH (mm)",
     main = "Perbandingan: Klimatologi vs Seasonality", xlab = "Bulanan",
     col = "blue",xaxt="n")
par(new = TRUE)
plot(ch_clim_seas[,"z"], type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("Seasonality", side = 4, line = 3)
legend("bottomright", c("KLIM", "SEAS"),col = c("blue", "red"), lty = c(1, 2),cex=0.5)
ticks <- seq(1,12,1)
axis(side = 1, at = ticks,labels = hedmon)

#LANGKAH5: Tampilkan pola seasonal pertahun
#Siapkan Data dalam format TS
library(tseries)
ts_ch_mon <-ts(df_ch_mon$VAL)
head(ts_ch_mon)

#plot seasonal per-12 bulan
library(forecast)
seasonplot(ts_ch_mon,12,main="Seasonal Plot", ylab="CH(mm)",year.labels = T, 
           col=rainbow(startYr-endYr+1))

#LANGKAH6: Menampilkan plot Seasonal sub-series dan boxplot
library(tsutils)
#Seasonal Sub-Series Plot
seasplot(hujan_ts, outplot = 3, trend = TRUE, 
         main = "Seasonal Subseries Plot", ylab= "Rainfall (mm2)")
#Seasonal Boxplot
seasplot(hujan_ts, outplot = 2, trend = TRUE, 
         main = "Seasonal Box Plot", ylab= "Rainfall (mm2)")

#LANGKAH7: menghitung seberapa kuat sinyal seasonal
Tt <- trendcycle(decomp)
St <- seasonal(decomp)
Rt <- remainder(decomp)
#Trend Strength Calculation
Ft <- round(max(0,1 - (var(Rt)/var(Tt + Rt))),1)
#Seasonal Strength Calculation
Fs <- round(max(0,1 - (var(Rt)/var(St + Rt))),1)
data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)
#Jika nilai Fs semakin dekat ke 1 maka sinyal seasonal makin kuat

