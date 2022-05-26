library(neuralnet) # Yapay Sinir Aðlarý Modeli
library(rJava)
library(XLConnect)
library(ggplot2)
library(forecast)
library(zoo)
library(lmtest)
library(readxl)


yanginPrim <- read_excel("C:/Users/Berke/Desktop/yanginPrim.xlsx")
#Veri setini yükleme. Veri setimiz 2011 yýlýnýn Ocak ayýndan, 2021 yýlýnýn Aralýk
#ayýna kadar olan aylýk yangýn ve doðal afet primlerini içeriyor. Tsb'den alýnmýþtýr.

veri <- as.data.frame(yanginPrim)
#Veri manipülasyonu yapmak için data frame hale getirdik.


egitim_veri <- ts(veri$yangýnPrim[1:108],start=c(2011,1), frequency=12)
test_veri <- ts(veri$yangýnPrim[109:132], start=c(2020,1), frequency=12)
#Ýlk 108 ayýn verisini -2011'den 2019 yýlýnýn sonuna kadar- modelleme yaparken kullanacaðýz.
#Son 2 yýlý test etmek için kullanacaðýz. Kurduðumuz model ile son 2 yýlýn verisini karþýlaþtýracaðýz.



### Zaman Serisi Grafigi ###
par(mfrow=c(1,1))
ts.plot(egitim_veri,gpars=list(xlab="aylar",ylab="prim tutarlarý"))
yanginPrim_ts<-ts(egitim_veri,start=c(2011,1), frequency=12)  #aylik veri oldugundan 12 
#2019 yýlýnýn sonuna kadar olan verilerin zaman serisi grafiðidir.
#Trend ve mevsimsellik var gibi görünüyor. ACF grafiðinden daha net yorum yapabiliriz.



### ACF ve PACF Grafikleri (Korelogram) ###

#install.packages("forecast")
library(forecast)
par(mfrow=c(2,1))
Acf(yanginPrim_ts,lag.max=42,ylim=c(-1,1),lwd=3)  #otokorelasyon grafigi
Pacf(yanginPrim_ts,lag.max=42,ylim=c(-1,1),lwd=3)  #kismi otokorelasyon grafigi
par(mfrow=c(1,1))

#ACF grafiðine bakýldýðýnda ilk 4 gecikme sýnýr dýþýnda olduðu için trend vardýr. 
#Seri duraðan olmadýðýndan 1. fark iþlemi almamýz gerekmektedir.

###ÞEYDA

### Zaman Serilerinde Fark Islemleri ###

yanginPrim1_ts<-diff(yanginPrim_ts) # Serinin birinci farki
par(mfrow=c(2,1))
Acf(diff(yanginPrim_ts),lag.max=42,ylim=c(-1,1),lwd=3)  #birinci farklarin otokorelasyon grafigi
Pacf(diff(yanginPrim_ts),lag.max=42,ylim=c(-1,1),lwd=3)  #birinci farklarin kismi otokorelasyon grafigi
par(mfrow=c(1,1))

## Serinin 1. farký alýndýðýnda ACF grafiðinde periyodu 12 olan mevsimsellik gözlemlenmiþtir.
## Seri duraðan olmadýðýndan ayrýca 1. mevsimsel fark iþlemi uygulanýr.

yanginPrim2_ts<-diff(diff(yanginPrim_ts),lag=12) # Serinin birinci ve birinci mevsimsel farki

par(mfrow=c(2,1))
Acf(yanginPrim2_ts,lag.max=42,ylim=c(-1,1),lwd=3)
Pacf(yanginPrim2_ts,lag.max=42,ylim=c(-1,1),lwd=3)
par(mfrow=c(1,1))

#Fark iþlemlerinden sonra seri duraðan hale gelmiþtir. Modellemeye uygun hale gelmiþtir.
#p=1, d=1, q=1, D=1
#ACF grafiði daha hýzlý azalýyor gibi göründüðü için MA(1) modeli daha uygun olabilir.
#Önce MA(1) modellerini, daha sonra AR(1) ve AR-MA(1,1) modellerinin hepsini deneyelim.

###FURKAN

### ARIMA(p,d,q)(P,D,Q)s Modelinin Seriye Uygulanmasi ###
###### MA(1)
yanginArima1=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima1)
BIC(yanginArima1) #bu model anlamlý býc deðeri 3594.016
summary(yanginArima1)


yanginArima2=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima2) #anlamsýz

yanginArima3=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima3) #anlamsýz

yanginArima4=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima4) #anlamsýz


###AR(1)
yanginArima5=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima5)
BIC(yanginArima5) #bu model anlamlý býc deðeri 3598.759

yanginArima6=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima6)
BIC(yanginArima6) #bu model anlamsýz, P deðerini bir arttýrýp (2,1,0)'ý göstermemize gerek yoktur.


yanginArima7=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(2,1,0),include.constant = TRUE)
coeftest(yanginArima7) #anlamsýz olacaðýný bildiðimiz halde test sonuçlarýyla göstermek istedik.


yanginArima8=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima8) #anlamsýz

yanginArima9=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima9) #anlamsýz



##### ARMA
yanginArima10=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima10)
BIC(yanginArima10) #bu model anlamlý býc deðeri 3591.493

yanginArima11=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima11) #anlamsýz

yanginArima12=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima12) #anlamsýz

yanginArima13=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima13) #anlamsýz

#en düþük BIC deðerini veren model ARIMA(1,1,1),(0,1,0)'dýr. 
#En uygun modeli bir de auto.arima fonksiyonu ile test edelim.

auto.arima(yanginPrim_ts, d=1,D=1)
#auto.arima fonksiyonu da en uygun modeli ARIMA(1,1,1),(0,1,0) olarak belirledi.

plot(window(yanginPrim_ts),xlab="Zaman(ay)",ylab="",lty=1,col=4,lwd=1)
lines(window(yanginArima10[["fitted"]]),lty=3,col=2,lwd=2)
legend("topleft",c(expression(paste(gercek)),
                   expression(paste(Tahmin))),lwd=c(2,2),lty=c(1,3),cex=0.6,col=c(4,2))

#2019 yýlý sonuna kadar olan verilerin analizini yaptýk.
#mavi çizgiler gerçek veri, kýrmýzý çizgiler ise ARIMA modelinin sonuçlarýdýr.
#grafiðe göre anlamlý bir sonuç elde edildiði söylenebilir. 

### Hatalarin ACF ve PACF Grafikleri ###
Acf(yanginArima10[["residuals"]],lag.max=42,ylim=c(-1,1),lwd=3)  #otokorelasyon grafigi
Pacf(yanginArima10[["residuals"]],lag.max=42,ylim=c(-1,1),lwd=3)  #kismi otokorelasyon grafigi

#hatalar akgürültüdür, model anlamlýdýr.
#diðer aþamada 2020 ile 2022 yýllarý arasýndaki gerçek deðerler ile,
#ARIMA modelinden elde ettiðimiz öngörü deðerlerini karþýlaþtýracaðýz.

ongoru<-forecast(yanginArima10,h=24)
plot(ongoru,ylim=c(min(veri$yangýnPrim),max(veri$yangýnPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("ARIMA Tahmin", "gerçek veri"), lwd=c(3,3), col=c("blue", "red"))

#grafikte gerçek veri ile ARIMA modelinin öngörüsü arasýnda 2021 yýlý sonunda fark vardýr.
#bunun sebebi 2021 yýlý yaz döneminde ülke genelinde gerçekleþen yangýnlarýn sonucunda,
#sigorta yaptýrma bilincinin artmýþ olmasý sonucu primlerde yüksek bir artýþ gerçekleþmiþtir. 

### YAPAY SÝNÝR AÐLARI ###
library(neuralnet) # Yapay Sinir Aðlarý Modeli için kullanýlan fonksiyondur.
?neuralnet

NN1 <- nnetar(egitim_veri)
#Ýlk baþta parametrelere hiç deðer vermedik. Otomatik olarak fonksiyon kendisi belirledi. 
NN1
#p deðerini 2, P=1, size=2 olarak otomatik belirlenmiþ.
plot(forecast(NN1, h=24),ylim=c(min(veri$yangýnPrim),max(veri$yangýnPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir aðlarý modeli", "gerçek verisi"), lwd=c(3,3), col=c("blue", "red"))


NN2 <- nnetar(egitim_veri , size=3 , p=2 , P=2 , lambda="auto" , repeats=20)
#Parametreleri kendimiz girdik.
NN2
plot(forecast(NN2, h=24),ylim=c(min(veri$yangýnPrim),max(veri$yangýnPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir aðlarý modeli", "test verisi"), lwd=c(3,3), col=c("blue", "red"))



NN3 <- nnetar(egitim_veri, P=2, p=1)
#Son olarak bu þekilde deneyelim
NN3
plot(forecast(NN3, h=24),ylim=c(min(veri$yangýnPrim),max(veri$yangýnPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir aðlarý modeli", "test verisi"), lwd=c(3,3), col=c("blue", "red"))



veri_ts <- ts(veri$yangýnPrim[],start=c(2011,1), frequency=12)
#2022 yýlýna kadar olan bütün verileri zaman serisi olarak atadýk.
veri_ts
NN4 <- nnetar(veri_ts, p=12, size=24)
#Bu verileri yapay sinir aðýna tanýtýp 2023'e kadar olan tahmini bulalým.
plot(forecast(NN4, h=12))
lines(veri_ts, type='l', col="black", lwd=2)
legend("topleft", c("Tahmin-yapay sinir aðlarý modeli", "gerçek veri"), lwd=c(3,3), col=c("blue", "black"))


