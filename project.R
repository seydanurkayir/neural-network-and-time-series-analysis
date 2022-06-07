library(neuralnet) # Yapay Sinir Ağları Modeli
library(rJava)
library(XLConnect)
library(ggplot2)
library(forecast)
library(zoo)
library(lmtest)
library(readxl)


yanginPrim <- read_excel("C:/Users/Berke/Desktop/yanginPrim.xlsx")
#Veri setini yükleme. Veri setimiz 2011 yılının Ocak ayından, 2021 yılının Aralık
#ayına kadar olan aylık yangın ve doğal afet primlerini içeriyor. Tsb'den alınmıştır.

veri <- as.data.frame(yanginPrim)
#Veri manipülasyonu yapmak için data frame hale getirdik.


egitim_veri <- ts(veri$yangınPrim[1:108],start=c(2011,1), frequency=12)
test_veri <- ts(veri$yangınPrim[109:132], start=c(2020,1), frequency=12)
#İlk 108 ayın verisini -2011'den 2019 yılının sonuna kadar- modelleme yaparken kullanacağız.
#Son 2 yılı test etmek için kullanacağız. Kurduğumuz model ile son 2 yılın verisini karşılaştıracağız.



### Zaman Serisi Grafigi ###
par(mfrow=c(1,1))
ts.plot(egitim_veri,gpars=list(xlab="aylar",ylab="prim tutarları"))
yanginPrim_ts<-ts(egitim_veri,start=c(2011,1), frequency=12)  #aylik veri oldugundan 12 
#2019 yılının sonuna kadar olan verilerin zaman serisi grafiğidir.
#Trend ve mevsimsellik var gibi görünüyor. ACF grafiğinden daha net yorum yapabiliriz.



### ACF ve PACF Grafikleri (Korelogram) ###

#install.packages("forecast")
library(forecast)
par(mfrow=c(2,1))
Acf(yanginPrim_ts,lag.max=42,ylim=c(-1,1),lwd=3)  #otokorelasyon grafigi
Pacf(yanginPrim_ts,lag.max=42,ylim=c(-1,1),lwd=3)  #kismi otokorelasyon grafigi
par(mfrow=c(1,1))

#ACF grafiğine bakıldığında ilk 4 gecikme sınır dışında olduğu için trend vardır. 
#Seri durağan olmadığından 1. fark işlemi almamız gerekmektedir.

###ŞEYDA

### Zaman Serilerinde Fark Islemleri ###

yanginPrim1_ts<-diff(yanginPrim_ts) # Serinin birinci farki
par(mfrow=c(2,1))
Acf(diff(yanginPrim_ts),lag.max=42,ylim=c(-1,1),lwd=3)  #birinci farklarin otokorelasyon grafigi
Pacf(diff(yanginPrim_ts),lag.max=42,ylim=c(-1,1),lwd=3)  #birinci farklarin kismi otokorelasyon grafigi
par(mfrow=c(1,1))

## Serinin 1. farkı alındığında ACF grafiğinde periyodu 12 olan mevsimsellik gözlemlenmiştir.
## Seri durağan olmadığından ayrıca 1. mevsimsel fark işlemi uygulanır.

yanginPrim2_ts<-diff(diff(yanginPrim_ts),lag=12) # Serinin birinci ve birinci mevsimsel farki

par(mfrow=c(2,1))
Acf(yanginPrim2_ts,lag.max=42,ylim=c(-1,1),lwd=3)
Pacf(yanginPrim2_ts,lag.max=42,ylim=c(-1,1),lwd=3)
par(mfrow=c(1,1))

#Fark işlemlerinden sonra seri durağan hale gelmiştir. Modellemeye uygun hale gelmiştir.
#p=1, d=1, q=1, D=1
#ACF grafiği daha hızlı azalıyor gibi göründüğü için MA(1) modeli daha uygun olabilir.
#Önce MA(1) modellerini, daha sonra AR(1) ve AR-MA(1,1) modellerinin hepsini deneyelim.

###FURKAN

### ARIMA(p,d,q)(P,D,Q)s Modelinin Seriye Uygulanmasi ###
###### MA(1)
yanginArima1=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima1)
BIC(yanginArima1) #bu model anlamlı bıc değeri 3594.016
summary(yanginArima1)


yanginArima2=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima2) #anlamsız

yanginArima3=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima3) #anlamsız

yanginArima4=Arima(yanginPrim_ts,order=c(0,1,1),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima4) #anlamsız


###AR(1)
yanginArima5=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima5)
BIC(yanginArima5) #bu model anlamlı bıc değeri 3598.759

yanginArima6=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima6)
BIC(yanginArima6) #bu model anlamsız, P değerini bir arttırıp (2,1,0)'ı göstermemize gerek yoktur.


yanginArima7=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(2,1,0),include.constant = TRUE)
coeftest(yanginArima7) #anlamsız olacağını bildiğimiz halde test sonuçlarıyla göstermek istedik.


yanginArima8=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima8) #anlamsız

yanginArima9=Arima(yanginPrim_ts,order=c(1,1,0),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima9) #anlamsız



##### ARMA
yanginArima10=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(0,1,0),include.constant = TRUE)
coeftest(yanginArima10)
BIC(yanginArima10) #bu model anlamlı bıc değeri 3591.493

yanginArima11=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(1,1,0),include.constant = TRUE)
coeftest(yanginArima11) #anlamsız

yanginArima12=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(0,1,1),include.constant = TRUE)
coeftest(yanginArima12) #anlamsız

yanginArima13=Arima(yanginPrim_ts,order=c(1,1,1),seasonal=c(1,1,1),include.constant = TRUE)
coeftest(yanginArima13) #anlamsız

#en düşük BIC değerini veren model ARIMA(1,1,1),(0,1,0)'dır. 
#En uygun modeli bir de auto.arima fonksiyonu ile test edelim.

auto.arima(yanginPrim_ts, d=1,D=1)
#auto.arima fonksiyonu da en uygun modeli ARIMA(1,1,1),(0,1,0) olarak belirledi.

plot(window(yanginPrim_ts),xlab="Zaman(ay)",ylab="",lty=1,col=4,lwd=1)
lines(window(yanginArima10[["fitted"]]),lty=3,col=2,lwd=2)
legend("topleft",c(expression(paste(gercek)),
                   expression(paste(Tahmin))),lwd=c(2,2),lty=c(1,3),cex=0.6,col=c(4,2))

#2019 yılı sonuna kadar olan verilerin analizini yaptık.
#mavi çizgiler gerçek veri, kırmızı çizgiler ise ARIMA modelinin sonuçlarıdır.
#grafiğe göre anlamlı bir sonuç elde edildiği söylenebilir. 

### Hatalarin ACF ve PACF Grafikleri ###
Acf(yanginArima10[["residuals"]],lag.max=42,ylim=c(-1,1),lwd=3)  #otokorelasyon grafigi
Pacf(yanginArima10[["residuals"]],lag.max=42,ylim=c(-1,1),lwd=3)  #kismi otokorelasyon grafigi

#hatalar akgürültüdür, model anlamlıdır.
#diğer aşamada 2020 ile 2022 yılları arasındaki gerçek değerler ile,
#ARIMA modelinden elde ettiğimiz öngörü değerlerini karşılaştıracağız.

ongoru<-forecast(yanginArima10,h=24)
plot(ongoru,ylim=c(min(veri$yangınPrim),max(veri$yangınPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("ARIMA Tahmin", "gerçek veri"), lwd=c(3,3), col=c("blue", "red"))

#grafikte gerçek veri ile ARIMA modelinin öngörüsü arasında 2021 yılı sonunda fark vardır.
#bunun sebebi 2021 yılı yaz döneminde ülke genelinde gerçekleşen yangınların sonucunda,
#sigorta yaptırma bilincinin artmış olması sonucu primlerde yüksek bir artış gerçekleşmiştir. 

### YAPAY SİNİR AĞLARI ###
library(neuralnet) # Yapay Sinir Ağları Modeli için kullanılan fonksiyondur.
?neuralnet

NN1 <- nnetar(egitim_veri)
#İlk başta parametrelere hiç değer vermedik. Otomatik olarak fonksiyon kendisi belirledi. 
NN1
#p değerini 2, P=1, size=2 olarak otomatik belirlenmiş.
plot(forecast(NN1, h=24),ylim=c(min(veri$yangınPrim),max(veri$yangınPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir ağları modeli", "gerçek verisi"), lwd=c(3,3), col=c("blue", "red"))


NN2 <- nnetar(egitim_veri , size=3 , p=2 , P=2 , lambda="auto" , repeats=20)
#Parametreleri kendimiz girdik.
NN2
plot(forecast(NN2, h=24),ylim=c(min(veri$yangınPrim),max(veri$yangınPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir ağları modeli", "test verisi"), lwd=c(3,3), col=c("blue", "red"))



NN3 <- nnetar(egitim_veri, P=2, p=1)
#Son olarak bu şekilde deneyelim
NN3
plot(forecast(NN3, h=24),ylim=c(min(veri$yangınPrim),max(veri$yangınPrim)))
lines(test_veri, type='o', col="red", lwd=2)
legend("topleft", c("Tahmin-yapay sinir ağları modeli", "test verisi"), lwd=c(3,3), col=c("blue", "red"))



veri_ts <- ts(veri$yangınPrim[],start=c(2011,1), frequency=12)
#2022 yılına kadar olan bütün verileri zaman serisi olarak atadık.
veri_ts
NN4 <- nnetar(veri_ts, p=12, size=24)
#Bu verileri yapay sinir ağına tanıtıp 2023'e kadar olan tahmini bulalım.
plot(forecast(NN4, h=12))
lines(veri_ts, type='l', col="black", lwd=2)
legend("topleft", c("Tahmin-yapay sinir ağları modeli", "gerçek veri"), lwd=c(3,3), col=c("blue", "black"))


