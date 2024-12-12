

            #Intervals de confiança i contrast hipòtesis



x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

# volem crear un interval de confiança 

xbar<- mean(x)   #media muestral (es distribueix normalment també)
xbar
sigma<-sqrt(25)
sigma

n<-length(x)
zalfa2<-qnorm(0.95)     # busquem la z alfa mitjos per calcular l'interval
zalfa2
c(xbar-zalfa2*sigma/sqrt(n), xbar+zalfa2*sigma/sqrt(n))  #interval al 90%

#instalar libreria només una vegada
install.packages("BSDA")
library(BSDA)
z.test(x, sigma.x=sigma, conf.level = 0.9 )  # si ens donen tot l'experiment millor usar z.test


#probar hipotesis de que les caixes tenen un pes diferent a mu0=500gr
#H0: mu =500
#H1: mu != 500  #hipotesis alternativa
mu0 <-500
#CRITERI1 (mu0 cau en interval de confiança?)
# per l'exercici d'abans, mureal esta entre 501 i 506, --> rechazamos H0 

#CRITERI2 (error estandaritzat)
zcritic<-qnorm(0.95)     #z es el valor normalitzat de (xbar-mu0)/(sigma/sqrt(n))
zcritic

zobservada <- (xbar-mu0)/(sigma/sqrt(n))   #error estandaritzat que cometem
zobservada             # com z observada fora de l'interval (-zcr , +zcr) rechazamos H0

z.test(x, sigma.x=sigma, conf.level = 0.9, mu=mu0 )  # escrivim H0
                                                     #vull clavar els 500gr, les dues cues

#CRITERI3 (PVALOR)

#si pvalor<alfa, H0 rechazamos, H1 aceptamos
#si pvalor>=alfa, H0 aceptamos
pvalue<- 2*pnorm(-zobservada)
pvalue


#canviem el problema, ara ens interessa <=500gr

z.test(x, sigma.x=sigma, conf.level = 0.9, mu=mu0, alternative='greater' )  # escrivim grater si la H1 és mureal>mu0 (less si es al revés)
                                                                            #ara volem tenir menys de 500, seleccionem 1 cua no 2



#PREG2
# fer a mà seguint IC( xbar-zalfa2*sigma/sqrt(n) ) i deduccions
n2<- (qnorm(0.975)*5)^2
n2    # necessitem 97 mostres per a tenir un interval de 2gr al 95%


#PREG3  Variança desconeguda   
#al no tenir sigma hem d'utilitzar tstudent no podem utilitzar zalfa

s<- sqrt(var(x))
s
sigma   #veiem com cometem error, però el paguem amb la tstudent

xbar <-(mean(x))
t05<- qt(0.995,n-1) #calculem l'equivalent a la z alfa mitjos
IC99<- c(xbar-t05*s/sqrt(n), xbar+t05*s/sqrt(n))
IC99

t.test(x, conf.level = 0.99)   # interval de conf, pvalue,

t.test(x, conf.level = 0.99, mu=500)   # tobservada, degrees of freedom

tobs<-(xbar-500)/(s/sqrt(n))
tobs














