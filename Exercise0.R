#Oppgave 1
#a)
x=c(1:1000)
y=sample(x,100)

#b)
mean(y[1:10])

#c)
y[-5]

#d)
y[length(y)]>500

#e)
sum(y<10)

#f)
M=matrix(nrow=10,ncol=2)

#g)
M[2,1]=2

#h)
for (r in 1:nrow(M)){
  for(c in 1:ncol(M)){
    M[r,c]=(r*c)
  }
}

#i)
myFunc = function(M){
  for (r in 1:nrow(M)){
    for(c in 1:ncol(M)){
      if (r*c>15){
        print(c(r,c))
      }
    }
  }     
}

#Oppgave 2
#a)
biler = c(53,64,12,14,53,64,100,120,145,222,350,240,233,
          255,305,373,403,407,320,200,187,170,57,24)
t = seq(from = 0, to = 23, by = 1)

biler_tids=plot(x=t, y=biler, main = "Antall biler på ulike tidspunkt",
     xlab="Klokkeslett", ylab="Antall biler")


#b)
max(biler)
min(biler)

#c)
g=t[which.max(biler)]
a=t[which.min(biler)]

#d)
biler_tids=plot(x=t, y=biler, main = "Antall biler på ulike tidspunkt",
                xlab="Klokkeslett", ylab="Antall biler")
abline(v=g,col="purple")
abline(v=a,col="pink")

#e) 

t[which(biler>100)]

#Oppgave 3
#a)
library(datasets)
data = data.frame(ChickWeight)

dim(ChickWeight)
#578 rader og 4 kolonner

#b)
table(data$Chick)

#c)
diet_tab=table(data$Time,data$Diet)


#d) Først lager vi en matrise m som har 12 rader og 4 kolonner som
# har null i seg. SEtter kolonnenavnene til 1,2,3,4. Setter radnavnene 
#til til m som gjenspeiler tiden uten gjentagning. Blir da 12 rader som
#heter tiden (0-21).
m = matrix(0, nrow=12, ncol=4)
colnames(m) = c("1", "2", "3", "4")
row.names(m) = unique(data$Time)

for(i in 1:dim(data)[1]){
#for løkken iterer gjennom radene i datasettet, det vil si fra 1. til rad 578.
  t = data$Time[i]
  #Setter variablen t som tilhørende nummer på "Time". Radnummer.
  r = match(t, row.names(m))
  #Her settes t sammen med radnavnet til tid.
  
  d = data$Diet[i]
  if(data$weight[i]>m[r,d]){
    m[r,d] = data$weight[i]
   }
}
#Denne koden itererer gjennom dataene våre og lagrer den største registrerte vekten for hver diett til
#hvert tidspunkt.
#Først lages en matrise med verdi 0 for alle elementene. Den får rad- og kolonnenavn etter tidspunktene
#og dietten. Deretter itereres det over radene i data (i), der vi lagrer tidspunkt (t) og diett (d) for denne
#kyllingen og registrerer hvilken rad (r) dette tilsvarer i matrisen m. Vi tester om vekten til kylling nr i er
#større enn det som er registrer i matrisen på rad r kolonne d, og hvis denne vekten er større bytter vi ut
#verdien i m med denne vekten. Når vi har iterert gjennom hele datasettet vil vi dermed stå igjen med de
#største vektene ved hvert tidspunkt for hver diett.


#e)
ma= matrix(0, nrow=12, ncol=4)
colnames(ma) = c("1", "2", "3", "4")
row.names(ma) = unique(data$Time)
for(i in 1:dim(data)[1]){
  t = data$Time[i]
  r = match(t, row.names(ma))
  d = data$Diet[i]
  ma[r,d]=ma[r,d]+data$weight[i]
  }
ma=ma/diet_tab

#f)
library(ggplot2)
data$Time = as.factor(data$Time)
data$Diet = as.factor(data$Diet)
ggplot(data, aes(x=Time, y=weight, group=Chick)) + geom_line(aes(color=Diet))
ggplot(data, aes(x=Time, y=weight, fill=Diet)) + geom_boxplot()

#g)
#diet 3 over tid, men i starten diet 4

