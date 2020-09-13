install.packages("gamlss.data")
suppressMessages(library(gamlss.data))
data = data.frame(rent99)
head(data)
?rent99
num_rows = nrow(data)
train = data[1:floor(0.6*num_rows),]
test = data[ceiling(0.6*num_rows):num_rows,]


#Oppgave 1)
model=lm(rent~ area +yearc+ location+ bath+
           kitchen+cheating+district,data=train)
summary(model)

#Oppgave 2)

#a)
#Hva betyr tallene i area: 
#For hver ekstra kvadratmeter får man 4,4 euro mer i leie
#Standardfeilen er 0,146. Gjennomsnittet av std.avviket er std.feil.
#t-verdi er på 29,8 (høyeste i modellen). Bra
#p-verdien er veldig lav, som betyr at forklaringsvariabelen er god.
#Statistisk signifikant.

#Estimate for intercept betryr at dersom alle variablene er null
#Vil verdien på leiligheten være -93,96 euro/kvm. Gir ikke mening

#95% konfidensintervall:
#konfindensintervall_95_area=[(4.365147-2*0.146279),(4.365147+2*0.146279)]
confint(model)


#b) Residual std. error
#Forteller hvor mye variablene varierer 
# fra den lineære modellen ved ulike frihetsgrader. 

#c) Multiple R-Squared
#R square forteller oss hvor mye av variasjonen i den 
#avhengige variabelen som forklares av de uavhengige variablene.

#d) F-statistic og p-verdien:
#En f-test som er en hypotesetest for hele modellen
#H0=0
#H1=/=0 Minst en x er høyere enn 0.
#Dersom H0 er sann vil f-verdien være nær 1 ellers mye større enn 1.
#Tilhørende p-verdi: viser at f-testen er signifikant.


#Opppgave 3) Hva sier plottet?
install.packages("GGally")
install.packages("ggplot2")
suppressMessages(library(GGally))
suppressMessages(library(ggplot2))
ggplot(model, aes(.fitted, .stdresid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Standardized residuals",
       title = "Fitted values vs. standardized residuals",
       subtitle = deparse(model$call))
## `geom_smooth()` using formula 'y ~ x'

# Den røde linjen er en lineær regresjonslinje av leiekostnadene.
#Prikkene er observasjoner for de ulike variablene 
#area, year, location, bath, kirtchen.
#Setter x aksen som tilhørende verdier og y aksen som stanardiserte residualer.
#Residualene viser hvor lang avstand det er mellom observasjonene og regresjonslinjen.

#Oppgave 4)
modelsqm=lm(rentsqm ~ area +yearc+ location+ bath+
           kitchen+cheating+district,data=train)
summary(modelsqm)

#Oppgave 5) Sammenlikning:
#Vi ser at variablene påvirker modelsqm i mindre grad enn model. Kommer også 
#frem ved å se på R squared. Area og yearc, district har lave verdier --> ikke signifikante,
#aka ikke påvirker kvadratmeter.

#Oppgave 6)
suppressMessages(library(GGally))
suppressMessages(library(ggplot2))
ggplot(modelsqm, aes(.fitted, .stdresid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "blue", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Standardized residuals",
       title = "Fitted values vs. standardized residuals",
       subtitle = deparse(modelsqm$call))

#Den siste modellen ser ut til å passe modellen best. Ser på løsningsforslag.

#Oppgave 7) Gjør prediksjoner på testsett
test$pred = predict(model, test[,-(1:2)])

# Evaluerer prediksjonene mot faktiske verdier
#cor(test_data$pred, testw_data$expenses)
#plot(x=test_data$pred, y=test_data$expenses)


#Oppgave 8) Regn ut MSE for model
mse = mean((test$rent-predict(model, test[,-(1:2)]))**2)

#Oppgave 9) Bruk modelsqm til å lage prediksjoner for testsettet
test$pred = predict(modelsqm,test[,-(1:2)])


#Oppgave 10) Regn ut MSE for model
mse = mean((test$rentsqm-predict(modelsqm, test[,-(1:2)]))**2)



