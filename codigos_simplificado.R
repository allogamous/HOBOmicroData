
#'########################################################################################
#'                                                                                       #'
#'                                                                                       #'
#' Codes para processamento de dados meteorologicos a partir da HOBOWARE                 #'
#' Autor: Germano                                                                        #'
#' Versao: 2.0 (Agosto de 2019, versao anterior de Abril de 2019)                        #'
#'                                                                                       #'
#'  Dados oriundos do processamento dos files binarios:                                  #'
#'  Anhumas_end.dtf e usp_end.dtf                                                        #'
#'                                                                                       #'
#'                                                                                       #'
#'########################################################################################'

## Leitura
###################################################################################
source("https://raw.githubusercontent.com/allogamous/HOBOmicroData/master/sourceHOBO.R")

# --------------------------ANHEMBI 2019 --------------------------
var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
anh = read.HOBO(file ="Anhumas_end_0.csv",
                start=NULL, # ja ta comecando no dia que eu quero
                end = "17/06/2019",
                id.vars = var.id,path=dir,save=T);


# --------------------------USP 2019 --------------------------

var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
usp = read.HOBO(file ="usp_end_0.csv",
                start=NULL, # ja ta comecando no dia que eu quero
                id.vars = var.id,path=dir,save=T);

#'------------------------------------------------------------------------------

# vejam alguns sensores, por exemplo, radiacao:
boxplot(usp$PAR[usp$DayTime %in% "night"])
# a noite a radiacao tem que ser 0

# em umidade, ha valroes negativos. Nao existe umidade negativa
boxplot(usp$SWC)

boxplot(usp$RAIN) 
# chuveu mais de 30mm numa hora...estranho? sim, mas nesse caso nos lembramos do dia! Choveu memso!

# Pensando nesses aspectos, criei uma funcao para controle de qualidade.
# porem acho melhor cada um fazer "na mao" com base nos proprios criterios
# minhas sugestões: 
# a) deixar PAR noturno = 0

usp$PAR[usp$DayTime %in% "night"] = 0
anh$PAR[anh$DayTime %in% "night"] = 0

# b) botar NA em quaisquer valores excessivos
summary(usp) # checando, ok
summary(anh) # checando, ok

# c) botar NA em valores de umidade do solo negativos (sao erros do sensor)
usp$SWC[usp$SWC < 0] = NA
anh$SWC[anh$SWC < 0] = NA


###################################################################################
## Processamento
###################################################################################


## Usp
var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");

usp2 = process.HOBO(dataset = usp,var.id = var.id,target = T);

usp2 = ldply(usp2) # transformando num dataset
head(usp2$day)

usp2 = weatherReady(dataset = usp2,stationID = "USP19")
usp2$all # media do dia e da noite
usp2$day # media apenas do dia
usp2$night # media apenas do noite

head(usp2$all)

# ou:

weatherReady(dataset = ldply(process.HOBO(dataset = usp,var.id = var.id,target = T)),stationID = "USP19");

save(lisdt=usp2, file="Weather-data-usp-2019.Rdata")


## Anhembi

var.id = c("PAR","SWC","STEMP","RAIN","ATEMP","RH","DEWP");
anh2 = process.HOBO(dataset = anh,var.id = var.id,target = T);
head(anh2)
anh2 = ldply(anh2) # transformando num dataset
head(anh2$day)

anh2 = weatherReady(dataset = anh2,stationID = "ANH19")
anh2$all # media do dia e da noite
anh2$day # media apenas do dia
anh2$night # media apenas do noite
head(anh2$all)

# ou

weatherReady(dataset = ldply(process.HOBO(dataset = anh,var.id = var.id,target = T)),stationID = "ANH19");

save(lisdt=anh2, file="Weather-data-anh-2019.Rdata")


############################# end
