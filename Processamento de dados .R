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
                                       

#---------------------- Source Codes ----------------------------------------------------

#' Funcao para leitura e processamento inicial de arquivos csv exportados do HOBOWARE

read.HOBO = function(file,                #' nome do arquivo (ex: dados.csv)
                     sep =",",            #' dados com colunas separadas por ,
                     skip = 1,            #' pula a primeira linha que tem informacoes
                     row.names = 1,       #' ignora a 1 coluna como rowname
                     pattern = "H",       #' padrao para cortar a coluna de hora (pode ser h tb)
                     id.vars = NULL,      #' nome das colunas de variaveis climaticas e de solo
                     path = NULL,         #' diretorio onde estao os arquivos .csv
                     daynight = TRUE,     #' TRUE ou FALSE se ira criar coluna separando dia da noite
                     save     = FALSE,    #' opcao que define se ira ou nao salvar o output
                     start = NULL,        #' inicio da janela (ex: 02-03-2019, para 2 de marco de 2019)
                     end   = NULL,        #' fim da janela (idem ao start)
                     formatDay = "%d/%m/%y" # formato de entrada do dia
                     ){
  #' -------------------------------------------------------------------------------------
  #' Pacotes necessarios
  require(reshape2);
  require(plyr);
  #' -------------------------------------------------------------------------------------
  #' Organizando diretorios de saida

    if(is.null(path)){path=getwd()};
      DF = read.csv(as.character(paste0(path,"/",file)),
                sep=sep,skip=skip,row.names = row.names);
  #' -------------------------------------------------------------------------------------
  #' Organizando colunas de data e hora

      DF = data.frame(colsplit(as.character(DF[,2]),pattern,c("Hora","Minutos")),DF[,-2]);
      DF$Data = as.Date(as.POSIXlt(as.character(DF$Data), format=formatDay))
      print("Converting date to year-month-day as R default")
  #' -------------------------------------------------------------------------------------
  #' Renomendo variaveis (caso seja necessario)
  
      if(!is.null(id.vars)){
        colnames(DF)[-c(1:3)] = var.id; # obs: estou ignorando as 3 colunas iniciais (data etc)
      }
  #' -------------------------------------------------------------------------------------
  #' Criando coluna que separa dia de noite
    
      if(isTRUE(daynight)){
        print("daynight assigned as TRUE: separating day from night");
        DF$DayTime = "day"
        DF$DayTime[DF$Hora %in% c(seq(from=19, to=23,by=1),seq(from=0, to=6,by=1))] = "night"
      }
  #' -------------------------------------------------------------------------------------
  #' Ajustando a janela de coleta de dados (vai ignorar caso nao tenha especificado)
      
      if(!is.null(start)){
        start = as.Date(as.POSIXlt(as.character(start), format="%d/%m/%y"))
        DF = DF[DF$Data >=start,]
      }
      if(!is.null(end)){
        end = as.Date(as.POSIXlt(as.character(end), format="%d/%m/%y"))
        DF = DF[DF$Data <= end,]
      }
      if(isTRUE(save)){
        output = as.character(paste0(gsub(x = file,pattern = ".csv",replacement = ""),"_processed.csv"))
        write.csv(x = DF,file = output,row.names = F)
        print(paste0("save assigned as TRUE: saving processed output file at ",dir))
      }
  #' -------------------------------------------------------------------------------------
  #' Informacao sobre output
   
      print(paste0("Returning a data.frame with ",nrow(DF)," rows and ",ncol(DF)," columns"))
   
      
      return(DF)
 #' -------------------------------------------------------------------------------------
}

# funcao para processamento inicial dos dados em escala diaria

process.HOBO = function(dataset,
                        target=FALSE,       #' TRUE usa so os targets definidos na collect
                        var.id,
                        collect = c(6,9,12,15),#' horario das coletas para media diaria
                        nightConsider = TRUE #' considera a noite toda para coleta de dados (coloque FALSE)
                        ){
  
  dataset$Collect = NA
  dataset$Collect[dataset$Hora %in% collect] = "TARGET"
  if(isTRUE(nightConsider)){
    dataset$Collect[dataset$DayTime %in% "night"] = "TARGET"}
    
  t = melt(dataset,id.vars=colnames(dataset)[!names(dataset) %in% var.id]);
  if(isTRUE(target)){t=t[t$Collect %in% "TARGET",]}
  t = dlply(ddply(t,.(DayTime,Data,variable),summarise,
            min.value = quantile(value,na.rm=T)[1],
            quant25   = quantile(value,na.rm=T)[2],
            median.value = median(value,na.rm=T),
            quant75    = quantile(value,na.rm=T)[4],
            max.value  = quantile(value,na.rm=T)[5],
            mean.value = mean(value,na.rm=T),
            sd.value   = sd(value,na.rm=T),
            cv.value   = round(sd.value/mean.value,4),
            soma.value=sum(value,na.rm=T)),.(DayTime))
  return(t)
}


# funcao para gerar arquivos para exportacao

weatherReady = function(dataset){
        p2=dcast(dataset, DayTime+Data~variable, value.var = "soma.value")
        head(p2)
        
        p2 = p2[,c(1:3,6)]
        
        p2=rbind(p2,data.frame(DayTime="all",ddply(p2,.(Data),summarise, PAR = sum(PAR),RAIN=sum(RAIN))))
        names(p2)[-c(1:2)] = c("PAR_cum","RAIN_cum")
        
        p3 = dcast(dataset, DayTime+Data~variable, value.var = "mean.value")
        p3 = p3[,c(1:5,7:9)]
        colnames(p3)[-c(1:2)] = c("PAR_mean","ST_mean","SWC_mean","T_mean","RH_mean","DEW_mean");
        p3=rbind(p3,data.frame(DayTime="all",ddply(p3,.(Data),summarise,
                                                   PAR_mean   = mean(PAR_mean,na.rm=T),
                                                   ST_mean    = mean(ST_mean,na.rm=T),
                                                   SWC_mean   = mean(SWC_mean,na.rm=T),
                                                   T_mean     = mean(T_mean,na.rm=T),
                                                   RH_mean    = mean(RH_mean,na.rm=T),
                                                   DEW_mean   = mean(DEW_mean,na.rm=T))));
        
        p3$SRad = p3$PAR_mean/(2.1*4.6*10) # convering to MJm2
        
        p4 = dcast(dataset, DayTime+Data~variable, value.var = "min.value")
        p4 = p4[,c(1,2,4:5,7:9)]
        colnames(p4)[-c(1:2)] = c("ST_min","SWC_min","T_min","RH_min","DEW_min")
        p4=rbind(p4,data.frame(DayTime="all",ddply(p4,.(Data),summarise, 
                                                   ST_min    = min(ST_min ,na.rm=T),
                                                   SWC_min    = min(SWC_min ,na.rm=T),
                                                   T_min      = min(T_min ,na.rm=T),
                                                   RH_min     = min(RH_min ,na.rm=T),
                                                   DEW_min    = min(DEW_min,na.rm=T))))                                           
        
        
        p5 = dcast(dataset, DayTime+Data~variable, value.var = "max.value")
        p5 = p5[,c(1,2,4:5,7:9)]
        colnames(p5)[-c(1:2)] = c("ST_max","SWC_max","T_max","RH_max","DEW_max")
        p5=rbind(p5,data.frame(DayTime="all",ddply(p5,.(Data),summarise, 
                                                   ST_max    = max(ST_max ,na.rm=T),
                                                   SWC_max    = max(SWC_max ,na.rm=T),
                                                   T_max      = max(T_max ,na.rm=T),
                                                   RH_max     = max(RH_max ,na.rm=T),
                                                   DEW_max    = max(DEW_max,na.rm=T)))) 
        
        final = merge(merge(merge(p2,p3,by=c("DayTime","Data")),
                            p4,by=c("DayTime","Data")),p5,by=c("DayTime","Data"))
        rm(p2,p3,p4,p5)
  
  return(dlply(final,.(DayTime),mutate,final))
}

#'---------------------------------------------------------------------------------------#'
#' Abaixo segue um passo-a-passo para processar os dados                                 #'
#' Duvidas procure o autor ou atual responsavel                                          #'
#'---------------------------------------------------------------------------------------#'

#' STEP BY STEP

#------- STEP 1: No Software HOBOWARE ----------------------------------------------------                                                

#'  1.1 Abra o arquivo X_end.dft pelo software HOBOWARE (double-click no arquivo)
#'  #'----------------------------------------------------------------
#'  #'  OBS: 
#'  dentro do HOBOWARE, confira em PREFERENCIAS > Geral como estao configuradas
#'  as notacoes de hora (formato de hora ideal = 24h) e data (DMA - dia mes ano, ideal)
#'  #'----------------------------------------------------------------
#'  OBS2: 
#'  na hora de importar o arquivo pro HOBOWARE, certifique-se de que esta usando o
#'  defasamento a partir de GMT -3. Isso evitara problemas de mudança de horario (de verao)
#'  e ira padronizar a coleta dos dados com base no tempo solar (e nao do pais)
#'  #'----------------------------------------------------------------
#'  # OBS3 
#'  ainda em preferencias, veja se a opcao "separar uma coluna para data e
#'  oura para hora" esta marcada. isso ira faciltar sua vida ;D
#'  #'----------------------------------------------------------------
#'  1.2 Uma planilha com os dados em escala temporal (ex: a cada 1h, por dia) serao mostrados
#'  1.3 Confira se as datas estao corretas
#'  1.4 Para exportar, pressione CTRL E ou va em Ficheiro > Exportar dados da Tabela
#'      e selecione quais variaveis quer exportar (default = todas)
#'  1.5 Um arquivo .csv (ou txt) sera gerado.
#'  1.6 No excel, organize a coluna de data e hora
#'  1.7 salve o arquivo para ser utilizado no passo 2
#'  

# ------- STEP 2: Importacao do arquivo .csv----------------------------------------------

require(reshape2);
require(plyr);
dir = getwd() # saving current directory


#' PAR  : radicacao fotossinteticamente ativa
#' STEMP: temperatura do solo
#' SWC  : umidade do solo
#' RAIN : chuva
#' ATEMP: temperatura do ar
#' RH   : umidade do ar
#' DEWP : temperatura no ponto de orvalho
#' 
#' exemplos:
#' 

## exemplo: o arquivo esta em formato diferente do esperado.
# o que fazer? so adaptar o script!
usp1 = read.HOBO(file ="usp_end_01.csv",path = dir,skip=0,
                formatDay = "%m/%d/%y",pattern = "h",sep=",");
# mudei o formato da data
# botei skip = 0 (ou seja, nao pule nenhuma linha)
# mudei o tipo de pattern (default - H, botei h)
# veja que o arquivo ta dando erro em definir dia e noite
# isso porque no HOBOWARE nao foi configurado a hora para 24h (de 0h ate 23h)
# nesse ponto, nao tem como arrumar o problema! tem que voltar no HOBOWARE e
# e gerar novamente o .csv a partir do metadado binario

head(usp1)
# importa sem mudar nome de colunas
usp = read.HOBO(file ="usp_end_0.csv",path = dir); 
# agora adicionando novos argumentos, importa e muda nome de coluna

# crie um vetor com o nome das colunas que voce quer trabalhar
# fique esperto pois as colunas serao organizadas com base na ordem dos canais
# nos quais os sensores foram plugados no HOBOWARE

var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
usp = read.HOBO(file ="usp_end_0.csv",id.vars = var.id,path=dir);
# com o argumento save=T, salva o output no seu diretorio
usp = read.HOBO(file ="usp_end_0.csv",id.vars = var.id,path=dir,save=T);
head(usp)

# teste o mesmo para o anhembi
var.id = c("PAR","SWC","STEMP","RAIN","ATEMP","RH","DEWP");
anh = read.HOBO(file ="Anhumas_end_0.csv",path = dir);
anh = read.HOBO(file ="Anhumas_end_0.csv",id.vars = var.id,path=dir,save=T);
head(anh);

# conseguimos tambem inserir um argumento que ja corta a planilha com base na janela de interesse
# para isso, basta usar os argumentos start e/ou end para definir as datas de inicio e fim
## no anhembi a estacao foi ligada no dia do plantio
var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
anh = read.HOBO(file ="Anhumas_end_0.csv",
                start=NULL, # ja ta comecando no dia que eu quero
                end = "17/06/2019",
                id.vars = var.id,path=dir,save=T);
tail(anh)
## na usp eu restartei no dia do plantio (dia 22/02) e retiramos antes da colheita
var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
usp = read.HOBO(file ="usp_end_0.csv",
                start=NULL, # ja ta comecando no dia que eu quero
                id.vars = var.id,path=dir,save=T);

head(usp)
head(anh)
## OBS
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


# ------- STEP 3: Processamento das informacoes ----------------------------------------------

#' os arquivos lidos estao na janela de tempo entre ativacao/desativacao do equipamento em campo
#' Portanto, precisamos cortar a janela de tempo que e do nosso interesse
#' Em seguida, verificar possiveis outliers
#' Por fim, gerar informacoes ambientais uteis

head(usp)
var.id = c("PAR","STEMP","SWC","RAIN","ATEMP","RH","DEWP");
# com target = F, faz as sumarizacoes sem pegar horas especificas do dia
usp2 = process.HOBO(dataset = usp,var.id = var.id,target = F)
usp2$day   # medias diarias do periodo diurno
usp2$night # medias diarias do periodo noturno

usp2 = process.HOBO(dataset = usp,var.id = var.id,target = T)
usp2$day  # dia usando apenas o collect (default = c(9,12,15))
usp2$night # noite usando a media de TODO o periodo noturno

# se colocar nightConsider = F, so considera os collect ou o periodo diurno
usp2 = process.HOBO(dataset = usp,var.id = var.id,target = T,nightConsider = F)
head(usp2$day)

var.id = c("PAR","SWC","STEMP","RAIN","ATEMP","RH","DEWP");
anh2 = process.HOBO(dataset = anh,var.id = var.id,target = T)
anh2$day  # dia usando apenas o collect (default = c(9,12,15))
anh2$night # noite usando a media de TODO o periodo noturno


## os dados nesse formato sao muito uteis para estudos de envirotyping!!
# O passo 4 visara apenas formar um dataset para disponibilziacao e outras aplicacoes

# ------- STEP 4: Formacao Banco de dados ----------------------------------------------

#' ao formar um banco de dados meteorologicos, temos interesse em:
#' Temperatura Media diaria
#' Temperatura Maxima diaria
#' Temperatura Minima Diaria

var.id = c("PAR","SWC","STEMP","RAIN","ATEMP","RH","DEWP");

usp2 = process.HOBO(dataset = usp,var.id = var.id,target = T);
head(usp2)
usp2 = ldply(usp2) # transformando num dataset
head(usp2$day)

usp2 = weatherReady(dataset = usp2)
usp2$all # media do dia e da noite
usp2$day # media apenas do dia
usp2$night # media apenas do noite

save(lisdt=usp2, file="Weather-data-usp-2019.Rdata")


anh2 = process.HOBO(dataset = anh,var.id = var.id,target = T);
head(anh2)
anh2 = ldply(anh2) # transformando num dataset
head(anh2$day)

anh2 = weatherReady(dataset = anh2)
anh2$all # media do dia e da noite
anh2$day # media apenas do dia
anh2$night # media apenas do noite

save(lisdt=anh2, file="Weather-data-anh-2019.Rdata")

# opcoes de operation:


# opcoes de operation:
#' operation = "mean.value" retorna as medias diarias
#' operation = "soma.value" retorna somatorio dos dias
#' operation = "median.value" retorna as medianas do dia
#' operation = "max.value" retorna as maximas do dia
#' operation = "min.value" retorna as minimas do dia
#' operation = "quant25" retorna percentil 25 do dia
#' operation = "quant75" retorna percentil 75 do dia
#' operation = "full" retorna o default da funcao, ajustando retorno de acordo com os dados






p = weatherReady(dataset = l)

save()
