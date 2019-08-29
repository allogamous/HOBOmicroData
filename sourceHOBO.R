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

weatherReady = function(dataset,stationID="station"){
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
  final$stationID = stationID 
  
  return(dlply(final,.(DayTime),mutate,final))
}

#'---------------------------------------------------------------------------------------#'
#' Abaixo segue um passo-a-passo para processar os dados                                 #'
#' Duvidas procure o autor ou atual responsavel                                          #'
#'---------------------------------------------------------------------------------------#'

#' STEP BY STEP
