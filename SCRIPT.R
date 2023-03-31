### Carregando Pacotes
{
  setwd("C:/Users/pedro/Desktop/UFU/SEMESTRE 2022-2/ML")
  require(caret)
  library(lubridate)
  require(zoo)
  require(forecast)
  require(dplyr)
  require(Amelia)
  require(ggplot2)
  require(corrplot)
}

###Carregando e Configurando Dataset
{
  setwd("C:/Users/pedro/Desktop/UFU/SEMESTRE 2022-2/ML")
  data = read.csv(file = "Brazil League.csv",sep = ";",header = TRUE, stringsAsFactors = TRUE, dec = ",",encoding = "latin1")
  dim(data)
  colnames(data)
  str(data)
  head(data,5)
  data$Date = as.Date(data$Date)

  "Criando variável resposta em relação ao time da Casa, se ele venceu, empatou ou perdeu"
  result_H = c()
  for(i in 1:nrow(data)){
    if(data$FT_Goals_H[i] > data$FT_Goals_A[i]){
      result_H[i] = 'W'
    } else if(data$FT_Goals_H[i] < data$FT_Goals_A[i]){
      result_H[i] = 'L'
    } else {
      result_H[i] = 'D'
    }
  }
  data$result_H = as.factor(result_H)
  
  "Criando variável que aponta o palpite do resultado da casa de apostas, com base apenas nas odds"
  result_bet = c()
  for(i in 1:nrow(data)){
    if(data$FT_Odds_H[i] < data$FT_Odds_A[i] && data$FT_Odds_H[i] < data$FT_Odds_D[i]){
      result_bet[i] = 'W'
    } else if(data$FT_Odds_H[i] > data$FT_Odds_A[i] && data$FT_Odds_A[i] < data$FT_Odds_D[i]){
      result_bet[i] = 'L'
    } else {
      result_bet[i] = 'D'
    }
    
  }
  data$result_bet = as.factor(result_bet)
  
  "Removendo dados onde há odds nulas ou negativas"
  nulos = data[ifelse(data$FT_Odds_H > 0 & data$FT_Odds_A > 0 & data$FT_Odds_D > 0,FALSE,TRUE),]
  data = data[ifelse(data$FT_Odds_H > 0 & data$FT_Odds_A > 0 & data$FT_Odds_D > 0,TRUE,FALSE),]
  
  "Filtrando dados apenas da Série A do Campeonato Brasileiro"
  data = data[data$League == "Brazil Serie A",]
  
  summary(data)
}

#Selecionando Variáveis para Iniciar Criação de Features, Visualizações e Modelagem
colnames(data)
data = data[,c("Season"  ,         "Home"   ,          "Away"       ,      "FT_Odds_H"     ,   "FT_Odds_D"       , "FT_Odds_A"      ,  "FT_Goals_H"   ,    "FT_Goals_A"   ,   
                "Date"         ,    "League"        ,  "result_H" ,  "result_bet","XG_Home_Pre", "XG_Away_Pre" )]

#Criação de Features
{
  "Construindo Features Relacionadas ao Histórico de Confrontos ao Longo do Tempo"
  {
    historico = data %>%
      group_by(Home,Away,Date) %>%
      summarise(
        Matchs = n(),
        Winners_Home = sum(ifelse(result_H == 'W',1,0)),
        Draws_Home = sum(ifelse(result_H == 'D',1,0)),
        Loses_Home = sum(ifelse(result_H == 'L',1,0)))
    
    times = as.character(unique(historico$Home))
    subset2 = data.frame()
    for(i in 1:length(times)){
      subset = historico[historico$Home == times[i],]
      subset = arrange(data.frame(subset),Date)
      
      times_visitantes = as.character(unique(subset$Away))
      for(j in 1:length(times_visitantes)){
        subset0 = subset[subset$Away == times_visitantes[j],]
        subset0$Matchs_Time = append(0,cumsum(subset0$Matchs[-length(subset0$Matchs)]))
        subset0$Winners_Home_Time = append(0,cumsum(subset0$Winners_Home[-length(subset0$Winners_Home)]))
        subset0$Draws_Home_Time = append(0,cumsum(subset0$Draws_Home[-length(subset0$Draws_Home)]))
        subset0$Loses_Home_Time = append(0,cumsum(subset0$Loses_Home[-length(subset0$Loses_Home)]))
        subset2 = rbind(subset2,subset0)
      }
      
    }
    historico = subset2
    
    colnames(historico)
    data = merge(data,historico[,c('Home','Away','Date','Matchs_Time','Winners_Home_Time','Draws_Home_Time','Loses_Home_Time')],
                 by = c('Home','Away','Date'),all.x = TRUE)
  }
  
  "Criar variáveis com relação a jogos anteriores, pra captar melhor o histórico atual dos times dentro do campeonato" 
  {
    "Variável que capta o equilíbrio/desequilíbrio das partidas em relação as odds"
    cv_odds = c()
    for(i in 1:nrow(data)){
      cv_odds[i] = sd(c(data$FT_Odds_H[i],data$FT_Odds_D[i],data$FT_Odds_A[i]))/mean(c(data$FT_Odds_H[i],data$FT_Odds_D[i],data$FT_Odds_A[i]))
    }
    data$cv_odds = cv_odds
    
    data$DATA = as.Date(data$Date)
    nacoes = unique(data$Home)
    subset2 = data.frame()
    for(i in 1:length(nacoes)){
      nacao = nacoes[i]
      subset = data[data$Home == as.character(nacao),]
      subset= arrange(subset,DATA)
      last_FT_Odds_H = c()
      last_FT_Odds_A = c()
      last_FT_Odds_D = c()
      last_FT_Goals_H = c()
      last_FT_Goals_A = c()
      for(j in (1:length(subset$Home))){
        if(j==1){
          last_FT_Odds_H[j] = NA
          last_FT_Odds_A[j] = NA
          last_FT_Odds_D[j] = NA 
          last_FT_Goals_H[j] = NA
          last_FT_Goals_A[j] = NA 
        } else {
          last_FT_Odds_H[j] = subset$FT_Odds_H[j-1]
          last_FT_Odds_D[j] = subset$FT_Odds_D[j-1]
          last_FT_Odds_A[j] = subset$FT_Odds_A[j-1]
          last_FT_Goals_H[j] = subset$FT_Goals_H[j-1]
          last_FT_Goals_A[j] = subset$FT_Goals_A[j-1]
          
        }
      }
      subset$last_FT_Odds_H = last_FT_Odds_H
      subset$last_FT_Odds_D = last_FT_Odds_D
      subset$last_FT_Odds_A = last_FT_Odds_A
      subset$last_FT_Goals_H = last_FT_Goals_H
      subset$last_FT_Goals_A = last_FT_Goals_A
      
      subset2 = rbind(subset2,subset)
    }
    
    subset2 = na.omit(subset2)
    subsethome = subset2
    
    
    nacoes = unique(data$Away)
    subset2 = data.frame()
    for(i in 1:length(nacoes)){
      nacao = nacoes[i]
      subset = data[data$Away == as.character(nacao),]
      subset= arrange(subset,DATA)
      last_FT_Odds_H_away = c()
      last_FT_Odds_A_away = c()
      last_FT_Odds_D_away = c()
      last_FT_Goals_H_away = c()
      last_FT_Goals_A_away = c()
      for(j in (1:length(subset$Away))){
        if(j==1){
          last_FT_Odds_H_away[j] = NA
          last_FT_Odds_A_away[j] = NA
          last_FT_Odds_D_away[j] = NA 
          last_FT_Goals_H_away[j] = NA
          last_FT_Goals_A_away[j] = NA 
          
        } else {
          last_FT_Odds_H_away[j] = subset$FT_Odds_H[j-1]
          last_FT_Odds_D_away[j] = subset$FT_Odds_D[j-1]
          last_FT_Odds_A_away[j] = subset$FT_Odds_A[j-1]
          last_FT_Goals_H_away[j] = subset$FT_Goals_H[j-1]
          last_FT_Goals_A_away[j] = subset$FT_Goals_A[j-1]
        }
      }
      subset$last_FT_Odds_H_away = last_FT_Odds_H_away
      subset$last_FT_Odds_D_away = last_FT_Odds_D_away
      subset$last_FT_Odds_A_away = last_FT_Odds_A_away
      subset$last_FT_Goals_H_away = last_FT_Goals_H_away
      subset$last_FT_Goals_A_away = last_FT_Goals_A_away
      
      subset2 = rbind(subset2,subset)
    }
    
    subset2 = na.omit(subset2)
    subsetaway = subset2
    
    
    colnames(subsethome)
    colnames(subsetaway)
    
    data = merge(subsethome,subsetaway[,c('Home','Away','Date',"last_FT_Odds_H_away",  "last_FT_Odds_D_away",  "last_FT_Odds_A_away" , "last_FT_Goals_H_away", "last_FT_Goals_A_away")],by=c('Home','Away','Date'),all.x = TRUE)
    data = na.omit(data)
    
    last_pts_H = c()
    last_pts_A = c()
    for(i in 1:nrow(data)){
      
      if(data$last_FT_Goals_H[i] > data$last_FT_Goals_A[i]){
        last_pts_H[i] = 3
      } else if(data$last_FT_Goals_H[i] < data$last_FT_Goals_A[i]){
        last_pts_H[i] = 0
      } else{
        last_pts_H[i] = 1
      }
      
    }
    
    
    for(i in 1:nrow(data)){
      
      if(data$last_FT_Goals_H_away[i] > data$last_FT_Goals_A_away[i]){
        last_pts_A[i] = 0
      } else if(data$last_FT_Goals_H_away[i] < data$last_FT_Goals_A_away[i]){
        last_pts_A[i] = 3
      } else{
        last_pts_A[i] = 1
      }
      
    }
    
    data$last_pts_H = last_pts_H
    data$last_pts_A = last_pts_A
  }
  "Adicionando médias móveis referente a diferença de gols e pontuação com relação ao time da Casa"
  {
    data$dif_gols = data$last_FT_Goals_H-data$last_FT_Goals_A
    data$DATA = as.Date(data$Date)
    nacoes = unique(data$Home)
    subset2 = data.frame()
    for(i in 1:length(nacoes)){
      nacao = nacoes[i]
      subset = data[data$Home == as.character(nacao),]
      subset= arrange(subset,DATA)
      jogos4 = 3
      jogos = 5
      subset$gols_casa_mm_3 = rollmean(subset$dif_gols,k=jogos4,align = 'right',na.pad = TRUE)
      subset$gols_casa_mm_5 = rollmean(subset$dif_gols,k=jogos,align = 'right',na.pad = TRUE)
      subset$forme_H_5 = rollmean(subset$last_pts_H,k=jogos,align = 'right',na.pad = TRUE)
      subset$forme_H_3 = rollmean(subset$last_pts_H,k=jogos4,align = 'right',na.pad = TRUE)
      
      
      subset2 = rbind(subset2,subset)
    }
    subset2 = na.omit(subset2)
    #data = subset2
    subsethome = subset2
  }
  "Adicionando médias móveis referente a diferença de gols e pontuação com relação ao time de Fora"
  {
    data$DATA = as.Date(data$Date)
    nacoes = unique(data$Away)
    subset2 = data.frame()
    for(i in 1:length(nacoes)){
      nacao = nacoes[i]
      subset = data[data$Away == as.character(nacao),]
      subset= arrange(subset,DATA)
      subset$dif_gols_away = subset$last_FT_Goals_H_away-subset$last_FT_Goals_A_away
      jogos4 = 3
      jogos = 5
      subset$gols_fora_mm_3 = rollmean(subset$dif_gols_away,k=jogos4,align = 'right',na.pad = TRUE)
      subset$gols_fora_mm_5 = rollmean(subset$dif_gols_away,k=jogos,align = 'right',na.pad = TRUE)
      subset$forme_A_5 = rollmean(subset$last_pts_A,k=jogos,align = 'right',na.pad = TRUE)
      subset$forme_A_3 = rollmean(subset$last_pts_A,k=jogos4,align = 'right',na.pad = TRUE)
      
      
      subset2 = rbind(subset2,subset)
    }
    subset2 = na.omit(subset2)
    subsetaway = subset2
    #data = subset2
  }
  #juntar dfs
  data = merge(subsethome,subsetaway[,c('Home','Away','Date','dif_gols_away','gols_fora_mm_3','gols_fora_mm_5','forme_A_3','forme_A_5')],by = c('Home','Away','Date'),all.x=TRUE)
  data = na.omit(data)
  savedata = data
  
  antes_ncol = ncol(data)
  "Variáveis Dummies/Binárias para os times Mandantes e Visitantes"
  {
    copa = data.frame(NATIONS = unique(append(data$Home,data$Away)))
    for(i in 1:length(copa$NATIONS)){
      Team = as.character(copa$NATIONS[i])
      data[paste(Team,'Casa')] = ifelse(data$Home == Team,1,0)
      data[paste(Team,'Fora')] = ifelse(data$Away == Team,1,0)
    }
    savedata = data
    
    dummies = colnames(data)[(antes_ncol+1):ncol(data)]
    dummies
  }
  
  

    
    data = na.omit(data)
    savedata = data
  
}

#Verificando e Tratando Dados Nulos ou Inconsistentes
{
ggplot(data = data, mapping = aes(x = XG_Home_Pre))+
  geom_histogram(bins = 50,fill = "green",colour = "black")+
  facet_wrap(~Season)+
  ggtitle("Distribuição da Expectativa de Gols dos Times Mandantes por Temporada")

"Verificar a possibilidade de acrescentar xg na modelagem , porém já percebi que 
muitas observações podem ser excluídas por conta disso, uma vez que há variáveis
de xg que são nulas, principalmente de temporadas antigas."
"O xG nada mais é que a expectativa de gols de uma equipe, e a abordagem que vou
utilizar para preencher esses dados nulos será subsituí-los pela média do time de
cada temporada e caso tenha alguma temporada em que a média é zero, esta será
subsituída apenas pela média do time, desconsiderando a temporada."
"Uma abordagem melhor será separar os dados que contem dados nulos de XG, para criar um modelo com objetivo
de preencher os dados para não perder tantas informações, medida que é mais efetiva que as consideradas anteriormente."
dataxg = data[data$XG_Home_Pre == 0 & data$XG_Away_Pre == 0,]
data = data[data$XG_Home_Pre != 0 & data$XG_Away_Pre != 0,]
savedata = data
#Modelando XG
{
set.seed(10)
index = createDataPartition(y = data$XG_Home_Pre, times = 1, p = 0.7, list = FALSE)
treino = data[index,]
teste = data[-index,]
trainControl = trainControl(method = 'repeatedcv',number = 5, repeats=2)
sel_var = append(dummies,c("FT_Odds_H","FT_Odds_D","FT_Odds_A","cv_odds","gols_casa_mm_3","gols_fora_mm_3","gols_casa_mm_5","gols_fora_mm_5","forme_H_3","forme_A_3","XG_Home_Pre","Season"))
set.seed(10)
modeloxghome = train(XG_Home_Pre~.,treino[,sel_var],trControl = trainControl, method = 'glmnet',metric = "MAE",preProc = c('center','scale'))
predictions = predict(modeloxghome,teste)
mean(abs(predictions-teste$XG_Home_Pre)) 

sel_var = append(dummies,c("FT_Odds_H","FT_Odds_D","FT_Odds_A","cv_odds","gols_casa_mm_3","gols_fora_mm_3","gols_casa_mm_5","gols_fora_mm_5","forme_H_3","forme_A_3","XG_Away_Pre","Season"))
set.seed(10)
modeloxgaway = train(XG_Away_Pre~.,treino[,sel_var],trControl = trainControl, method = 'glmnet',metric = "MAE",preProc = c('center','scale'))
predictions = predict(modeloxgaway,teste)
mean(abs(predictions-teste$XG_Away_Pre)) 

#Preenchendo dados de xg
dataxg$XG_Home_Pre = predict(modeloxghome,dataxg)
dataxg$XG_Away_Pre = predict(modeloxgaway,dataxg)
data = rbind(data,dataxg)
savedata = data
}
"Criando algumas variáveis derivadas"
{
  data$var1 = data$gols_casa_mm_3-data$gols_fora_mm_3
  data$var2 = data$gols_casa_mm_5-data$gols_fora_mm_5
  data$var3 = data$XG_Home_Pre/data$XG_Away_Pre
  data$var4 = data$forme_H_5-data$forme_A_5
  data$var5 = data$forme_H_3-data$forme_A_3
  data$var6 = data$Winners_Home_Time-data$Loses_Home_Time
  
  savedata = data
}
}

#Explorando os Dados
{
  summary(data)
  
  corrplot(corr = cor(data[,c('FT_Odds_H','FT_Odds_D','FT_Odds_A','FT_Goals_H','FT_Goals_A','XG_Home_Pre','XG_Away_Pre',
                              'cv_odds','gols_casa_mm_3','forme_H_3','gols_fora_mm_3','forme_A_3',
                              'gols_casa_mm_5','forme_H_5','gols_fora_mm_5','forme_A_5')]),tl.cex = 0.75)
  
  "Em geral na etapa de modelagem, variáveis explicativas que estão correlacionadas podem não
ser tão eficientes juntas para a eficiência do modelo. Logo, uma boa prática é tentar retirar
algumas delas. Como podemos perceber, as features que foram criadas para captar o histórico recente dos times
utilizando médias móveis, estão sempre correlacionadas entre si, uma boa medida seria testar na etapa de modelagem
se a exclusão de algumas delas teria impacto na performance do modelo, e caso esse impacto não se altere ou seja negativo,
a manutenção dela seria a melhor alternativa. E isso vale para as outras também, mas em geral
poucas variáveis das selecionadas no gráfico estão correlacionadas entre si."
  
  
  ggplot(data = data, mapping = aes(x = FT_Odds_H))+
    geom_histogram(bins = 50,fill = "green",colour = "black")+
    ggtitle(  "Distribuição das Odds dos Times Mandantes")
  
  ggplot(data = data, mapping = aes(x = FT_Odds_A))+
    geom_histogram(bins = 50,fill = "green",colour = "black")+
    ggtitle(  "Distribuição das Odds dos Times Visitantes")
  
  ggplot(data = data, mapping = aes(x = FT_Odds_D))+
    geom_histogram(bins = 50,fill = "green",colour = "black")+
    ggtitle(  "Distribuição das Odds de Empates")
  
  ggplot(data = data, mapping = aes(x = XG_Home_Pre))+
    geom_histogram(bins = 50,fill = "green",colour = "black")+
    ggtitle(  "Distribuição da Expectativa de Gols dos Times Mandantes")
  

  ggplot(data = data, mapping = aes(x = XG_Away_Pre))+
    geom_histogram(bins = 50,fill = "green",colour = "black")+
    ggtitle(  "Distribuição da Expectativa de Gols dos Times Visitantes")
  
  ggplot(data = data, mapping = aes(x = result_H, fill = result_H))+
    geom_bar()+
    facet_wrap(~Home)+
    scale_fill_manual(values = c("gray","red","green"))+
    ggtitle(  "Resultados como Mandante por Time")
  
  ggplot(data = data, mapping = aes(x = result_H, fill = result_H))+
    geom_bar()+
    facet_wrap(~Away)+
    scale_fill_manual(values = c("gray","red","green"))+
    ggtitle(  "Resultados como Visitante por Time")
  
  ggplot(data = data, mapping = aes(x = XG_Home_Pre,fill = Home))+
    geom_histogram(bins = 30)+
    facet_wrap(~Home)+
    ggtitle(  "Histograma da Expectativa de Gols dos times mandantes")
  
  ggplot(data = data, mapping = aes(x = XG_Home_Pre,fill = Home))+
    geom_boxplot()+
    facet_wrap(~Home)+
    ggtitle(  "Boxplot da Expectativa de Gols dos times mandantes")
  
  ggplot(data = data, mapping = aes(x = FT_Goals_H,fill = Home))+
    geom_density()+
    facet_wrap(~Home)+
    ggtitle(  "Densidade dos Gols dos times mandantes")
  
  ggplot(data = data, mapping = aes(x = FT_Goals_H,fill = Home))+
    geom_boxplot()+
    facet_wrap(~Home)+
    ggtitle(  "Boxplot dos Gols dos times mandantes")
  
  ggplot(data = data, mapping = aes(y = FT_Goals_H, x = XG_Home_Pre,colour = Home))+
    geom_point()+
    facet_wrap(~Home)+
    ggtitle(  "Relação entre os gols esperados com os gols reais")
  
  ggplot(data = data, mapping = aes(y = dif_gols, x = FT_Odds_H,colour = Home))+
    geom_point()+
    facet_wrap(~Home)+
    ggtitle(  "Relação entre as Odds do Time mandante e o resultado da partida, pela diferença de gols")
  
  ggplot(data = data, mapping = aes(y = gols_casa_mm_5,x = Date,colour = Home))+
    geom_line()+
    facet_wrap(~Home)+
    ggtitle(  "Médias Móveis da diferença de gols dos últimos 5 jogos dos times mandantes")
  
  ggplot(data = data, mapping = aes(y = gols_fora_mm_5,x = Date,colour = Away))+
    geom_line()+
    facet_wrap(~Away)+
    ggtitle(  "Médias Móveis da diferença de gols dos últimos 5 jogos dos times visitantes")
  
  ggplot(data = data, mapping = aes(y = forme_H_5,x = Date,colour = Home))+
    geom_line()+
    facet_wrap(~Home)+
    ggtitle(  "Médias Móveis do retrospecto de pontos nos últimos 5 jogos dos times mandantes")
  
  ggplot(data = data, mapping = aes(y = forme_A_5,x = Date,colour = Away))+
    geom_line()+
    facet_wrap(~Away)+
    ggtitle(  "Médias Móveis do retrospecto de pontos nos últimos 5 jogos dos times visitantes")
}

teste2022 = data[data$Date >= '2022-01-01',]
data = data[data$Date < '2022-01-01',]

###MODELANDO
ligas = unique(savedata$League)
{
  cms = array(dim = c(3,3,length(ligas)))
  MAE = c()
  clas_stakes = c()
  clas_ROI = c()
  clas_jogos_bet = c()
  reg_stakes = c()
  reg_ROI = c()
  reg_jogos_bet = c()
  clas_stakes_l = c()
  clas_ROI_l = c()
  clas_jogos_bet_l = c()
  reg_stakes_l = c()
  reg_ROI_l = c()
  reg_jogos_bet_l = c()
  models_class = c('rf')
  models_reg = c('rf')
  for(i in 1:(length(ligas))){
    data = savedata[savedata$League == as.character(ligas[i]),]
    data = droplevels(data)
    if(nrow(data) != 0){
      set.seed(10)
      index = createDataPartition(y = data$result_H, times = 1, p = 0.7, list = FALSE)
      treino = data[index,]
      teste = data[-index,]
      trainControl = trainControl(method = 'repeatedcv',number = 5, repeats=2)
      
      
      #CLASSIFICATION
      
      modellist = list()
      metrica = c()
      sel_var = append(dummies,c("result_H","FT_Odds_H","FT_Odds_D","FT_Odds_A","cv_odds",
                                 "forme_H_5","gols_casa_mm_5","gols_fora_mm_5","forme_A_5",
                                 "forme_H_3","gols_casa_mm_3","gols_fora_mm_3","forme_A_3",
                                 "XG_Home_Pre","XG_Away_Pre","Season",
                                 "Winners_Home_Time","Draws_Home_Time","Loses_Home_Time",
                                 "var1",'var2','var3','var4','var5','var6'
                                 ))
      for(j in 1:length(models_class)){
        set.seed(10)
        fit.RF = train(result_H~.,treino[,sel_var],trControl = trainControl, method = models_class[j],metric = "Accuracy",preProc=c("center","scale")) #preProc=c("center","scale")
        saveRDS(fit.RF,paste('./modelo',as.character(ligas[i]),models_class[j],'.rds'))
        key = models_class[j]
        modellist[[key]] = fit.RF
        metrica [j] = as.numeric(confusionMatrix(table(predict(modellist[[j]],teste),teste$result_H))$byClass[3,5]) #precision w

        }
      
      fit.RF = modellist[[arrange(data.frame(modelos = models_class,metrica = metrica),desc(metrica))[1,]$modelos]]

      predictions = predict(fit.RF,teste, type = "raw")
      cms[,,i] = confusionMatrix(table(predictions,teste$result_H))$table
      saveRDS(fit.RF,paste('./modelo',as.character(ligas[i]),'.rds'))

      "Financeiro"
      stake = 1
      calc = teste[ifelse(predict(fit.RF,teste) == 'W',TRUE,FALSE),]
      calc$g_r = ifelse(calc$result_H == 'W','Green','Red')
      clas_stakes[i] = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))
      clas_ROI[i] = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))/(nrow(calc)*stake))*100
      clas_jogos_bet[i] = (nrow(calc)/nrow(teste))*100
      
      calc = teste[ifelse(predict(fit.RF,teste) == 'L',TRUE,FALSE),]
      calc$g_r = ifelse(calc$result_H == 'L','Green','Red')
      clas_stakes_l[i] = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_A-1),-stake))
      clas_ROI_l[i] = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_A-1),-stake))/(nrow(calc)*stake))*100
      clas_jogos_bet_l[i] = (nrow(calc)/nrow(teste))*100


      
      #REGRESSION
      
      set.seed(10)
      index = createDataPartition(y = data$dif_gols, times = 1, p = 0.7, list = FALSE)
      treino = data[index,]
      teste = data[-index,]
      
      modellist = list()
      metrica = c()
      sel_var = append(dummies,c("dif_gols","FT_Odds_H","FT_Odds_D","FT_Odds_A","cv_odds","gols_casa_mm_3","gols_fora_mm_3","forme_H_3","forme_A_3","XG_Home_Pre","XG_Away_Pre","Season",
                                 "var1",'var2','var3','var4','var5',"var6",
                                 "forme_H_5","gols_casa_mm_5","gols_fora_mm_5","forme_A_5",
                                 "Winners_Home_Time","Draws_Home_Time","Loses_Home_Time"))
      for(l in 1:length(models_reg)){
        set.seed(10)
        fit.RF_2 = train(dif_gols~.,treino[,sel_var],trControl = trainControl, method = models_reg[l],metric = "MAE",preProc=c("center","scale"))
        saveRDS(fit.RF_2,paste('./modelo_difgols',as.character(ligas[i]),models_reg[l],'.rds')) #pensar em o que coletar
        key = models_reg[l]
        modellist[[key]] = fit.RF_2
        metrica[l] = mean(abs(predict(fit.RF_2,teste)-teste$dif_gols))
      }
      
      fit.RF_2 = modellist[[arrange(data.frame(modelos = models_reg,metrica = metrica),metrica)[1,]$modelos]]
      predictions = predict(fit.RF_2,teste)
      saveRDS(fit.RF_2,paste('./modelo_difgols',as.character(ligas[i]),'.rds')) 
      MAE[i] = mean(abs(predictions-teste$dif_gols)) 
      
      "Financeiro"
      stake = 1
      calc = teste[ifelse(predict(fit.RF_2,teste) > 0,TRUE,FALSE),]
      calc$g_r = ifelse(calc$dif_gols > 0,'Green','Red')
      reg_stakes[i] = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))
      reg_ROI[i] = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))/(nrow(calc)*stake))*100
      reg_jogos_bet[i] = (nrow(calc)/nrow(teste))*100
      calc = teste[ifelse(predict(fit.RF_2,teste) < 0,TRUE,FALSE),]
      calc$g_r = ifelse(calc$dif_gols < 0,'Green','Red')
      reg_stakes_l[i] = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_A-1),-stake))
      reg_ROI_l[i] = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_A-1),-stake))/(nrow(calc)*stake))*100
      reg_jogos_bet_l[i] = (nrow(calc)/nrow(teste))*100
      
      
      
      
    } else {
    }
    
  }
}

"As métricas escolhidas para avaliar a performance do modelo serão baseadas na 
proposta do problema, que é prever se o time mandante vai vencer ou não sua partida,
e com base nisso adotarei o precision referente a classe W (de vitórias) como métrica
principal que pode ser nomeada como w_rate, mas como métrica alternativa e que vai
interessar bastante pela dificuldade do problema, é o wd_rate, que é o acerto que o
modelo tem para que o time mandante no mínimo empate com seu adversário. Essa outra
abordagem, é importante, uma vez que classificar um time vencedor ou não em uma partida
de futebol pode ser bem complicado. E assim, em apostas esportivas ter em mãos o wd_rate
como métrica auxiliar, é uma excelente alternativa pra ser lucrativo sem correr muito risco,
uma vez que é possível fazer apostas para o time mandante e caso a partida termine empatada,
o apostador pode não perder nada com a aposta e até ganhar um pouco por conta do empate."

"Uma métrica extra que é muito importante pra avaliação do modelo é o ROI, que seria o
retorno financeiro em percentual do investimento feito nas apostas. Assim podemos avaliar
se o modelo é rentável ou não, independente do w_rate e wd_rate. Algo que podemos fazer
para aumentar essa rentabilidade é definir uma probabilidade de corte pra vitória do time
mandante de tal forma que maximize o ROI."

"Sobre a abordagem de modelar XG para prever dados faltantes a melhora não foi tão significativa,
aumentando um w_rate em pouco menos que 1%, mas na minha visão é um modelo mais robusto
do que simplesmente considerando só a média do time seja por ano ou não, além de não perder nada
dos dados iniciais, o que é muito bom! Poderíamos melhorar mais tentando encontrar ou criar novas 
features e até usar outros tipos de modelos."

"Guardando métricas do modelo"
perf = data.frame()
{
  for(i in 1:length(ligas)){
    cm = cms[,,i]
    perf = rbind(perf,c(as.character(ligas[i]),as.numeric(1-(cm[3,2]+cm[1,2])/sum(cm[-2,])),
                        as.numeric((cm[3,3])/sum(cm[3,])),as.numeric((cm[2,2])/sum(cm[2,])),MAE[i],
                        clas_stakes[i],
                        clas_ROI[i],
                        clas_jogos_bet[i],
                        reg_stakes[i],
                        reg_ROI[i],
                        reg_jogos_bet[i],
                        clas_stakes_l[i],
                        clas_ROI_l[i],
                        clas_jogos_bet_l[i],
                        reg_stakes_l[i],
                        reg_ROI_l[i],
                        reg_jogos_bet_l[i]))
  }
  colnames(perf) = c("Liga","wd_rate","w_rate","l_rate","MAE_difgols",
                     "clas_stakes","clas_ROI","clas_jogos_bet","reg_stakes","reg_ROI","reg_jogos_bet",
                     "clas_stakes_l","clas_ROI_l","clas_jogos_bet_l","reg_stakes_l","reg_ROI_l","reg_jogos_bet_l")
  perf$wd_rate = as.numeric(perf$wd_rate)
  perf$w_rate = as.numeric(perf$w_rate)
  perf$l_rate = as.numeric(perf$l_rate)
  perf$clas_stakes = as.numeric(perf$clas_stakes)
  perf$clas_ROI = as.numeric(perf$clas_ROI)
  perf$clas_jogos_bet = as.numeric(perf$clas_jogos_bet)
  perf$reg_stakes = as.numeric(perf$reg_stakes)
  perf$reg_ROI = as.numeric(perf$reg_ROI)
  perf$reg_jogos_bet = as.numeric(perf$reg_jogos_bet)
  perf$clas_stakes_l = as.numeric(perf$clas_stakes_l)
  perf$clas_ROI_l = as.numeric(perf$clas_ROI_l)
  perf$clas_jogos_bet_l = as.numeric(perf$clas_jogos_bet_l)
  perf$reg_stakes_l = as.numeric(perf$reg_stakes_l)
  perf$reg_ROI_l = as.numeric(perf$reg_ROI_l)
  perf$reg_jogos_bet_l = as.numeric(perf$reg_jogos_bet_l)
  perf$MAE_difgols = as.numeric(perf$MAE_difgols)
}
perf
perf = na.omit(perf)
write.table(perf, file = "perf_brazilserieafinal.csv", sep = ";", na = "NA", quote = TRUE,dec=",", row.names = FALSE,fileEncoding = "latin1")
perf = read.csv("perf_brazilserieafinal.csv",sep=";",fileEncoding = "latin1",dec=",")
perf

#Avaliando o Modelo
"Análise do Modelo"
{
  
  fit.RF = readRDS(paste("modelo Brazil Serie A",".rds"))
  fit.RF
  varImp(fit.RF)
  set.seed(10)
  index = createDataPartition(y = data$result_H, times = 1, p = 0.7, list = FALSE)
  treino = data[index,]
  teste = data[-index,]
  predictions = predict(fit.RF,teste, type = "raw")
  confusionMatrix(table(predictions,teste$result_H))
  
  "Financeiro"
  #calc = teste[ifelse(predictions == 'W',TRUE,FALSE),]
  stake = 1
  probs = seq(0.33,0.6,0.01)
  rois = c()
  perc_jogos = c()
  for(p in 1:length(probs)){
    calc = teste[ifelse(predict(fit.RF,teste, type = "prob")[,"W"] >= probs[p],TRUE,FALSE),]
    calc$g_r = ifelse(calc$result_H == 'W','Green','Red')
    clas_stakes = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))
    calc$stakes = ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)
    clas_ROI = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))/(nrow(calc)*stake))*100
    calc$ROI = (ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)/(nrow(calc)*stake))*100
    clas_jogos_bet = (nrow(calc)/nrow(teste))*100
    rois[p] = clas_ROI
    perc_jogos[p] = clas_jogos_bet
    #cat("\n","prob",probs[p]," - ",clas_stakes,'stakes em',nrow(calc),'jogos, com um ROI de',clas_ROI,'%','entrando em',(nrow(calc)/nrow(teste))*100,'% dos jogos\n')
  }

  cat('probabilidade de corte para melhor ROI : ',arrange(data.frame(probs = probs, ROI = rois,perc = perc_jogos),desc(ROI))$probs[1],
      'com um ROI de',arrange(data.frame(probs = probs, ROI = rois,perc = perc_jogos),desc(ROI))$ROI[1],'%','entrando em',
      arrange(data.frame(probs = probs, ROI = rois,perc = perc_jogos),desc(ROI))$perc[1],'% dos jogos')

  
}
arrange(data.frame(probs = probs, ROI = rois,perc = perc_jogos))

"definindo a probabilidade que maximiza o ROI e analisando resultados"
prob = arrange(data.frame(probs = probs, ROI = rois,perc = perc_jogos),desc(ROI))$prob[1]
prob

teste = teste2022

#Classificação
calc = teste[ifelse(predict(fit.RF,teste, type = "prob")[,"W"] >= prob,TRUE,FALSE),]
calc$g_r = ifelse(calc$result_H == 'W','Green','Red')
clas_stakes = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))
calc$stakes = ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)
clas_ROI = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))/(nrow(calc)*stake))*100
calc$ROI = (ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)/(nrow(calc)*stake))*100

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds")

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  facet_wrap(~Home)+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds para os Times Mandantes")

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  facet_wrap(~Away)+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds para os Times Visitantes")

ggplot(data = calc, mapping = aes(x = ROI))+
  geom_histogram(bins = 15,fill = 'green', col = 'black')+
  ggtitle("Distribuição Geral do ROI")

ggplot(data = calc, mapping = aes(x = ROI))+
  geom_boxplot(fill = 'green',col = 'black')+
  ggtitle("Distribuição Geral do ROI - Boxplot")

ggplot(data = calc, mapping = aes(x = ROI,fill = Home))+
  geom_boxplot()+
  facet_wrap(~Home)+
  ggtitle("Distribuição do ROI por Mandante")

calc$entradas = seq(1,nrow(calc),1)
"Neste caso corresponde a uma stake, que pode ser entendida como um real por aposta"
"Ao longo do tempo seria o saldo acumulado da carteira do investidor"
ggplot(data = calc, mapping = aes(y = cumsum(calc$stakes),x = entradas))+
  geom_line(color="green")+
  geom_hline(yintercept = 0,color = "red")+
  geom_hline(yintercept = mean(cumsum(calc$stakes)),color = "black")+
  ggtitle("Soma Acumulada de Stakes (Lucro por Unidade Investida) ao longo do tempo")

ggplot(data = calc, mapping = aes(y = cumsum(calc$ROI),x = entradas))+
  geom_line(color = "green")+
  geom_hline(yintercept = 0,color = "red")+
  geom_hline(yintercept = mean(cumsum(calc$ROI)),color = "black")+
  ggtitle("Soma Acumulada do ROI ao longo do tempo de investimento nas apostas")


cumsum(calc$ROI)[nrow(calc)]
"Portanto, podemos concluir que o modelo que aponta a vitória dos times mandantes é rentável no campeonato brasileiro,
atingindo um ROI de 20.16%, que seria o retorno do investimento total feito na liga após n jogos apostados."

teste
#Regressão
fit.RF = readRDS(paste("modelo_difgols Brazil Serie A",".rds"))
fit.RF
calc = teste[ifelse(predict(fit.RF,teste) > 0,TRUE,FALSE),]
calc$g_r = ifelse(calc$result_H == 'W','Green','Red')
clas_stakes = sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))
calc$stakes = ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)
clas_ROI = (sum(ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake))/(nrow(calc)*stake))*100
calc$ROI = (ifelse(calc$g_r == 'Green',stake*(calc$FT_Odds_H-1),-stake)/(nrow(calc)*stake))*100

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds")

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  facet_wrap(~Home)+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds para os Times Mandantes")

ggplot(data = calc, mapping = aes(x = g_r, fill = g_r))+
  geom_bar()+
  facet_wrap(~Away)+
  scale_fill_manual(values = c("green","red"))+
  ggtitle("Quantidade de Greens e Reds para os Times Visitantes")

ggplot(data = calc, mapping = aes(x = ROI))+
  geom_histogram(bins = 15,fill = 'green', col = 'black')+
  ggtitle("Distribuição Geral do ROI")

ggplot(data = calc, mapping = aes(x = ROI))+
  geom_boxplot(fill = 'green',col = 'black')+
  ggtitle("Distribuição Geral do ROI - Boxplot")

ggplot(data = calc, mapping = aes(x = ROI,fill = Home))+
  geom_boxplot()+
  facet_wrap(~Home)+
  ggtitle("Distribuição do ROI por Mandante")

calc$entradas = seq(1,nrow(calc),1)
"Neste caso corresponde a uma stake, que pode ser entendida como um real por aposta"
"Ao longo do tempo seria o saldo acumulado da carteira do investidor"
ggplot(data = calc, mapping = aes(y = cumsum(calc$stakes),x = entradas))+
  geom_line(color="green")+
  geom_hline(yintercept = 0,color = "red")+
  geom_hline(yintercept = mean(cumsum(calc$stakes)),color = "black")+
  ggtitle("Soma Acumulada de Stakes (Lucro por Unidade Investida) ao longo do tempo")

ggplot(data = calc, mapping = aes(y = cumsum(calc$ROI),x = entradas))+
  geom_line(color = "green")+
  geom_hline(yintercept = 0,color = "red")+
  geom_hline(yintercept = mean(cumsum(calc$ROI)),color = "black")+
  ggtitle("Soma Acumulada do ROI ao longo do tempo de investimento nas apostas")


cumsum(calc$ROI)[nrow(calc)]
"Portanto, podemos concluir que o modelo de regressão não é rentável no campeonato brasileiro,
atingindo um ROI de -18.67%, que seria o retorno do investimento total feito na liga após n jogos apostados."