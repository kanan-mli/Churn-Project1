# make sure to be in the appropriate working directory
cardholders<- read.csv("an13.csv", header=TRUE)
use_of_card<-read.csv("in13.csv", header=TRUE)
renewal<- read.csv("dati2.csv", header=TRUE)
library(tidyverse)
library(lubridate)
use_of_card$datai<-as.Date(parse_date_time(use_of_card$datai, "dmy"))
use_of_card_ordered<- arrange(use_of_card, use_of_card$CodCliente, use_of_card$datai)
total_cost<-rep(0,545085)
length(total_cost) == dim(use_of_card)[1]
use_of_card_ordered <- cbind(use_of_card_ordered, total_cost)
sum_cost = 0
#beware that the following chunk of code may take some minutes to run in full
#####################
for (i in 1:dim(use_of_card_ordered)[1]){
  if (i == dim(use_of_card_ordered)[1]) {
    if (use_of_card_ordered$CodCliente[i] == use_of_card_ordered$CodCliente[i-1]){
      use_of_card_ordered$total_cost[i] = sum_cost+use_of_card_ordered$importo[i]
    } else {
      use_of_card_ordered$total_cost[i] = use_of_card_ordered$importo[i]
      }
  } else {
      if (use_of_card_ordered$CodCliente[i] == use_of_card_ordered$CodCliente[i+1]){
        sum_cost = sum_cost + use_of_card_ordered$importo[i]
      } else {
        sum_cost = sum_cost + use_of_card_ordered$importo[i]
        use_of_card_ordered$total_cost[i] = sum_cost
        sum_cost = 0
      }
    }
}
##################
sum(is.na(use_of_card_ordered))
sum(is.na(cardholders))
sum(is.na(renewal))

renewal %>% summarize_all(funs(sum(is.na(.)) / length(.)))
cardholders %>% summarize_all(funs(sum(is.na(.)) / length(.)))
use_of_card_ordered %>% summarize_all(funs(sum(is.na(.)) / length(.)))

#find the unique values in the column codcliente
unique_val <- unique(use_of_card_ordered$CodCliente)
length(unique_val)

#drop the column x
use_of_card_ordered <- use_of_card_ordered %>% select(-X)
use_of_card_ordered

library(stringr)
#divide the date part from the time one in the column data_inizio
two_values <- str_split_fixed(cardholders$data_inizio, ' ', 2)
date <- two_values[, 1]
time <- two_values[,2]

cardholders$data_inizio <- date
cardholders$data_inizio<-as.Date(parse_date_time(cardholders$data_inizio, "dmy"))
cardholders_ordered <- arrange(cardholders, cardholders$codcliente)

unique_dates <- unique(use_of_card_ordered$datai)
length(unique_dates)
occurences = rep(0, length(unique_dates)) 
date_occ = data.frame(unique_dates, occurences)

for (i in 1:dim(use_of_card_ordered)[1]) {
  index = match(use_of_card_ordered$datai[i], date_occ$unique_dates)
  date_occ$occurences[index] = date_occ$occurences[index] +1
}

date_occ = arrange(date_occ, unique_dates)    
library(ggplot2)     
ggplot(date_occ)+
  geom_line(aes(x=unique_dates, y=occurences))


####################
#renewal data cleaning
#####################
summary(renewal)


sum(is.na(renewal))


renewal %>% summarize_all(funs(sum(is.na(.)) / length(.)))

#mode is preferable for categorical variables
#define missing values in vector
values <- unique(renewal$sesso)[!is.na(renewal$sesso)]
# mode of cat_column
themode <- values[which.max(tabulate(match(renewal$sesso, values)))] 
#assign missing vector
renewal$sesso <- renewal$sesso                                  
renewal$sesso[is.na(renewal$sesso)] <- themode



#here we have huge outlier and I prefer median
renewal$nvisite0512  <- ifelse(is.na(renewal$nvisite0512), 0,renewal$nvisite0512)
renewal$nmusei0512  <- ifelse(is.na(renewal$nmusei0512), 0,renewal$nmusei0512)

renewal$prov <- ifelse(is.na(renewal$prov), "unknown", renewal$prov)
renewal <- subset( renewal, select = -abb14 )

renewal %>% summarize_all(funs(sum(is.na(.)) / length(.)))
renewal$ultimo_ing.x <- ifelse(is.na(renewal$ultimo_ing.x), mode(renewal$ultimo_ing.x), renewal$ultimo_ing.x)



sum(is.na(renewal))

#################
#data cleaning cardholders
########################

cardholders_ordered %>% summarize_all(funs(sum(is.na(.)) / length(.)))

#define missing values in vector
values <- unique(cardholders_ordered$sesso)[!is.na(cardholders_ordered$sesso)]
# mode of cat_column
themode <- values[which.max(tabulate(match(cardholders_ordered$sesso, values)))] 
#assign missing vector
cardholders_ordered$sesso <- cardholders_ordered$sesso                                  
cardholders_ordered$sesso[is.na(cardholders_ordered$sesso)] <- themode

cardholders_ordered <- subset( cardholders_ordered, select = -professione )

sum(is.na(cardholders_ordered))

###################
#use_of_card_ordered data cleaning
####################
sum(is.na(use_of_card_ordered$datai))
unique_val_prov_museo <- unique(use_of_card_ordered$prov_museo)
unique_val_prov_museo
unique_val_com_museo <- unique(use_of_card_ordered$com_museo)
unique_val_com_museo



sum(is.na(use_of_card_ordered))

use_of_card_ordered$prov_museo <- ifelse(use_of_card_ordered$prov_museo== "XX", "unknown", use_of_card_ordered$prov_museo)

use_of_card_ordered %>% summarize_all(funs(sum(is.na(.)) / length(.)))


sum(is.na(use_of_card_ordered))       
sum(is.na(renewal))
sum(is.na(cardholders_ordered))

#find unique values in the card holder dataset
cardholders_ordered <- cardholders_ordered[cardholders_ordered$data_nascita<=2014,]
cardholders_ordered<- cardholders_ordered[cardholders_ordered$data_nascita>1914,]

length(unique(cardholders_ordered$codcliente)) #87018, it's the same as the rows of the dataset
length(unique(cardholders_ordered$date)) #just 338 different dates
length(unique(cardholders_ordered$importo)) #8
length(unique(cardholders_ordered$sconto)) #24
length(unique(cardholders_ordered$riduzione)) #12
length(unique(cardholders_ordered$tipo_pag)) #5
length(unique(cardholders_ordered$agenzia)) #146
length(unique(cardholders_ordered$agenzia_tipo)) #13
length(unique(cardholders_ordered$sesso)) #2
length(unique(cardholders_ordered$data_nascita)) #98
length(unique(cardholders_ordered$comune)) #1644
length(unique(cardholders_ordered$cap)) #1452
length(unique(cardholders_ordered$nuovo_abb)) #2


eta_13<-unique(renewal$eta13)
eta_13<-sort(eta_13)
eta_13 # age from 0-10 and >100 are pretty unrealistic, we drop those rows
renewal <- renewal[renewal$eta13>10,]
renewal<- renewal[renewal$eta13<100,]
dim(renewal)
#find unique values in the renewal dataset
length(unique(renewal$codcliente)) #79785, it's the same as the rows of the dataset
length(unique(renewal$si2013)) #1
length(unique(renewal$si2014)) #2
length(unique(renewal$nabb0512)) #9
length(unique(renewal$nvisit13)) #85
length(unique(renewal$nmus13)) #44
length(unique(renewal$nvisite0512)) #324
length(unique(renewal$nmusei0512)) #198
length(unique(renewal$sesso)) #2
length(unique(renewal$eta13)) #89
length(unique(renewal$prezzo13)) #6
length(unique(renewal$cambiocap0512)) #2
length(unique(renewal$prov)) #107
length(unique(renewal$ultimo_ing.x)) #472
length(unique(renewal$abb13)) #121


#find unique values in the use of card dataset
use_of_card_ordered <- subset(use_of_card_ordered, select = -X )
length(unique(use_of_card_ordered$CodCliente)) #77846 -> LESS THAN THE NUMBER OF ROWS
length(unique(use_of_card_ordered$datai)) #365
length(unique(use_of_card_ordered$orai)) #909
length(unique(use_of_card_ordered$importo)) #26
length(unique(use_of_card_ordered$museo)) #139
length(unique(use_of_card_ordered$prov_museo)) #9
length(unique(use_of_card_ordered$com_museo)) #64
length(unique(use_of_card_ordered$total_cost)) #1948
#next chunk may need some minutes to run, too
#
for (i in dim(use_of_card_ordered)[1]:2){
  if (use_of_card_ordered$CodCliente[i] == use_of_card_ordered$CodCliente[i-1] &
      use_of_card_ordered$total_cost[i] != 0){
    use_of_card_ordered$total_cost[i-1] <- use_of_card_ordered$total_cost[i]
  }
}
#    

renewal_ordered <- arrange(renewal, renewal$codcliente)    
renewal_ordered <- renewal_ordered %>% select(-X)
cardholders_ordered <- cardholders_ordered %>% select(-X)
names(renewal_ordered)[13] <- "prov_cliente"
names(use_of_card_ordered)[1] <- "data"
names(use_of_card_ordered)[2] <- "ora"
names(use_of_card_ordered)[3] <- "importo_biglietto"
names(cardholders_ordered)[3] <- "costo_carta"

full_data<- merge(use_of_card_ordered, renewal_ordered, by.x = "CodCliente",
                  by.y = "codcliente", all.x = TRUE, all.y = TRUE)
full_data %>% summarize_all(funs(sum(is.na(.)) / length(.)))
full_dataset <- merge(full_data, cardholders_ordered, by.x = "CodCliente", 
                      by.y = "codcliente", all.x = TRUE, all.y = TRUE)  
full_dataset %>% summarize_all(funs(sum(is.na(.)) / length(.)))  

indexes <- which(full_dataset$sesso.x != full_dataset$sesso.y) #they're all equal 
indexes <- which(full_dataset$prezzo13 != full_dataset$importo_carta) # they're all equal

full_dataset <- full_dataset %>% select(-sesso.x)
full_dataset <- full_dataset %>% select(-prezzo13)
  
full_dataset<- full_dataset[is.na(full_dataset$si2013) == FALSE,]
full_dataset %>% summarize_all(funs(sum(is.na(.))/length(.))) 


full_dataset$costo_carta <- ifelse(is.na(full_dataset$costo_carta),
                                   median(full_dataset$costo_carta, na.rm=TRUE),
                                   full_dataset$costo_carta)  
  
full_dataset<- full_dataset[is.na(full_dataset$data) == FALSE,]
full_dataset<- full_dataset[is.na(full_dataset$data_inizio) == FALSE,]
full_dataset %>% summarize_all(funs(sum(is.na(.))/length(.))) 


names(full_dataset)[10]<- "y"
full_dataset <- full_dataset %>% select(-si2013)

length(unique(full_dataset$CodCliente)) #71343
names(full_dataset)[8]<-"total_cost_to_company"
full_dataset$total_cost_to_company= (full_dataset$total_cost_to_company)/2
full_dataset$total_cost_to_company= round(full_dataset$total_cost_to_company,2)
full_dataset= arrange(full_dataset, full_dataset$CodCliente, full_dataset$data)
x<- as.Date("2013-01-01")
k=0
for (i in 1:length(full_dataset$data)){
  if (full_dataset$data[i]-x <0) {
    k = k+1
  }
}
k #8451

l = rep(0,8451)
k=1
for (i in 1:length(full_dataset$data)){
  if (full_dataset$data[i]-x <0) {
    l[k]=i
    k= k+1
  }
}

for (i in 1:length(l)){
  year(full_dataset$data[l[i]]) = 2013 
  }

accomp = rep(0, 507397)
full_dataset = cbind(full_dataset,accomp)
for (i in 1:dim(full_dataset)[1]){
  if (i == dim(full_dataset)[1]) {
    if (full_dataset$data[i] == full_dataset$data[i-1] & full_dataset$museo[i] ==
        full_dataset$museo[i-1]){
      full_dataset$accomp[i] = 1
    } 
    else {
      full_dataset$accomp[i] = 0
    }
  } else {
    if (full_dataset$data[i] == full_dataset$data[i+1] & full_dataset$museo[i] ==
        full_dataset$museo[i+1]){
      full_dataset$accomp[i] = 1
    }
    else {
      full_dataset$accomp[i] = 0
    } 
  }
}

full_dataset$ultimo_ing.x<-as.Date(parse_date_time(full_dataset$ultimo_ing.x, "dmy"))
full_dataset$abb13<-as.Date(parse_date_time(full_dataset$abb13, "dmy"))
full_dataset$nuovo_abb = ifelse(full_dataset$nuovo_abb == "NUOVO ABBONATO", 1, 0)
full_dataset$com_museo = ifelse(full_dataset$com_museo == "TORINO (TO)", "TORINO",
                                full_dataset$com_museo)  
 
k=0
for (i in 1:507397){
    if (full_dataset$sconto[i] == "RINNOVO ABBONAMENTO" &
        full_dataset$nuovo_abb[i] == 1 & full_dataset$nabb0512[i] == 0){
      k = k+1
    }
  }
k # 5263, logical inconsistency: "nuovo_abb" is 1 if the person did not have the 
  # card in 2012 and "sconto" applies to 2013 cards (not to 2014 since 26134 
  # churners have that option) how can 5263 new subscriptors (never had the card)
  # have the "renewal subscription" discount? (Maybe drop the rows)
names(full_dataset)[31] <- "no_card_in_2012"


# next chunk also will take a bit to run until the end
i =1
while (i <= dim(full_dataset)[1]){
  if (full_dataset$sconto[i] == "RINNOVO ABBONAMENTO" &
      full_dataset$no_card_in_2012[i] == 1 & full_dataset$nabb0512[i] == 0){
    full_dataset = full_dataset[-i,]
  }
  else {
    i = i+1
  }
}

full_dataset <- full_dataset %>% select(-data_inizio)
full_dataset <- full_dataset %>% select(-data_nascita)
full_dataset <- full_dataset %>% select(-cap_cliente)

elenco_comuni_italiani = read.csv("Elenco-comuni-italiani.csv", header= TRUE)

names(elenco_comuni_italiani)[1] <- "CodReg"
names(elenco_comuni_italiani)[2] <- "UnTerr"
names(elenco_comuni_italiani)[3] <- "CodProv"
names(elenco_comuni_italiani)[4] <- "ProgCom"
names(elenco_comuni_italiani)[5] <- "AlfaCod"
names(elenco_comuni_italiani)[6] <- "Comune"

elenco_comuni_italiani <- elenco_comuni_italiani %>% select(-CodReg)
elenco_comuni_italiani <- elenco_comuni_italiani %>% select(-UnTerr)
elenco_comuni_italiani <- elenco_comuni_italiani %>% select(-CodProv)
elenco_comuni_italiani <- elenco_comuni_italiani %>% select(-ProgCom)
elenco_comuni_italiani <- elenco_comuni_italiani %>% select(-AlfaCod)



elenco_comuni_italiani <- elenco_comuni_italiani[,-3]
elenco_comuni_italiani <- elenco_comuni_italiani[,-c(1,3,4,5)]
elenco_comuni_italiani <- elenco_comuni_italiani[,-c(3:16)]

names(elenco_comuni_italiani)[1] <- "Comune"
names(elenco_comuni_italiani)[2] <- "Provincia"
com_TO = elenco_comuni_italiani %>% filter(Provincia == "Torino")
com_TO = com_TO[,-2]  
unique(com_TO)  
com_TO = toupper(com_TO)  
head(com_TO, 10)


provincia_cliente = rep("", dim(full_dataset)[1])
full_dataset = cbind(full_dataset, provincia_cliente)
# running the following loop may take some time (~ 40 minutes)
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_TO){
    full_dataset$provincia_cliente[i] ="TORINO"
    full_dataset$comune_cliente[i] = "-"
  }
}
unique(full_dataset$comune_cliente)

com_CO = elenco_comuni_italiani %>% filter(Provincia == "Como")
com_CO = com_CO[,-2]  
unique(com_CO)  
com_CO = toupper(com_CO)  
head(com_CO, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_CO){
    full_dataset$provincia_cliente[i] ="COMO"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_CN = elenco_comuni_italiani %>% filter(Provincia == "Cuneo")
com_CN = com_CN[,-2]  
length(unique(com_CN)) 
com_CN = toupper(com_CN)  
head(com_CN, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_CN){
    full_dataset$provincia_cliente[i] ="CUNEO"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_AT = elenco_comuni_italiani %>% filter(Provincia == "Asti")
com_AT = com_AT[,-2]  
length(unique(com_AT)) 
com_AT = toupper(com_AT)  
head(com_AT, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_AT){
    full_dataset$provincia_cliente[i] ="ASTI"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_VC = elenco_comuni_italiani %>% filter(Provincia == "Vercelli")
com_VC = com_VC[,-2]  
length(unique(com_VC)) 
com_VC = toupper(com_VC)  
head(com_VC, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_VC){
    full_dataset$provincia_cliente[i] ="VERCELLI"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_BI = elenco_comuni_italiani %>% filter(Provincia == "Biella")
com_BI = com_BI[,-2]  
length(unique(com_BI)) 
com_BI = toupper(com_BI)  
head(com_BI, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_BI){
    full_dataset$provincia_cliente[i] ="BIELLA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_NO = elenco_comuni_italiani %>% filter(Provincia == "Novara")
com_NO = com_NO[,-2]  
length(unique(com_NO)) 
com_NO = toupper(com_NO)  
head(com_NO, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_NO){
    full_dataset$provincia_cliente[i] ="NOVARA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_AL = elenco_comuni_italiani %>% filter(Provincia == "Alessandria")
com_AL = com_AL[,-2]  
length(unique(com_AL)) 
com_AL = toupper(com_AL)  
head(com_AL, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_AL){
    full_dataset$provincia_cliente[i] ="ALESSANDRIA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_AO = elenco_comuni_italiani %>% filter(Provincia == "Valle d'Aosta/Vallée d'Aoste")
com_AO = com_AO[,-2]  
length(unique(com_AO)) 
com_AO = toupper(com_AO)  
head(com_AO, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_AO){
    full_dataset$provincia_cliente[i] ="AOSTA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_MI = elenco_comuni_italiani %>% filter(Provincia == "Milano")
com_MI = com_MI[,-2]  
length(unique(com_MI)) 
com_MI = toupper(com_MI)  
head(com_MI, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_MI){
    full_dataset$provincia_cliente[i] ="MILANO"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_IM = elenco_comuni_italiani %>% filter(Provincia == "Imperia")
com_IM = com_IM[,-2]  
length(unique(com_IM)) 
com_IM = toupper(com_IM)  
head(com_IM, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_IM){
    full_dataset$provincia_cliente[i] ="IMPERIA"
    full_dataset$comune_cliente[i] = "-"
  }
}

for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$prov_cliente[i] == "NI"){
    full_dataset$provincia_cliente[i] ="NIZZA (FRANCIA)"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_PV = elenco_comuni_italiani %>% filter(Provincia == "Pavia")
com_PV = com_PV[,-2]  
length(unique(com_PV)) 
com_PV = toupper(com_PV)  
head(com_PV, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_PV){
    full_dataset$provincia_cliente[i] ="PAVIA"
    full_dataset$comune_cliente[i] = "-"
  }
}

for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] == "DATO MANCANTE"){
    full_dataset$provincia_cliente[i] ="SCONOSCIUTO"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_GE = elenco_comuni_italiani %>% filter(Provincia == "Genova")
com_GE = com_GE[,-2]  
length(unique(com_GE)) 
com_GE = toupper(com_GE)  
head(com_GE, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_GE){
    full_dataset$provincia_cliente[i] ="GENOVA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_ME = elenco_comuni_italiani %>% filter(Provincia == "Messina")
com_ME = com_ME[,-2]  
length(unique(com_ME)) 
com_ME = toupper(com_ME)  
head(com_ME, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_ME){
    full_dataset$provincia_cliente[i] ="MESSINA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_FE = elenco_comuni_italiani %>% filter(Provincia == "Ferrara")
com_FE = com_FE[,-2]  
length(unique(com_FE)) 
com_FE = toupper(com_FE)  
head(com_FE, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_FE){
    full_dataset$provincia_cliente[i] ="FERRARA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_PC = elenco_comuni_italiani %>% filter(Provincia == "Piacenza")
com_PC = com_PC[,-2]  
length(unique(com_PC)) 
com_PC = toupper(com_PC)  
head(com_PC, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_PC){
    full_dataset$provincia_cliente[i] ="PIACENZA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_PD = elenco_comuni_italiani %>% filter(Provincia == "Padova")
com_PD = com_PD[,-2]  
length(unique(com_PD)) 
com_PD = toupper(com_PD)  
head(com_PD, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_PD){
    full_dataset$provincia_cliente[i] ="PADOVA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_VI = elenco_comuni_italiani %>% filter(Provincia == "Vicenza")
com_VI = com_VI[,-2]  
length(unique(com_VI)) 
com_VI = toupper(com_VI)  
head(com_VI, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_VI){
    full_dataset$provincia_cliente[i] ="VICENZA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_CZ = elenco_comuni_italiani %>% filter(Provincia == "Catanzaro")
com_CZ = com_CZ[,-2]  
length(unique(com_CZ)) 
com_CZ = toupper(com_CZ)  
head(com_CZ, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_CZ){
    full_dataset$provincia_cliente[i] ="CATANZARO"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_RM = elenco_comuni_italiani %>% filter(Provincia == "Roma")
com_RM = com_RM[,-2]  
length(unique(com_RM)) 
com_RM = toupper(com_RM)  
head(com_RM, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_RM){
    full_dataset$provincia_cliente[i] ="ROMA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_FI = elenco_comuni_italiani %>% filter(Provincia == "Firenze")
com_FI = com_FI[,-2]  
length(unique(com_FI)) 
com_FI = toupper(com_FI)  
head(com_FI, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_FI){
    full_dataset$provincia_cliente[i] ="FIRENZE"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_NA = elenco_comuni_italiani %>% filter(Provincia == "Napoli")
com_NA = com_NA[,-2]  
length(unique(com_NA)) 
com_NA = toupper(com_NA)  
head(com_NA, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_NA){
    full_dataset$provincia_cliente[i] ="NAPOLI"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_VE = elenco_comuni_italiani %>% filter(Provincia == "Venezia")
com_VE = com_VE[,-2]  
length(unique(com_VE)) 
com_VE = toupper(com_VE)  
head(com_VE, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_VE){
    full_dataset$provincia_cliente[i] ="VENEZIA"
    full_dataset$comune_cliente[i] = "-"
  }
}

com_PA = elenco_comuni_italiani %>% filter(Provincia == "Palermo")
com_PA = com_PA[,-2]  
length(unique(com_PA)) 
com_PA = toupper(com_PA)  
head(com_PA, 10)
# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] %in% com_PA){
    full_dataset$provincia_cliente[i] ="PALERMO"
    full_dataset$comune_cliente[i] = "-"
  }
}


# running the following loop may take some time 
for (i in 1:dim(full_dataset)[1]){
  if (full_dataset$comune_cliente[i] != "-"){
    full_dataset$provincia_cliente[i] ="ALTRA"
    full_dataset$comune_cliente[i] = "-"
  }
}

full_dataset <- full_dataset %>% select(-prov_cliente)
full_dataset <- full_dataset %>% select(-comune_cliente)
full_dataset <- full_dataset %>% select(-rep_visits)

utilizzo_carta = rep(0, dim(full_dataset)[1])
full_dataset = cbind(full_dataset, utilizzo_carta)
for (i in 1: dim(full_dataset)[1]){
  full_dataset$utilizzo_carta[i] = full_dataset$ultimo_ing.x[i]-full_dataset$abb13[i]
}  

for (i in dim(full_dataset)[1]:1){
  if (i == 1){
    if (full_dataset$CodCliente[i] == full_dataset$CodCliente[i+1]){
      if (full_dataset$accomp[i+1] == 1){
        full_dataset$accomp[i] = 1
      }
    }
  } else {
    if(full_dataset$CodCliente[i] == full_dataset$CodCliente[i-1]){
      if(full_dataset$accomp[i] == 1){
        full_dataset$accomp[i-1] = 1
      }
    }
  }
}

full_dataset <- full_dataset %>% select(-ultimo_ing.x) 
full_dataset <- full_dataset %>% select(-abb13) 

dataset_for_models = full_dataset
dataset_for_models <- dataset_for_models %>% select(-data) 
dataset_for_models <- dataset_for_models %>% select(-ora) 
dataset_for_models <- dataset_for_models %>% select(-importo_biglietto) 
dataset_for_models <- dataset_for_models %>% select(-museo) 
dataset_for_models <- dataset_for_models %>% select(-com_museo) 
dataset_for_models <- dataset_for_models %>% select(-agenzia) 
dataset_for_models <- dataset_for_models %>% select(-prov_museo)

dataset_for_models_to_adjust = dataset_for_models %>% distinct()

for (i in 1:dim(dataset_for_models_to_adjust)[1]){
 if(i == dim(dataset_for_models_to_adjust)[1]){
    if (dataset_for_models_to_adjust$CodCliente[i] == dataset_for_models_to_adjust$CodCliente[i-1]){
      dataset_for_models_to_adjust$accomp[i] = 1
    }}
  else {
  if(dataset_for_models_to_adjust$CodCliente[i] == dataset_for_models_to_adjust$CodCliente[i+1]){
    dataset_for_models_to_adjust$accomp[i+1] = 1
  }}
}
dataset_for_models = dataset_for_models_to_adjust %>% distinct()

dataset_for_models$y = ifelse(dataset_for_models$y == 0, 1, 0)  #switch level for churn

dataset_for_models$cambiocap0512 = as.factor(dataset_for_models$cambiocap0512)
dataset_for_models$sconto = as.factor(dataset_for_models$sconto)
dataset_for_models$riduzione = as.factor(dataset_for_models$riduzione)
dataset_for_models$tipo_pag = as.factor(dataset_for_models$tipo_pag)
dataset_for_models$agenzia_tipo = as.factor(dataset_for_models$agenzia_tipo)
dataset_for_models$sesso.y = as.factor(dataset_for_models$sesso.y)
dataset_for_models$no_card_in_2012 = as.factor(dataset_for_models$no_card_in_2012)
dataset_for_models$accomp = as.factor(dataset_for_models$accomp)
dataset_for_models$provincia_cliente = as.factor(dataset_for_models$provincia_cliente)


## 80% of the sample size
smp_size <- floor(0.80 * nrow(dataset_for_models))

## set the seed to make your partition reproducible
set.seed(42)
train_ind <- sample(seq_len(nrow(dataset_for_models)), size = smp_size)

traininig_set <- dataset_for_models[train_ind, ]
test_set <- dataset_for_models[-train_ind, ]

training_set_for_model = traininig_set %>% select(-CodCliente)
test_set_for_model = test_set %>% select(-y)

table(test_set$y)

########################## GLM ########################
glm_model = glm(y~., family="binomial", data= training_set_for_model)
summary(glm_model)

anova(glm_model, test="Chisq")

glm_model1 = update(glm_model,.~. -provincia_cliente)
summary(glm_model1)

anova(glm_model1, test="Chisq")

glm_model2 =update(glm_model1, .~.-accomp)
summary(glm_model2)

anova(glm_model2, test="Chisq")

glm_model3 = update(glm_model2, .~. -no_card_in_2012)
summary(glm_model3)

anova(glm_model3, test="Chisq")

glm_model4 = update(glm_model3, .~. -nmus13)
summary(glm_model4)

anova(glm_model4, test="Chisq")

glm_model5 = update(glm_model4, .~. -agenzia_tipo)
summary(glm_model5)

anova(glm_model5, test="Chisq")

glm_model6 =update(glm_model5, .~. -nvisit13)
summary(glm_model6)

anova(glm_model6, test="Chisq")

glm_model7 = update(glm_model6, .~. -nvisite0512)
summary(glm_model7)

anova(glm_model7, test="Chisq")

glm_model8 = update(glm_model7, .~. -nmusei0512)
summary(glm_model8)

anova(glm_model8, test="Chisq")

fitted_results_logit <- predict(glm_model8,newdata=test_set_for_model,type='response')
fitted_results_logit <- ifelse(fitted_results > 0.5,1,0)


misClasificError_logit <- mean(fitted_results_logit != test_set$y)
Accuracy_logit = 1-misClasificError_logit
Accuracy_logit

library(ROCR)
library(caret)

pr_logit <- prediction(fitted_results_logit, test_set$y)
prf_logit <- performance(pr_logit, measure = "tpr", x.measure = "fpr")
plot(prf_logit)
auc_logit <- performance(pr_logit, measure = "auc")
auc_logit <- auc_logit@y.values[[1]]
auc_logit


confusionMatrix(data = as.factor(fitted_results_logit), reference = as.factor(test_set$y))


model_probit = glm(y~., family="binomial"(link="probit"), data= training_set_for_model)
model_probit = update(model_probit, .~. -provincia_cliente -accomp -no_card_in_2012 -nmus13
                      -agenzia_tipo -nvisit13 -nvisite0512 -nmusei0512)
summary(model_probit)
anova(model_probit, test="Chisq")

fitted_results_probit <- predict(model_probit,newdata=test_set_for_model,type='response')
fitted_results_probit <- ifelse(fitted_results_probit > 0.5,1,0)


misClasificError_probit <- mean(fitted_results_probit != test_set$y)
Accuracy_probit = 1-misClasificError_probit
Accuracy_probit

pr_probit <- prediction(fitted_results_probit, test_set$y)
prf_probit <- performance(pr_probit, measure = "tpr", x.measure = "fpr")
plot(prf_probit)
auc_probit <- performance(pr_probit, measure = "auc")
auc_probit <- auc_probit@y.values[[1]]
auc_probit

confusionMatrix(data = as.factor(fitted_results_probit), reference = as.factor(test_set$y))

model_loglog = glm(y~., family="binomial"(link="cloglog"), data= training_set_for_model)
model_loglog = update(model_loglog, .~. -provincia_cliente -accomp -no_card_in_2012 -nmus13
                      -agenzia_tipo -nvisit13 -nvisite0512 -nmusei0512)
summary(model_loglog)
anova(model_loglog, test="Chisq")

fitted_results_loglog <- predict(model_loglog,newdata=test_set_for_model,type='response')
fitted_results_loglog <- ifelse(fitted_results_loglog > 0.5,1,0)


misClasificError_loglog <- mean(fitted_results_loglog != test_set$y)
Accuracy_loglog = 1-misClasificError_loglog
Accuracy_loglog

pr_loglog <- prediction(fitted_results_loglog, test_set$y)
prf_loglog <- performance(pr_loglog, measure = "tpr", x.measure = "fpr")
plot(prf_loglog)
auc_loglog <- performance(pr_loglog, measure = "auc")
auc_loglog <- auc_loglog@y.values[[1]]
auc_loglog

confusionMatrix(data = as.factor(fitted_results_loglog), reference = as.factor(test_set$y))


model_log = glm(y~., family="binomial"(link="log"), data= training_set_for_model)
model_log = update(model_loglog, .~. -provincia_cliente -accomp -no_card_in_2012 -nmus13
                      -agenzia_tipo -nvisit13 -nvisite0512 -nmusei0512)
summary(model_log)
anova(model_log, test="Chisq")

fitted_results_log <- predict(model_log,newdata=test_set_for_model,type='response')
fitted_results_log <- ifelse(fitted_results_log > 0.5,1,0)


misClasificError_log <- mean(fitted_results_log != test_set$y)
Accuracy_log = 1-misClasificError_log
Accuracy_log

pr_log <- prediction(fitted_results_log, test_set$y)
prf_log <- performance(pr_log, measure = "tpr", x.measure = "fpr")
plot(prf_log)
auc_log <- performance(pr_log, measure = "auc")
auc_log <- auc_log@y.values[[1]]
auc_log

confusionMatrix(data = as.factor(fitted_results_log), reference = as.factor(test_set$y))

library(glmnet)
x_train<-model.matrix(training_set_for_model$y~., training_set_for_model)
y_train = training_set_for_model$y
cv.ridge<-cv.glmnet(x_train,y_train, alpha=0, family="binomial")
ridge.model<-glmnet(x_train,y_train,alpha=0,family="binomial",lambda=cv.ridge$lambda.min)
test_set_reg = test_set %>% select(-CodCliente)
x_test<-model.matrix(test_set_reg$y~., test_set_reg)
y_test = test_set_reg$y

fitted_results_ridge <- predict(ridge.model,newx=x_test,type='response')
fitted_results_ridge <- ifelse(fitted_results_ridge > 0.5,1,0)


misClasificError_ridge <- mean(fitted_results_ridge != test_set$y)
Accuracy_ridge = 1-misClasificError_ridge
Accuracy_ridge

pr_ridge <- prediction(fitted_results_ridge, test_set$y)
prf_ridge <- performance(pr_ridge, measure = "tpr", x.measure = "fpr")
plot(prf_ridge)
auc_ridge <- performance(pr_ridge, measure = "auc")
auc_ridge <- auc_ridge@y.values[[1]]
auc_ridge

confusionMatrix(data = as.factor(fitted_results_ridge), reference = as.factor(test_set$y))


cv.lasso<-cv.glmnet(x_train,y_train, alpha=1, family="binomial")
lasso.model<-glmnet(x_train,y_train,alpha=1,family="binomial",lambda=cv.ridge$lambda.min)

fitted_results_lasso <- predict(lasso.model,newx=x_test,type='response')
fitted_results_lasso <- ifelse(fitted_results_lasso > 0.5,1,0)


misClasificError_lasso <- mean(fitted_results_lasso != test_set$y)
Accuracy_lasso = 1-misClasificError_lasso
Accuracy_lasso

pr_lasso <- prediction(fitted_results_lasso, test_set$y)
prf_lasso <- performance(pr_lasso, measure = "tpr", x.measure = "fpr")
plot(prf_lasso)
auc_lasso <- performance(pr_lasso, measure = "auc")
auc_lasso <- auc_lasso@y.values[[1]]
auc_lasso

confusionMatrix(data = as.factor(fitted_results_lasso), reference = as.factor(test_set$y))

mean(test_set_with_profits$profit-1)


library(MASS)
library(pROC)
my_roc <- roc(fitted_results_logit, test_set$y)
coords(my_roc,"best", ret = "threshold")

########### RECURSIVE PARTITIONING ################
library(rpart)
library(rpart.plot)

x.rp <- rpart(y ~ ., data=training_set_for_model)
x.rp

x.rp.pred <- predict(x.rp, test_set_for_model)
x.rp.pred <-ifelse(x.rp.pred >=0.5,1,0)
pr_x.rp <- prediction(x.rp.pred, test_set$y)
prf_x.rp <- performance(pr_x.rp, measure = "tpr", x.measure = "fpr")
plot(prf_x.rp)
auc_x.rp <- performance(pr_x.rp, measure = "auc")
auc_x.rp <- auc_x.rp@y.values[[1]]
auc_x.rp

confusionMatrix(data = as.factor(x.rp.pred), reference = as.factor(test_set$y))
rpart.plot(x.rp, box.palette="RdBu", shadow.col="gray", nn=TRUE, main="Decision tree (rpart)")

###################### CONDITIONAL INFERENCE TREE ################

library(party)


x.ct <- ctree(y ~ ., data=training_set_for_model)
x.ct

x.ct.prob = predict(x.ct, newdata=test_set_for_model, type="prob")
predclass_x.ct <- ifelse(x.ct.prob>= 0.5 , 1,0)
pr_x.ct <- prediction(predclass_x.ct, test_set$y)
prf_x.ct <- performance(pr_x.ct, measure = "tpr", x.measure = "fpr")
plot(prf_x.ct)
auc_x.ct <- performance(pr_x.ct, measure = "auc")
auc_x.ct <- auc_x.ct@y.values[[1]]
auc_x.ct
confusionMatrix(data = as.factor(predclass_x.ct), reference = as.factor(test_set$y))


########### BAGGING ###############
library(ipred)
x.ip <- bagging(y ~ ., data=training_set_for_model)
x.ip.prob <- predict(x.ip, type="prob", newdata=test_set_for_model)
class_ip=ifelse(x.ip.prob>=0.5,1,0)

pr_ip <- prediction(x.ip.prob, test_set$y)
prf_ip <- performance(pr_ip, measure = "tpr", x.measure = "fpr")
plot(prf_ip)
auc_ip <- performance(pr_ip, measure = "auc")
auc_ip <- auc_ip@y.values[[1]]
auc_ip

confusionMatrix(data = as.factor(class_ip), reference = as.factor(test_set$y))

########## choice goes to CONDITIONAL INFERENCE TREE, since best overall accuracy and specificity ##

test_set_for_marketing = cbind(test_set,predclass_x.ct)

profit = rep(0, dim(test_set_for_marketing)[1])

test_set_for_marketing = cbind(test_set_for_marketing, profit)

for (i in 1:dim(test_set_for_marketing)[1]){
  test_set_for_marketing$profit[i] = test_set_for_marketing$costo_carta[i]- 
    test_set_for_marketing$total_cost_to_company[i]
}

test_set_for_marketing_ordered = test_set_for_marketing %>% arrange(desc(profit))

test_set_pred_churners = test_set_for_marketing_ordered %>% filter(predclass_x.ct == 1)
test_set_pred_churners_to_contact = test_set_pred_churners %>% filter(profit>2)
triple_contacted = test_set_pred_churners_to_contact[1:258,]
double_contacted = test_set_pred_churners_to_contact[259:2371,]

EXP_cumulative_triple = 0

for (i in 1:dim(triple_contacted)[1]){
  EXP_cumulative_triple = EXP_cumulative_triple + (0.125*(-1)+0.875*(-1+triple_contacted$profit[i]))
}
EXP_cumulative_triple

EXP_cumulative_double = 0

for (i in 1:dim(double_contacted)[1]){
  EXP_cumulative_double = EXP_cumulative_double + (0.125*(-1)+0.875*(-1+double_contacted$profit[i]))
}
EXP_cumulative_double

EXP_total_profit = EXP_cumulative_triple +EXP_cumulative_double

EXP_total_profit

effective_churners_prob = 1674/(1674+904)

effective_churners_prob

ROI = (EXP_total_profit*effective_churners_prob/5000)*100

ROI

##################### PLOTS and GRAPHS ##################
library(RColorBrewer)
theme_set(theme_bw())

#month graph
full_dataset_for_plots = full_dataset
full_dataset_for_plots <- full_dataset_for_plots %>%
  mutate(full_dataset_for_plots, month_1 = months.Date(data)) 

for (i in 1:dim(full_dataset_for_plots)[1]){
  full_dataset_for_plots$month_1[i] = month(full_dataset_for_plots$data[i])
}
full_dataset_for_plots <- full_dataset_for_plots %>% arrange(month_1)

for (i in 1:dim(full_dataset_for_plots)[1]) {
  if (full_dataset_for_plots$month_1[i] == 1){
    full_dataset_for_plots$month_1[i] = 'gennaio'
  }
  if (full_dataset_for_plots$month_1[i] == 2){
    full_dataset_for_plots$month_1[i] = 'febbraio'
  }
  if (full_dataset_for_plots$month_1[i] == 3){
    full_dataset_for_plots$month_1[i] = 'marzo'
  }
  if (full_dataset_for_plots$month_1[i] == 4){
    full_dataset_for_plots$month_1[i] = 'aprile'
  }
  if (full_dataset_for_plots$month_1[i] == 5){
    full_dataset_for_plots$month_1[i] = 'maggio'
  }
  if (full_dataset_for_plots$month_1[i] == 6){
    full_dataset_for_plots$month_1[i] = 'giugno'
  }
  if (full_dataset_for_plots$month_1[i] == 7){
    full_dataset_for_plots$month_1[i] = 'luglio'
  }
  if (full_dataset_for_plots$month_1[i] == 8){
    full_dataset_for_plots$month_1[i] = 'agosto'
  }
  if (full_dataset_for_plots$month_1[i] == 9){
    full_dataset_for_plots$month_1[i] = 'settembre'
  }
  if (full_dataset_for_plots$month_1[i] == 10){
    full_dataset_for_plots$month_1[i] = 'ottobre'
  }
  if (full_dataset_for_plots$month_1[i] == 11){
    full_dataset_for_plots$month_1[i] = 'novembre'
  }
  if (full_dataset_for_plots$month_1[i] == 12){
    full_dataset_for_plots$month_1[i] = 'dicembre'
  }
}
full_dataset_for_plots <- full_dataset_for_plots %>% mutate(agegroup = case_when(eta13 >= 40  & eta13 <= 59 ~ 'Middle-aged Adults',
                                                                                 eta13 >= 18  & eta13 <= 39 ~ 'Young Adults',
                                                                                 eta13 <= 17  & eta13 >= 0 ~ 'Children',
                                                                                 eta13 >=59  ~ "Old Adults"))

ggplot(full_dataset_for_plots) +
  geom_bar(aes(x = month_1, fill = agegroup), position = 'dodge' ) +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = 'Months', y = 'Number of Visitors', title = 'Attendance Trend by Month' )


#---------------------------------------------------------------------

# graph of total visit
dataset_for_models <- dataset_for_models %>%mutate(n_visit12 = case_when(nvisite0512 >= 0  & nvisite0512 <= 100 ~ '100<',
                                                                         nvisite0512 >= 101  & nvisite0512 <= 200  ~ '200<',
                                                                         nvisite0512 >= 201  & nvisite0512 <= 300  ~ '300<',
                                                                         nvisite0512 >= 301  & nvisite0512 <= 400  ~ '400<',
                                                                         nvisite0512 >= 401  & nvisite0512 <= 500  ~ '500<',
                                                                         nvisite0512 >= 501  & nvisite0512 <= 600  ~ '600<',
                                                                         nvisite0512 >= 601  & nvisite0512 <= 700  ~ '700<',
                                                                         nvisite0512 >= 701  & nvisite0512 <= 800  ~ '800<',
                                                                         nvisite0512 >= 801   ~ '800>'))



ggplot(data=dataset_for_models, aes(x=n_visit12, group=n_visit12, fill=n_visit12)) +
  geom_density(adjust=1.5, alpha=.4)+
  scale_fill_brewer(palette = "Set1") +
  labs(x = 'Number of Visits between 2005-2012', y = 'Density', title = 'Culture-Loving Customers' ) +
  facet_wrap(~y)

#----------------------------------------------
#graph for the variable "riduzione"

offer <- rep(0, 70546)
dataset_for_models <- cbind(dataset_for_models, offer)

#BEFORE RUNNING IT, INSERT THE SYMBOL OF EURO ???  IN (funs(gsub("???", "", .)))

dataset_for_models <- dataset_for_models %>% mutate_all(funs(gsub("???", "", .)))

for (i in 1:dim(dataset_for_models)[1]) {
  if (dataset_for_models$riduzione[i] == "ABBONAMENTI MUSEI TORINO"){
    dataset_for_models$offer[i] = 'ab_TO'
  }
  if (dataset_for_models$riduzione[i] == "ABBONAMENTO MUSEI RIDOTTO"){
    dataset_for_models$offer[i] = 'ab_mr'
  }
  if (dataset_for_models$riduzione[i] == "OFFERTA SU QUANTITATIVO 30"){
    dataset_for_models$offer[i] = 'off_30'
  }
  if (dataset_for_models$riduzione[i] == "PASS 60 e VOUCHER OFFERTA 30  "){
    dataset_for_models$offer[i] = 'p_60'
  }
  if (dataset_for_models$riduzione[i] == "ABBONAMENTO RIDOTTO SCONTATO"){
    dataset_for_models$offer[i] = 'ab_rs'
  }
  if (dataset_for_models$riduzione[i] == "OFFERTA SU QUANTITATIVO 44"){
    dataset_for_models$offer[i] = 'off_40'
  }
  if (dataset_for_models$riduzione[i] == "ABBONAMENTO MUSEI OMAGGIO"){
    dataset_for_models$offer[i] = 'ab_mo'
  }
  if (dataset_for_models$riduzione[i] == "OFFERTA CONVENZIONE 28"){
    dataset_for_models$offer[i] = 'off_28'
  }
  if (dataset_for_models$riduzione[i] == "ABBONAMENTO MUSEI su carte EDISU"){
    dataset_for_models$offer[i] = 'ab_mc'
  }
  if (dataset_for_models$riduzione[i] == "ABBONAMENTO MUSEI BUONO" ){
    dataset_for_models$offer[i] = 'ab_mb'
  }
  if (dataset_for_models$riduzione[i] == "OFFERTA CONVENZIONE 33"){
    dataset_for_models$offer[i] = 'off_33'
  }
}

ggplot(dataset_for_models)+
  geom_histogram(aes(x=offer,  fill = offer),stat ="count") +
  facet_wrap(~y) + scale_fill_brewer(palette = "Paired") +
  labs(x = 'Type of Offer', y ='Number of Clients', title = 'Clients Behaviour with Museum Promotions')


unique_offer <- unique(dataset_for_models$offer)
unique_offer
freq <- rep(0, 11)
data_1 <- data.frame(unique_offer, freq)

for (i in 1:dim(dataset_for_models)[1]) {
  if(dataset_for_models$offer[i] %in% data_1$unique_offer){
    a = match(dataset_for_models$offer[i], data_1$unique_offer)
    data_1$freq[a] = data_1$freq[a]+1
  }
}  

# Compute the position of labels
data_1 <- data_1 %>% 
  mutate(prop = freq / sum(data_1$freq) *100) 


# Horizontal version
ggplot(data_1, aes(x=unique_offer, y=prop)) +
  geom_segment( aes(x=unique_offer, xend=unique_offer, y=0, yend=prop), color="orange") +
  geom_point( color="green", fill=alpha("green", 0.3), size=5, alpha=0.7) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+ labs(x = 'Type of Offer', y = 'Percentage', title = 'Museum Promotions' )

#-------------------graph for the variable accomp

ggplot(dataset_for_models) +
  geom_bar(aes(x=accomp, fill = y), position = 'dodge') +
  scale_fill_brewer(palette = "Paired") +
  labs(x = 'Accompanied Clients', y = 'Number of Clients', title = 'Customers Behavior by Group or Alone')

unique_offer <- unique(full_dataset$offer)
unique_offer
freq <- rep(0, 11)
data_1 <- data.frame(unique_offer, freq)

for (i in 1:dim(full_dataset)[1]) {
  if(full_dataset$offer[i] %in% data_1$unique_offer){
    a = match(full_dataset$offer[i], data_1$unique_offer)
    data_1$freq[a] = data_1$freq[a]+1
  }
}  

# Compute the position of labels
data_1 <- data_1 %>% 
  mutate(prop = freq / sum(data_1$freq) *100) 


# Horizontal version
ggplot(data_1, aes(x=unique_offer, y=prop)) +
  geom_segment( aes(x=unique_offer, xend=unique_offer, y=0, yend=prop), color="orange") +
  geom_point( color="green", fill=alpha("green", 0.3), size=5, alpha=0.7) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )+ labs(x = 'Type of Offer', y = 'Percentage', title = 'Museum Promotions' )

cardholders  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())


renewal  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())


use_of_card_ordered  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col(position = "fill")+
  labs(x="Proportion")+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())


#correlation matrix




install.packages("ggpubr")
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library("corrplot")
# Subset numeric columns with dplyr
data_num <- select_if(full_dataset_for_plots, is.numeric) 
data_num
#si2013 column return zero variance , after checking several times, we 
#decided to drop it
cor_1 <- cor(data_num)
cor_1
#correlation coefficients and the p-value of the correlation
cor_1_p <- rcorr(cor_1, type = c("pearson","spearman"))
cor_1_p
#correlgram

corrplot(cor_1, method = "square", type = "upper", tl.col = "black", order = "hclust", col = brewer.pal(n = 5, name = "RdYlBu"))


##some graphs
#full_dataset
#graph of categorical variables

#gender
barplot(table(full_dataset$sesso.y),
        col= c("bisque", "bisque2"),
        xlab="gender",
        ylab="count",
        main="Visitors' Gender",
        ylim=c(0, 500000))

#average spending


tc <- full_dataset_for_plots$total_cost
hist(tc, prob = TRUE,breaks = 100,xlab="Total cost", main="total cost that spent")
x <- seq(min(tc), max(tc), length = 40)
f <- dnorm(x, mean = mean(tc), sd = sd(tc))
lines(x, f, col = "red", lwd = 2)


barplot(table(full_dataset$nvisit13),
        col = brewer.pal(5, name = "Purples"),
        xlab="visit by one person in 2013",
        ylab="number of people",
        main="Number of visits in 2013",
        ylim=c(0,40000))

#age bar chart

full_dataset$year2013 <- 2013

full_dataset<- full_dataset %>%
  mutate(full_dataset, age = as.numeric(year2013) - as.numeric(data_nascita))

full_dataset <- full_dataset %>% mutate(agegroup = case_when(age >= 40  & age <= 59 ~ 'Middle-aged Adults',
                                                             age >= 18  & age <= 39 ~ 'Young Adults',
                                                             age <= 17  & age >= 0 ~ 'Children',
                                                             age >=59  ~ "Old Adults"))

ggplot(full_dataset)+
  geom_histogram(aes(x=age), color="blue", fill="white")+
  labs(y="number of Visitors", x =  "Age of Visitors")

ggplot(full_dataset)+
  geom_bar(aes(x=agegroup), color= "blue", fill= 11)+
  labs(y="number of Visitors", x =  "Age Groups")+
  coord_flip()

