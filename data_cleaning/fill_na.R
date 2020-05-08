#il seguente script ha lo scopo di trattare i comuni dove non è presente il dato sui decessi 
#nel 2020. Secondo l'istat, i comuni per cui non si dispone di dati, sono quelli che hanno avuto 
#nel 2020 meno di 10 morti, oppure un incremento di morti nel 2020 inferiore del 20%
#rispetto al numero di morti medio nello stesso periodo degli anni passati.
#per questo motivo, ora si fornisce una stima ottimistica e pessimistica del numero di decessi
#all'interno di questi comuni, per poi poter svolgere le analisi in seguito.

#lo script restituisce un dataset chiamato [comuni_decessi_marzo.csv]
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(xts)) install.packages("xts") 
if(!require(forecast)) install.packages("forecast")

#si carica il dataset con i decessi giornalieri nei comuni lombardi.
df_raw <- read_csv("comuni_decessi_assoluti.csv")

#si rimuove il mese di aprile
df_raw <- df_raw %>% filter(DATA < as.Date("2020-04-04"))

#ora si salvano tutti i comuni per cui i dati sono mancanti
n_com_NA <- df_raw %>% 
  filter(is.na(DECESSO)) %>% 
  select(NOME_COMUNE) %>%
  unique() %>% nrow()
n_com_TOT <- df_raw %>% 
  select(NOME_COMUNE) %>%
  unique() %>% nrow()
cat("Comuni per cui non si dispone dei decessi a Marzo: ", n_com_NA, 
    "\nSu un totale di: ", n_com_TOT)

# ora, per tutti questi comuni si sa che il numero di decessi:
# - è stato inferiore a 10 dal 4 gennaio al 4 aprile
# - è stato inferiore al 120% del numero di decessi medi degli anni precedenti, 
#   nel periodo 1-marzo/4-aprile
# si propone quindi un intervallo di valori realistico per il numero di decessi all'interno 
# di questi comuni, nel mese di marzo

# come limite superiore si propone il 120% del numero medio di decessi negli anni precedenti
# come limite inferiore si propone il numero medio di decessi per il periodo 4 marzo - 4 aprile 
# meno due volte la deviazione standard della media

# STEP 1
# Creazione di un dataframe con le seguenti colonne:
# | NOME_PROVINCIA | NOME_COMUNE | DECESSI_LOW | DECESSI_HIGH |
# Contenente i decessi di marzo 2020 per ogni comune, 
# Con gli NA per i comuni non disponibili.
dec_m20 <- df_raw %>% filter(DATA >= as.Date("2020-03-01") & DATA <= as.Date("2020-04-01")) %>%
  group_by(NOME_PROVINCIA, NOME_COMUNE) %>% 
  summarise(DECESSI_HIGH = sum(DECESSO), DECESSI_LOW = sum(DECESSO))
dec_m20["AVG"] <- 0
#si nota che il numero di righe del dataset diminuisce, passando da 1505 comuni a 1484
#questo perchè, come si osserva, nel dataset originario alcuni comuni come ad esempio 
#"Battuda" sono presenti solo per pochi giorni dell'anno
#Supponendo che nel resto dei giorni il numero di decessi sia pari a 0, ci aspettiamo 
#che questi comuni non influenzino i risultati finali

#STEP 2
#Riempimento dei valori nulli
estimateHighLow <- function(name, df_tot){
  decessi <- integer(5)
  decessi[1] <- df_raw %>% 
    filter(DATA > as.Date("2015-03-01") & DATA < as.Date("2015-04-04")) %>% 
    filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
  decessi[2] <- df_raw %>% 
    filter(DATA > as.Date("2016-03-01") & DATA < as.Date("2016-04-04")) %>% 
    filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
  decessi[3] <- df_raw %>% 
    filter(DATA > as.Date("2017-03-01") & DATA < as.Date("2017-04-04")) %>% 
    filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
  decessi[4] <- df_raw %>% 
    filter(DATA > as.Date("2018-03-01") & DATA < as.Date("2018-04-04")) %>% 
    filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
  decessi[5] <- df_raw %>% 
    filter(DATA > as.Date("2019-03-01") & DATA < as.Date("2019-04-04")) %>% 
    filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
  
  high_w<- as.integer(mean(decessi)* 31/35 *1.20 + 0.5) 
  low_w <- mean(decessi)*31/35 - 2*sd(decessi) + 0.5 
  low_w <-ifelse(as.integer(low_w) > 0, as.integer(low_w),0)
  
  prev=list("H"=high_w,"L"=low_w)
  return(prev)
}

# estimate_low <- function(name, df_tot){
#   decessi <- integer(5)
#   decessi[1] <- df_raw %>% 
#     filter(DATA > as.Date("2015-03-01") & DATA < as.Date("2015-04-04")) %>% 
#     filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
#   decessi[2] <- df_raw %>% 
#     filter(DATA > as.Date("2016-03-01") & DATA < as.Date("2016-04-04")) %>% 
#     filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
#   decessi[3] <- df_raw %>% 
#     filter(DATA > as.Date("2017-03-01") & DATA < as.Date("2017-04-04")) %>% 
#     filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
#   decessi[4] <- df_raw %>% 
#     filter(DATA > as.Date("2018-03-01") & DATA < as.Date("2018-04-04")) %>% 
#     filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
#   decessi[5] <- df_raw %>% 
#     filter(DATA > as.Date("2019-03-01") & DATA < as.Date("2019-04-04")) %>% 
#     filter(NOME_COMUNE == name) %>% select(DECESSO) %>% sum()
#   
#    %>% return()
# }

##############################################################################



##############################################################################



pd <- txtProgressBar(min = 1, max = nrow(dec_m20), style = 3)
for(i in 1:nrow(dec_m20)){
  setTxtProgressBar(pd, i)
  if(is.na(dec_m20[i, "DECESSI_HIGH"])){
    parameters=estimateHighLow(dec_m20[i, "NOME_COMUNE"] %>% as.character(), df_raw)
    dec_m20[i, "DECESSI_HIGH"]=parameters["H"]
    dec_m20[i, "DECESSI_LOW"] = parameters["L"]
  }
  dec_m20$AVG[i]=mean(dec_m20$DECESSI_HIGH[i], dec_m20$DECESSI_LOW[i])
}

close(pd)
#STEP 3 - Scrittura del dataframe
view(dec_m20)
write_csv(dec_m20, "comuni_decessi_marzo.csv")

