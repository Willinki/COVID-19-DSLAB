#lo script prende i dati a partire dal dataset delle popolazioni mensili
#dei comuni lombardi, suddivise per sesso
#restituisce un dataset contenente le stesse popolazioni ma con frequenza gionaliera
#interpolando linearmente
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(zoo)) install.packages("zoo")

#questa è la funzione che ha come argomento un dataset contenente:
#-un singolo comune
#-uno fra MASCHI, FEMMINE, TOTALE
#Restituisce lo stesso dataset ma con popolazione interpolata
#linearmente con frequenza giornaliera
expand <- function(df_g){
  #salvato l'intervallo temporale a cui si rifeerisce
  min_date <- min(df_g$DATE)
  max_date <- max(df_g$DATE)
  #salvato il territorio e il sesso del dataframe
  terr <- df_g$Territorio[1]
  sex <- df_g$Sesso_c[1]
  print(terr)
  #si crea un dataframe con sesso e territorio ripetuti
  #per ogni giorno compreso tra data min e max
  daily_df <- data.frame(Territorio = rep(terr), 
                         Sesso_c = rep(sex), 
                         DATE = seq(from = min_date, to = max_date, by = "day"))
  #si effettua un left join con il dataframe originale
  daily_data <- left_join(x = daily_df, 
                          y = df_g[, c("DATE", "Value_i")], 
                          by = "DATE")
  #si interpola linearmente e si esclude la colonna con NA
  daily_data <- daily_data %>% 
    mutate(Value = as.integer(na.approx(Value_i))) %>% 
    select(-Value_i)
  
  
  return(daily_data)
}

forecast_2020 <- function(df_c){
  if(max(df_c$DATE) != as.Date("2019-11-01")) return(df_c)
  min_date <- df_c$DATE[1]
  max_date <- as.Date("2020-04-01", format = "%Y-%m-%d")
  tsex <- df_c$Sesso_c[1]
  terr <- df_c$Territorio[1]
  dates <- seq(min_date, max_date, by="months")
  print(terr)
  
  d_ts <- as.xts(df_c$Value_i, order.by = as.Date(df_c$DATE, format = "%Y-%m-%d"))
  model1 <- auto.arima(d_ts)
  df_c_pred <- forecast(model1, h = 5) 
  Value_extended <- c(df_c$Value_i, as.integer(df_c_pred$mean)) 
  df_c_extended <- data.frame(Territorio = rep(terr), 
                              Sesso_c = rep(tsex), 
                              DATE = dates, 
                              Value_i = Value_extended)
  return(df_c_extended)
}

#il dataset viene manipolato
t_mf <- read_csv(file ="data/raw/popolazione_raw.csv") %>% 
  select("Territorio", "Sesso", "TIME", "Value", "ITTER107") %>% 
  mutate(DATE = as.Date(paste0(TIME, "-01"), format = "%Y-%m-%d")) %>% 
  mutate(Value_i = as.integer(Value)) %>% 
  mutate(Sesso_c = toupper(Sesso)) %>% 
  select(-TIME, -Value, -Sesso) %>% 
  filter(Sesso_c == "TOTALE")

#creo un dataset con le province e uno con i comuni
p_mf <- t_mf %>% filter(str_detect(ITTER107, "IT"))
c_mf <- t_mf %>% filter(!str_detect(ITTER107, "IT"))

#viene poi suddiviso, ad ognuno viene applicata la funzione expand, 
#e in seguito ricombinato
l_final <- p_mf %>% select(-ITTER107) %>% 
  group_by(DATE, Sesso_c) %>% 
  summarise(Value_i = sum(Value_i), 
            Territorio = rep("Lombardia")) %>%
  as.data.frame() %>%
  forecast_2020() %>%
  expand()

cm_final <- c_mf %>% select(-ITTER107) %>% 
  group_by(Territorio, Sesso_c) %>% 
  group_split() %>% 
  lapply(forecast_2020) %>% 
  lapply(expand) %>%
  bind_rows()

#le colonne vengono infine rinominate per essere uguali 
#al dataset sui decessi
names(cm_final) <- c("NOME_COMUNE", "SESSO", "DATA", "POPOLAZIONE")
names(l_final) <- c("REGIONE" , "SESSO", "DATA", "POPOLAZIONE")

plot(l_final$Value)
#viene salvato come .csv
write_csv(l_final, path = "data/raw/popolazione_lombardia_giorno.csv")
write_csv(cm_final, path = "data/raw/popolazione_comuni_giorno.csv")
