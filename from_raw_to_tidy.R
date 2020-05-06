#il seguente script prende i dati da comune_raw.csv,
#li manipola per ottenere un formato più comodo ai fini dell'
#analisi e lo integra con i dataset della popolazione
#per aggiungere la colonna "DECESSI_P10k", che indica il numero di 
#decessi giornalieri ogni 10k abitanti.

#restituisce 6 dataset:
# - Numero di decessi giornalieri per 10k abitanti di sesso femminile in tutta la lombardia 
#   [lombardia_giorno_femmine.csv]
# - Numero di decessi giornalieri per 10k abitanti di sesso maschile in tutta la lombardia 
#   [lombardia_giorno_maschi.csv]
# - Numero di decessi giornalieri per 10k abitanti in tutta la lombardia 
#   [lombardia_giorno_totale.csv]
# - Numero di decessi giornalieri per 10k abitanti di ogni sesso per le province lombarde
#   [province_giorno.csv]
# - Numero di decessi giornalieri per 10k abitanti di ogni sesso per i principali comuni lombardi
#   [comuniPrincipali_giorno.csv]
# - Numero di decessi giornalieri in valore assoluto di ogni sesso per i comuni lombardi
#   [comuni_decessi_assoluti.csv]

if( !require(tidyverse)) install.packages("tidyverse")
if( !require(lubridate)) install.packages("lubridate")

# si importa il dataset con i decessi suddivisi per comune
db <- read_csv("data/raw/comune_raw.csv", 
                 locale = locale(encoding = "ISO-8859-1"))

#vengono mantenuti solo i dati relativi alla lombardia
#vengono rimosse le colonne col nome della regione
lomb1 <- db %>%  filter(NOME_REGIONE == "Lombardia") 
lomb1 <- select(lomb1, -c("REG","NOME_REGIONE"))

#ora vengono svolte una serie di operazioni sul dataset lomb1 per renderlo più facile da usare
#il dataset finale avrà
#\ DATA  \ NOME_PROVINCIA \ NOME_COMUNE  \ SESSO \ DATA_INIZIO_DIFF \ DECESSI
#1 si raggruppa il numero di decessi in 2 colonne
lomb1 <- lomb1 %>% gather(key = "SESSO_ANNO", value = "DECESSO",MASCHI_15:TOTALE_20)
#2 si divide la colonna "SESSO_ANNO"
lomb1 <- lomb1 %>% separate(SESSO_ANNO, c("SESSO", "ANNO"), "_") 
#4 si eliminano i dati mancanti
lomb1 <- lomb1 %>% mutate(DECESSO = ifelse(DECESSO == 9999, NA, DECESSO))
#5 si formatta la data
lomb1 <- lomb1 %>% mutate(DATA = as.Date(paste0(lomb1$GE,lomb1$ANNO), format = "%m%d%y"))
#5.1 si escludono le date inesistenti
lomb1 <- lomb1 %>% filter(!is.na(DATA))
#6 si rimuove la colonna relativa alla classe d'età e si sommano i decessi
comuni_mf <- lomb1 %>% select(-CL_ETA) %>% 
  group_by(DATA, NOME_PROVINCIA, NOME_COMUNE, SESSO, DATA_INIZIO_DIFF) %>% 
  summarise(DECESSO = sum(DECESSO)) %>% 
  as.data.frame() 

#si effettuano due controlli per assicurarsi che tutto sia andato a buon fine
#1 il numero di comuni in lomb1 è lo stesso di comuni_mf
n_c_lomb1 <- unique(lomb1$NOME_COMUNE) %>% length()
n_c_comuni_mf <- unique(comuni_mf$NOME_COMUNE) %>% length()
if(n_c_lomb1 != n_c_comuni_mf) warning("Numero di comuni in [lomb1] e [comuni_mf] diverso.")
#2 il numero totale di decessi è lo stesso dei due datasets
n_d_lomb1 <- lomb1 %>% filter(SESSO == "TOTALE" & !is.na(DECESSO)) %>% select(DECESSO) %>% filter() %>% sum()
n_d_comuni_mf <- comuni_mf %>% filter(SESSO == "TOTALE" & !is.na(DECESSO)) %>% select(DECESSO) %>% sum()
if(n_d_lomb1 != n_d_comuni_mf) warning("Numero di decessi totali in [lomb1] e [comuni_mf] diverso.")

#ora si crea il dataset con i decessi e la popolazione giornaliera relativi ai comuni italiani
#per molti comuni non è possibile ottenere i dati completi, in quanto non esistono all'inizio
#dell'intervallo temporale considerato, oppure hanno cambiato struttura
#tuttavia, per i PRINCIPALI comuni italiani è possibile analizzare i dati
pop_com <- read_csv("data/raw/popolazione_comuni_giorno.csv")
comuniPrincipali_giorno <- comuni_mf %>%
  group_by(NOME_PROVINCIA, NOME_COMUNE, DATA, SESSO) %>% 
  summarise(DECESSI = sum(DECESSO)) %>% 
  left_join(y = pop_com, by = c("NOME_COMUNE", "DATA", "SESSO")) %>% 
  as.data.frame()
  
comuniPrincipali_giorno <- comuniPrincipali_giorno %>% 
  mutate(DECESSI_P10k = ifelse(is.na(POPOLAZIONE), NA, DECESSI*10000/POPOLAZIONE)) %>% 
  select(NOME_PROVINCIA, NOME_COMUNE, DATA, SESSO, DECESSI, POPOLAZIONE, DECESSI_P10k)

#ora si crea il dataset con i decessi e la popolazione giornaliera relativi alle province italiane
pop_prov <- read_csv("data/raw/popolazione_province_giorno.csv")
province_giorno <- comuni_mf %>% 
  group_by(NOME_PROVINCIA, DATA, SESSO) %>% 
  summarise(DECESSI = sum(DECESSO)) %>% 
  left_join(y = pop_prov, by = c("NOME_PROVINCIA", "DATA", "SESSO")) %>% 
  as.data.frame()

province_giorno <- province_giorno %>% 
  mutate(DECESSI_P10k = ifelse(is.na(POPOLAZIONE), NA, DECESSI*10000/POPOLAZIONE)) %>% 
  select(NOME_PROVINCIA, DATA, SESSO, DECESSI, POPOLAZIONE, DECESSI_P10k)
#diversamente dai comuni, in questo caso è necessario avere tutti i dati, eccetto per l'anno 2020
#dove non sono ancora disponibili, si effettua un controllo.
n_NA_popolazione <- province_giorno[is.na(province_giorno$POPOLAZIONE) & year(province_giorno$DATA) != 2020, ] %>% 
  unique() %>% nrow()
if(n_NA_popolazione) warning("NA values in pr_tot.")

#ora si ricavano i 3 dataset relativi alla lombardia
lomb_list <- province_giorno %>% 
  group_by(DATA, SESSO) %>% 
  summarise(DECESSI = sum(DECESSI), POPOLAZIONE = sum(POPOLAZIONE)) %>% 
  mutate(DECESSI_P10k = ifelse(is.na(POPOLAZIONE), NA, DECESSI*10000/POPOLAZIONE)) %>% 
  group_by(SESSO) %>% 
  group_split() 

#infine si salvano tutti i dataset ricavati
write.csv(lomb_list[[1]], "data/lombardia_giorno_femmine.csv")
write.csv(lomb_list[[2]], "data/lombardia_giorno_maschi.csv")
write_csv(lomb_list[[3]], "data/lombardia_giorno_totale.csv")
write.csv(province_giorno, "data/province_giorno.csv")
write.csv(comuniPrincipali_giorno, "data/comuniPrincipali_giorno.csv")
write.csv(comuni_mf, "data/comuni_decessi_assoluti.csv")
