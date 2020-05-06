# COVID-19-DSLAB
Data Science Project about the impact of Covid-19 on Lombardy.

2/05
- Initial **data cleaning** in order to start with data analysis.
  
3/05
# TO DO
- raggruppare le classi d'eta (poichè mancano nel dataset che contiene il numero di abitanti per un determinato comune).
  - per farlo utilizza una group by per nome comune %>% DATA %>% SESSO, collapse con somma
  
- Integrazione con dataset Popolazione comuni lombardia [full inner join ]
  cosi otteniamo informazioni circa: 
  > il numero di abitanti di quel comune distinti per maschi e femmine e le morti nei diversi anni di maschi e femmine.
    Con queste informazioni andremo a calcolare il numero di morti per ogni giorno pesandolo con il numero degli abitanti rispettivo.

- Una volta arrivati ad avere quindi il numero di morti procapite, si potrà iniziare a costruire una serie storica (giornaliera) per tutti gli anni.
  > In pratica avremo a disposzione i morti procapite dal 1 gennaio al 30 aprile (**salvo dati mancanti**). Si è deciso di stimare il numero di morti causati dal
  nuovo covid-19, con la differenza dei morti tra gli anni 15,...19, di fatto sottraendo il numero di morti non dovuti alla pandemia.

## PER PIETRO 06-05 
Ti scrivo qui due cose, al posto che scriverti un messaggio noioso a mezzanotte.

- Nello script fill_NA.R c'è una parte bruttina con un cilo for. Non ho saputo scriverlo meglio. Ho provato con dei mutate ma niente da fare. Se riesci a farlo meglio è ottimo.
Altrimenti va bene così.

- Tutto quello che ho fatto è nel notebook finale. Forse bisognerebbe essere un po' più cauti. Quando prendo l'intercetta del modello lineare, uso come incertezza la sua deviazione standard.
Intendo nel modello per stimare le morti SENZA covid. Quello della serie storica. Forse sarebbe meglio usare come incertezza 2 o 3 volte la deviazione standard... visti i risultati.

- Per il resto dovrebbe esserci tutto. Ho cercato di commentare abbastanza bene. 
