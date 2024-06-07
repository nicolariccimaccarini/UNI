## Importazione del dataset
``` Python
import pandas as pd
dataset1 = pd.read_csv('/content/sample_data/winemag-data-130k-v2.csv',index_col=0)
```

## Raggruppare e ordinare i dati
``` Python
# Raggruppa le righe del DataFrame per valore della colonna 'points' e per ogni raggruppamento calcola il numero delle righe
dataset1.groupby('points').points.count()

# Raggruppa le righe del DataFrame per valore della colonna 'points' e per ogni raggruppamento calcola il prezzo minimo tra le righe presenti
dataset1.groupby('points').price.min()

# agg e' una funzione a cui viene indicato un vettore di funzioni
# Queste funzioni vengono utilizzate per calcolare la statistica corrispondente sulla base della colonna indicata
# Quindi raggruppa le righe del DataFrame sulla base delle colonne 'country' e 'province' e poi calcola le statistiche indicate in agg()
dataset1.groupby(['country', 'province']).description.agg([len])
dataset1.groupby(['country', 'province']).price.agg([len, min, max])

# Raggruppa le righe sulla base di 'winery' e per ogni cantina indica il nome del vino della prima riha del raggruppamento
dataset1.groupby('winery').apply(lambda dataset1: dataset1.title.iloc[0])

# Raggruppa le righe sulla base di 'winery' e per ogni cantina calcola il prezzo medio
dataset1.groupby('winery').price.mean()
```

``` Python
# seleziono le righe del dataset che sono della cantna "10 knots"
my_winery = dataset1.loc[dataset1.winery = "10 Knots"]
# print(len(my_winery))
# print(my_winery)

# Utilizziamo reset_index() per restituire gli indici e ripartire da 0
# Prima manteneva l'indice del dataset originale
my_winery = my_winery.reset_index()
# print(my_winery)

# Ordinamento delle righe del dataset sulla base della colona/colonne del DataFrame che sono indicate in by
# L'argomento ascending ci permette di indicare la tipologia di ordinamento
#     - ascending=True -> ordinamento crescente
#     - ascending=False -> ordinamento decrescente
my_winery.sort_values(by='price')    # default: ascending=True
my_winery.sort_values(by='price', ascending=False)
my_winery.sort_values(by=['country','price'])
```

## Gestione dei valori mancanti (`NaN` o `null`)
La gestione dei valori mancanti e' molto delicata e dipende al contesto in cui ci troviamo. Se il numero dei dati e' molto grande e il numero di righe col valore `NaN`e' molto piccolo, allora una scelta conveniente puo' essere eliminare quelle righe. Se invece ci troviamo in cui un'applicazione di machine learning con pochi dati, allora eliminare queste righe non e' la scelta migliore. Possiamo quindi sostituire i valori mancanti con la media, se si tratta di una colonna scomposta da valori numerici oppure il dato piu' frequentante nel caso di valori categoriali.

``` Python
# Prendiamo un esempio che contiene un NaN
# In questo caso, ci viene restituita una riga, dove nella colonna 'price' troviamo NaN
dataset1.loc[dataset1.country == "Egypt"]

# Dimensione del dataset di partenza
print(dataset1.shape)
```

``` Python
# Tecnica 1: elimino le righe (axis=0) che contengono NaN
dfnan = dataset1.copy()
dfnan.dropna(axis=0, inplace=True)

# Stampa della dimensione del dataset, dopo l'eliminazione dei NaN
print(dfan.shape)
# Stampa delle righe che hanno come valore di country Egypt
# Risultato: non ci sono, è stata eliminata a causa del valore NaN in price
print(dfnan.loc[dfnan.country == "Egypt"])
```

``` Python 
#Tecnica 2: eliminiamo le colonne (axis=1) che contengono NaN
dfnan = dataset1.copy()
dfnan.dropna(axis=1, inplace=True)

# Stampa della dimensione del dataset, dopo l'eliminazione dei NaN
# Vediamo una riduzione di colonne
print(dfnan.shape)
```

``` Python
# Tecnica 3: come sostituire ai NaN un valore
dfnan = dataset1.copy()
# Utilizziamo fillna() per sostituire a NaN il valore 0
dfnan.fillna(value=0, inplace=True)

# Stampando le righe con valore "Egypt", vediamo che nella colonna 'price' abbiamo 0.0 al posto di NaN
print(dfnan.loc[dfnan.country == "Egypt"])
```

``` Python
# Esempio per riempire colonne categoriche (cioè che contengono stringhe)
dfnan = dataset1.copy()

# Calcolo il numero di righe che ha valore NaN nella colonna 'country'
n_rows = dfnan[dfnan.country.isnull()].shape[0]
# Ottenere gli indici delle righe che hanno valore NaN nella colonna 'country'
nan_indices = dfnan.loc[dfnan.country.isnull()].index
# Calcolo della moda della colonna 'country'
mode_country = dfnan.country.mode()[0]
# Sostituire ai valori NaN la moda trovata
df_filled = dfnan.fillna({'country':mode_country})

# Varie stampe che potete utilizzare
print(n_rows)
print(nan_indices)
print(mode_country)
print(dfnan.loc[nan_indices,'country'])
print(df_filled.loc[nan_indices,'country'])
```

``` Python
# Esempio per sostituire a valori NaN dei valori numerici
dfnan = dataset1.copy()

# Calcolo il numero di righe che ha valore NaN nella colonna 'price'
n_rows = dfnan[dfnan.price.isnull()].shape[0]
# Ottenere gli indici delle righe che hanno valore NaN nella colonna 'price'
nan_indices = dfnan.loc[dfnan.price.isnull()].index
# Calcolo della media della colonna 'price'
mean_price = dfnan.price.mean()
# Sostituire ai valori NaN la media trovata
df_filled = dfnan.fillna({'price':mean_price})

# Varie stampe
print(n_rows)
print(nan_indices)
print(mean_price)
print(dfnan.loc[nan_indices,'price'])
print(df_filled.loc[nan_indices,'price'])
```

## Scaling e normalization
La differenza tra le due e' che:
- **Scaling**: porta tutti i valori all'interno di un range
- **Normalization**: modifica la distribuzione dei dati

``` Python
from scipy import stats
from mlxtend.preprocessing import minmax_scaling
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
```

``` Python 
# Esempio di scaling
# Otteniamo i valori che vogliamo che siano scalati
ydsnet = np.array(df_filled.loc[df_filled.price < 200].price)
# Scaliamo i dati nel range [0,1]
scaled_data = minmax_scaling(ydsnet, columns=[0])
  
# Plotting del risultato
fig, ax = plt.subplots(1, 2, figsize=(15, 3))
sns.histplot(ydsnet, ax=ax[0], kde=True, legend=False)
ax[0].set_title("Original Data")
sns.histplot(scaled_data, ax=ax[1], kde=True, legend=False)
ax[1].set_title("Scaled data")
plt.show()
```

``` Python
# Esempio di normalizzazione
# Otteniamo i valori che vogliamo normalizzare
ydsnet = np.array(df_filled.loc[df_filled.price < 200].price)
# Normalizzazione dei dati
normalized_data = stats.boxcox(ydsnet)

# Plotting del risultato
fig, ax = plt.subplots(1, 2, figsize=(15, 3))
sns.histplot(ydsnet, ax=ax[0], kde=True, legend=False)
ax[0].set_title("Original Data")
sns.histplot(normalized_data[0], ax=ax[1], kde=True, legend=False)
ax[1].set_title("Normalized data")
plt.show()
```

## Gestione delle date
``` Python
import pandas as pd

# Inseriamo delle date per vedere come gestirle
dates = pd.Series(['2023-05-23','2022-05-20','2021-02-24'])

# Parsing delle date, da stringhe a datetime
date_parsed = pd.to_datetime(dates)

# Cambio del formato delle date
date_parsed = date_parsed.dt.strftime('%d/%m/%Y')
print(date_parsed)

data = pd.to_datetime(dates)
# Ottenimento dei giorni
data_days = data.dt.day
# Ottenimento dei mesi
data_months = data.dt.month
# Ottenimento degli anni
data_years = data.dt.year
  
print(data_days)
print(data_months)
print(data_years)
```