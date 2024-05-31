``` Python
import pandas as pd
```

## Strutture dati in pandas
``` Python
# Esempio di DataFrame (puo' essere vista come una tabella)
data = pd.DataFrame({'Nome':['Mario', 'Luigi'], 'Cognome':['Rossi', 'Luigi'], 'Eta'
					 ['22', '37']})

print('Esempio di DataFrame: ')
print(data)

# Esempio di Series (lista di valori - singola colonna)
data2 = pd.Series(['Mario', 'Luigi'], index['Persona1', 'Persona2'], name='Nomi')

print('Esempio di Series: ')
print(data2)
```

## Lettura di un file CSV

Codice per il montaggio del drive personale per leggere il csv da My Drive
``` Python
from google.colab import drive

drive.mount('/content/drive') 

# df rappresenta il dataset letto da csv
df = pd.read_csv(
    '/content/drive/My Drive/Colab Notebooks/Tutorato/winemage-data-130k-v2.csv',
    index_col=0,
)
```

Se invece **non vi funziona il montaggio del drive**, Google Colab mette a disposizione la cartella sample_data che trovate cliccando l'icona della directory nella barra laterale sinistra. Cliccando sui tre puntini in corrispondenza della cartella "sample_data" potete fare carica e caricare il file csv
``` Python
df = pd.read_csv(
    '/content/sample_data/winemag-data-130k-v2.csv',
    index_col=0,
)
```

In generale
``` Python
df = pd.read_csv(
	'path_cartella/nome_file',
	index_col = 0,
)
```

### Contenuto del dataset
Il dataset riguarda le recensioni di vini, dove ogni riga indica una determinata recensione su un particolare vino. Le colonne sono le seguenti:
- **country**: nazione di derivazione del vino
- **description**: descrizione del vino
- **designation**: vigneto che ha prodotto il vino
- **points**: numero di punti attribuiti a quel vino
- **price**: costo di una bottiglia di quel vino
- **province**: provincia di derivazione del vino
- **region_1**: area vinicola nella provincia o stato
- **region_2**: specifica regione all'interno dell'area vinicola
- **taster_name**: nome del degustatore
- **taster_twitter_handle**: nome utente twitter del degustatore
- **title**: nome del vino
- **variety**: varieta' del vino (es. bianco, rosso, rose')
- **winery**: cantina

## Operazioni che possiamo compiere sul dataset
``` Python
# Attributo columns: lista del nome delle colonne del dataset
df.columns

# Attributo shape: restituisce una tupla contenente come primo elemento il numero di righe, secondo elemento il numero di colonne
n_rows, n_columns = df.shape
print(n_rows)
print(n_columns)
```

``` Python
# Funzione head(): restituisce le prime 5 righe del dataframe (dalle 0 alla 4)
df.head

# Se mettiamo come parametro 2, ci restituisce le righe del dataframe dalla 0 alla 1
df.head(2)
```

``` Python
# Accedere alla colonna price del datgaframe
df.price
df['price']

# dtype e' l'attributo che indica la tipologia di dato contenuta nella colonna price
df.price.dtype

# dtypes restituisce la tipologia di dato per ogni colonna del dataframe
df.dtypes
```

``` Python
"""

Per accedere ai valori contenuti nel dataframe possiamo usare gli indici:
  - possiamo restringerci su una singola colonna (sottoforma di lista) richiamandoci       sulla singola colonna e poi accedere tramite indice
  - possiamo usare le funzioni iloc (se usiamo solo indici numerici) oppure loc (se        usiamo delle labels) per ottenere delle sottomatrici
"""

df.price[1]    # si accede ai vari valori della colonna price

df.iloc[0:3,0:3]    # slezionare sottomatrici utilizzando indici numerici
df.loc[0:3,['country','description','designation']]  # selezionare sottomatrici
													 # utilizzando labels
""" 
possiamo anche usare delle condizoni per ottenere delle righe del dataframe che soddisfano quella condizione
"""
df.loc[(df.country == 'Italy') & (df.points >= 90)] # & -> and, | -> or

"""
isnull() e' la funzione che ci permette di individuare le righe del dataframe che hanno valore NaN nella colonna considerata (in questo caso price)
"""
df.loc[df.price.isnull()]
```

``` Python
# unique() restituisce la lista di valori non ripetuti contenuti nella colonna su cui e' stato richiamato
df.taster_name.unique()

# value_counts() restituisce la lista di valore, dove a ciascun valore viene associato il numero di volte che questo compare nella colonna
df.taster_name.value_counts()
```

``` Python
# Copia del dataset, in modo da non modifcare quello originale
df_taster = df.copy()

# Calcoliamo il numero di righe che contengono con taster_twitter_handle il valore @kerinokeefe
# Una volta selezionate le righe tramite condizone, possiamo suare len per contarle
rows_taster = df_taster.taster_twitter_handle.loc[df_taster.taster_twitter_handle ==
												  '@kerinokeefe']
print("Numero di occorrenze di @kerinokeefe: ", len(rows_taster))

# Un altro metodo e' quello di richiamare sulla colonna taster_twitter_handle la funzine value_counts() 
# che restituisce un dizionario dove a ogni nome utente assovcia la frequeza
# ci accediamo tramite chiave cosi' da ottenere il numero di volte che compare @kerinokeefe
print("Numero di occorreze di @kerinokeefe: ", df_taster.taster_twitter_handle.value_counts()['@kerinokeefe'])

# La fuinzione replace() viene utilizzata per rimpiazzare un valore della colonna con un altro
# Visto che vogliamo che la modifica si rifletta anche sul dataframe, dobbiamo assegnare il risultato alla colonna del dataframe
df_tastrer['taster_twitter_handle'] = df_taster.taster_twitter_handle.replace("@kerinokeefe", "@kerino")
print("Numero di occorrenze di @kerino: ", df_taster.taster_twitter_handle.value_counts()['@kerino'])
```



