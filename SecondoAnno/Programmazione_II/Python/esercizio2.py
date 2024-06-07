lista_numeri = list(range(2, 8))

# Ciclo la lista per sommare i valori in essa contenuti e stampo il risultato
somma = 0
for numero in lista_numeri:
    somma += numero
print("Somma dei valori nella lista:", somma)

# Stampo il contenuto della lista
print("Contenuto della lista prima della modifica:", lista_numeri)

# Ciclo la lista e modifico i suoi valori sommando ad ogni elemento il risultato della somma ottenuta precedentemente
for i in range(len(lista_numeri)):
    lista_numeri[i] += somma

# Stampo il contenuto della lista dopo la modifica
print("Contenuto della lista dopo la modifica:", lista_numeri)