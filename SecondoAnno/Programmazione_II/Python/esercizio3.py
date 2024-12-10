from numpy import array 

array_numeri = array(range(2, 8))

# Ciclo l'array per sommare i valori in esso contenuti e stampo il risultato
somma = 0
for numero in array_numeri:
    somma += numero
print("somma dei valori nell array: ", somma)

# Stampo il contenuto dell'array
print("Contenuto dell array prima delle modifiche: ", array_numeri)

# Ciclo l'array e modifico i suoi valori sommando ad ogni elemento il risultato della somma ottenuta precedentemente
for i in range(len(array_numeri)):
    array_numeri[i] += somma

# Stampo il contenuto dell array dopo la modifica
print("Contenuto dell array dopo la modifica: ", array_numeri)