![[tutorato11.png]]

1. Cliente o clienti con il limite di credito piu' alto
2. Per ogni cliente indicare il tasso di rischio calcolato come il 70% del limite del credito
3. Selezionare i clienti con partita IVA
4. Per ogni conto indicare la media dell'ammontare delle transazioni
5. Selezionare i tipi di transazione fatti dal cliente con `num_cliente='CL010'`
6. Per ogni anno indicare il numero di transizioni fatte e ordinarle dall'anno con piu' transizioni a quello con meno transizioni
7. Per ogni cliente associare il conto di cui e' titolare (indicare anche i clienti a cui non e' associato alcun conto)
8. Clienti che hanno un solo conto
9. Clienti che hanno piu' di 2 conti
10. Restituire il conto o i conti che hanno fatto meno transizioni nel 2023
11. Per l'anno 2023, indicare il cliente che ha fatto il maggior numero di transizioni e indicarne la somma del loro ammontare
12. Tutte le transizioni del cliente con numero cliente 'CL001' che sono state fatte tra l'1 gennaio 2023 e il 28 febbraio 2023 e che hanno un ammontare > 50000€


## QUERY

1. 
``` MySQL
SELECT num_cliente, limite_credito
FROM CLIENTE
WHERE limite_credito=(
	SELECT MAX(limite_credito)
	FROM CLIENTE
)
```

2. 
``` MySQL
SELECT num_cliente, 0.7*limite_credito
FROM CLIENTE
GROUP BY num_cliente
```

3. 
``` MySQL
SELECT *
FROM CLIENTE
WHERE partita_iva IS NOT NULL
```

4. 
``` MySQL
SELECT num_conto, AVG(ammontare)
FROM OPERAZIONE AS O, TRANSIZIONE AS T
WHERE O.num_transazione=T.num_transazione
GROUP BY num_conto
```

5. 
``` MySQL
SELECT DISTINCT tipo
FROM TRANSIZIONE AS TR JOIN
	 OPERAZIONE AS O ON TR.num_transizione=O.num_transizione
	 JOIN CONTO AS C ON O.num_conto=C.num_conto
	 JOIN TITOLARITA AS TI ON C.num_conto=TI.num_conto
WHERE TI.num_cliente='CL010'
```

6. 
``` MySQL
SELECT YEAR(data), COUNT(*) AS CT
FROM TRANSIZIONE
GROUP BY YEAR(data)
ORDER BY CT DESC
```

7. 
``` MySQL
SELECT num_cliente, num_conto
FROM CLIENTE LEFT JOIN TITOLARITA ON CLIENTE.num_cliente=TITOLARITA.num_cliente
```

8. 
``` MySQL
SELECT num_cliente
FROM TITOLARITA
GROUP BY num_cliente
HAVING COUNT(*)=1
```

9. 
``` MySQL
SELCET num_cliente
FROM TITOLARITA
GROUP BY num_cliente
HAVING COUNT(*)>2
```

10. 
``` MySQL
SELECT num_conto
FROM OPERAZIONE AS O JOIN TRANSIZIONE AS TR
	 ON O.num_transizione = TR.num_transizione
WHERE TR.data>='20233-01-01' AND TR.data <= '2023-12-31'
GROUP BY num_conto
HAVING COUNT(*)=(
	SELECT MIN(count_tr)
	FROM (
		SELECT COUNT(num_transazione) AS count_tr
		FROM OPERAZIONE AS O JOIN TRANSIZIONE AS TR
		ON O.num_transizione=TR.num_transizione
		WHERE TR.data>='20233-01-01' AND TR.data <= '2023-12-31'
		GROUP BY num_conto
	) AS CTR
)
```

11. 
``` MySQL
SELECT num_cliente, SUM(ammontare)
FROM OPERAZIONE AS O JOIN TRANSIZIONE AS TR
	 ON O.num_transizione=TR.num_transizione
	 JOIN CONTO AS C ON O.num_conto=C.num_conto
	 JOIN TITOLARITA AS TI ON C.num_conto=TI.num_conto
WHERE TR.data >= '2023-01-01' AND TR.data <= '2023-12-3'
GROUP BY num_cliente
HAVING COUNT(*)=(
	SLECT MAX(count_tr)
	FROM (
		SELECT COUNT(num_transazione) AS count_tr
		FROM OPERAZIONE AS O JOIN TRANSIZIONE AS TR
		ON O.num_transizione=TR.num_transizione
		JOIN CONTO AS C ON O.num_conto=C.num_conto
		JOIN TITOLARITA AS TI ON C.num_conto=TI.num_conto
		WHERE TR.data >= '2023-01-01' AND TR.data <= '2023-12-3'
		GROUP BY num_cliente
	) AS CTR
)
```

12. 
``` MySQL
SELECT TRANSIZIONE.*
FROM TRANSIZIONE AS TR JOIN OPERAZIONE AS O
	 ON TR.num_transizione=O.num_transizione
	 JOIN CONTO AS C ON O.num_conto=C.num_conto
	 JOIN TITOLARITA AS TI ON C.num_conto=TI.num_conto
WHERE TI.num_cliente='CL001' AND 
      TR.data >= '2023-01-01' AND
      TR.data <= '2023-02-08' AND
      TR.ammontare > 5000
```