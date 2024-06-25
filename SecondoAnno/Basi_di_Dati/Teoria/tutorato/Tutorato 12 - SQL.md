![[tutorato12.png]]

1. Quanti biglietti ha acquistato Matteo Rossi nel 2023?
2. Quali sono i proprietari della giostra che hanno acquistato anche dei biglietti?
3. Quali sono le persone che hanno acquistato piu' di 2 biglietti?
4. Ordinare le giostre per data di fabbricazione.
5. Quali sono le persone che non hanno acquistato biglietti?
6. Giostra o giostre per cui sono stati acquistati meno biglietti nel 2023?
7. Per ogni provincia di persona indicare il numero di biglietti acquistati 

## QUERY

1. 
``` MySQL
SELECT COUNT(*)
FROM PERSONA AS P JOIN BIGLIETTO AS B
     ON P.cf=B.cf_acq
WHERE nome='Matteo' AND cognome='Rossi' AND data>='2023-01-01' AND data<='20233-12-31';
```

2. 
``` MySQL
SELECT CF, nome, cognome
FROM GIOSTRA JOIN PERSONA ON CF_prop=CF
	 JOIN BIGLIETTO ON CF=CF_acq;
```

3. 
``` MySQL
SELECT CF, nome, cognome
FROM PERSONA JOIN BIGLIETTO ON CF=CF_acq
GROUP BY CF_acq
HAVING COUNT(*)>2;
```

4. 
``` MySQL
SELECT numero_serie, data_fabbr
FROM GIOSTR
ORDER BY data_fabbr ASC;
```

5. 
``` MySQL
SELECT CF, nome, cognome
FROM PERSONA
WHERE CF NOT IN (
	SELECT CF_acq
	FROM BIGLIETTO
);
```

6. 
``` MySQL
SELECT numero_serie, capienza, COUNT(*)
FROM GIOSTRA JOIN BIGLIETTO AS B1
	 ON numero_serie=B1.ns_giostra
WHERE B1.data>='2023-01-01' AND B1.data<='2023-12-31'
GROUP BY B1.ns_giostra
HAVING COUNT(*)=(
	SELECT MIN(cb_23)
	FROM(
		SELECT COUNT(*) AS cb_23
		FROM BIGLIETTO AS B2
		WHERE YEAR(B2.data)	=2023
		GROUP BY B2.ns_giostra
	) AS CONTEGGI
)
```

7. 
``` MySQL
SELECT provincia, COUNT(*)
FROM PERSONA JOIN BIGLIETTO ON CF=CF_acq
GROUP BY provincia;
```