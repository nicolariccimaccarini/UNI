![[tutorato10.png]]

1. Seleziona tutti gli invitati che sono +1
2. Seleziona tutti i cibi che hanno piu' di 1000 calorie
3. Seleziona il nome e cognome di tutti gli invitati presenti alla festa e salva il risultato come `lista_partecipanti`
4. Numero di persone al turno con orario 09:00:00
5. Numero di persone a cui dovra' dare un passaggio il guidatore con CF = CF1
6. Allergeni non presenti alla festa
7. Cibi e bevande (vogliamo solo denominazione e marca) che sono senza glutine
8. La bevanda o le bevande piu' costose tra quelle presenti
9. Selezionare nome e cognome degli invitati che sono allergici ad almeno un allergene
10. Selezionare tutti i turni del planetario vuoti
11. Selezionare tutti gli invitati che sono guidatori
12. Selezionare tutti gli invitati minorenni
13. Associare ad ogni invitato il relativo turno, lasciando tutte le info del turno
14. Tabella che associa ogni persona ai cibi che non possono mangiare e/o bere

## QUERY
1. 
``` MySQL
SELECT *
FROM INVITATO
WHERE plus1=TRUE
```

2. 
``` MySQL
SELECT *
FROM CIBO
WHERE calorie>100
```

3. 
``` MySQL
SELECT nome, cognome
FROM INVITATO 
WHERE presenza=TRUE
```

4. 
```MySQL
SELECT COUNT(*)
FROM INVITATO
WHERE ora_turno='09:00:00'
```

5. 
``` MySQL
SELECT COUNT(*)
FROM INVITATO
WHERE CF!=CF1 AND CF_guidatore=CF1
```

6. 
``` MySQL
SELECT *
FROM ALLERGENE
WHERE ALLERGENE.denominazione NOT IN (
	(SELECT DISTINCT den_allergene
	 WHERE CIBO_ALLERGENE)
	 UNION
	 (SELECT DISTINCT den_allergene
	  WHERE BEVANDA_ALLERGENE)
)
```

7. 
``` MySQL
SELECT den, marca
FROM (
	(SELECT den_cibo AS den, marca_cibo AS marca, den_allergene
	 FROM CIBO_ALLERGENE)
	 UNION
	 (SELECT den_bev AS den, marca_bev AS marca, den_allergene
	  FROM BEVANDA_ALLERGENE)
) AS CB
WHERE (den_marca) NOT IN (
	(SELECT den_cibo, marca_cibo
	 FROM CIBO_ALLERGENE
	 WHERE den_allergene='glutine')
	 UNION
	 (SELECT den_bev, marca_bev 
	  FROM BEVANDA_ALLERGENE
	  WHERE den_allergene='glutine')
)
```

8. 
``` MySQL
SELECT *
FROM BEVANDA
WHERE prezzo=(
	SELECT MAX(prezzo)
	FROM BEVANDA
)
```

9. 
``` MySQL
SELECT DISTINCT nome, cognome
FROM INVITATO, INVITATO_ALLERGENE
WHERE CF=FC_invitato
```

10. 
``` MySQL
"Query per indicare l'ora del turno"
SELECT ora_turno, COUNT(*)
FROM INVITATO
GROUP BY(ora_turno)

SELECT *
FROM TURNO
WHERE ora NOT IN(
	SELECT DISTINCT ora_turno
	FROM INVITATO
)
```

11. 
``` MySQL
SELECT nome, cognome
FROM INVITATO
WHERE CF=CF_giudatore
```

12. 
``` MySQL
SELECT nome, cognome
FROM INVITATO
WHERE CF=CF_giudatore
```

13. 
``` MySQL
SELECT *
FROM INVITATO, TURNO
WHERE ora_turno=ora
```

14. per ogni invitato tutti i cibi che non puo' mangiare
``` MySQL
SELECT nome, cognome, den_cibo, marca_cibo
FROM CIBO_ALLERGENE(
	SELECT *
	FROM INVITATO, INVITATO_ALLERGENE
	WHERE CF=CF_invitato
) AS INV_ALL
WHERE INV_ALL.den_allergene=CIBO_ALLERGENE.den_allergene
```

per ogni invitato, tutte le bevande che non puo' bere
``` MySQL
SELECT nome, cognome, den_bev, marca_bev
FROM BEVANDA_ALLERGENE (
	SELECT *
	FROM INVITATO, INVITATO_ALLERGENE
	WHERE CF=CF+invitato
) AS INV_ALL
WHERE INV_ALL.den_allergene=BEVANDA_ALLERGENE.den_allergene
```

