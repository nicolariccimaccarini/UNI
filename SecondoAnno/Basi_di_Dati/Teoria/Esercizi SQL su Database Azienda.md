![[Stato_DB_Azienda.png]]

00. Aggiungere alla tabella Dipartimento una nuova colonna "BUDGET", ai dipendenti gia' creati deve essere assegnato un budget di 30000
``` MySQL
ALTER TABLE DEPARTMENT ADD BUDGET INT DEFAULT 30000;
```

1. Aggiungere un nuovo Dipartimento e un nuovo impiegato
``` MySQL
INSERT INTO DEPARTMENT (Dname, Dnumber, Mgr_ssn, Mgr_start_date, Budget)
VALUES ('Quality Assurance', 3, 'MRRMRY65F34G876Q', '1998-05-22', 65000);

INSERT INTO EMPLOYEE (Fname, Minit, Lname, Ssn, Bdate, Address, Sex, Salary, Super_ssn, Dno)
VALUES ('Mary', 'M', 'Moore', 'MRRMRY65F34G876Q', '1965-08-10', '4562 Maccheroni, Houston, TX', 'F', 23000, 'MRRMRY65F34G876Q', 3);
```

2. Recuperare il cognome di tutti gli impiegati senza duplicati
``` MySQL
SELECT DISTINCT Lname
FROM EMPLOYEE;
```

3. Recuperare tutte le informazioni degli impiegati con cognome Smith
``` MySQL
SELECT *
FROM EMPLOYEE
WHERE Lname = 'Smith';
```

4. Recuperare tutte le informazioni degli impiegati il cui cognome e' "Smith" o "Borg"
``` MySQL
SELECT *
FROM EMPLOYEE
WHERE Lname in ('Smith', 'Borg');
```

5. Selezionare tutti i dati degli impiegati che lavorano nel dipartimento 5
``` MySQL
SELECT *
FROM EMPLOYEE
WHERE Dno = 5;
```

6. Recuperare tutte le informazioni degli impiegati che lavorano nei dipartimenti 1 e 5
``` MySQL
SELECT *
FROM EMPLOYEE 
WHERE Dno in (1, 5);
```

7. Recuperare tutte le informazioni degli impiegati il cui cognome inizia con "S"
``` MySQL
SELECT *
FROM EMPLOYEE
WHERE Lname LIKE 'S%';
```

8. Recupera la somma di tutti i budget dei dipartimenti
``` MySQL
SELECT SUM(Budget) AS Total_Budget
FROM DEPARTMENT;
```

9. Recupera il numero di impiegati di ogni dipartimento
``` MySQL
SELECT Dno, COUNT(*) AS Num_Employees
FROM EMPLOYEE
GROUP BY Dno;
```

10. Recupera tutte le informazioni degli impiegati includendo le informazioni del dipartimento in cui lavorano
``` MySQL
SELECT E.*, D.*
FROM EMPLOYEE AS E
JOIN DEPARTMENT AS D ON E.Dno = D.Dnumber;
```

11. Recupera il nome e il cognome di ogni impiegato, insieme al nome e al budget del dipartimento in cui lavora
``` MySQL
SELECT E.Fname, E.Lname, D.Dname, D.budget
FROM EMPLOYEE AS E
JOIN DEPARTMENT AS D ON E.Dno = D.Dnumber;
```

12. Recupera il nome e il cognome di ogni impiegato che lavora in un dipartimento con budget > 60000
``` MySQL
SELECT E.Fname, E.Lname
FROM EMPLOYEE AS E
JOIN DEPARTMENT AS D ON E.Dno = D.Dnumber
WHERE D.Budget > 60000;
```

13. Recuperare il nome del dipartimento con Budget superiore alla media del budget di tutti i dipartimenti
``` MySQL
SELECT Dname 
FROM DEPARTMENT
WHERE Budget > (
	SELECT AVG(Budget)
	FROM DEPARTMENT
);
```

14. Recuperare il nome dei dipartimenti che hanno piu' di 2 impiegati
``` MySQL
SELECT D.Dname
FROM DEPARTENT AS D
JOIN EMPLOYEE AS E ON D.Dnumber = E.Dno
GROUP BY D.Dname
HAVING COUNT(E.Ssn) > 2;
```

15. Ridurre il Budget di ogni dipartimento del 10% per i dipartimenti che hanno un budget maggiore di 60000
``` MySQL
UPDATE DEPARTMENT 
SET Budget = Budget * 0.9
WHERE Budget > 60000;
```

16. Riassegnare tutti gli impiegati del dipartimento 5 al dipartimento 3
``` MySQL
UPDATE EMPLOYEE 
SET Dno = 3
WHERE Dno = 5;
```

17. Ricavare il nome completo di tutti gli impiegati e il nome del dipartimento in cui lavorano
``` MySQL
SELECT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name, D.Dname 
FROM EMPLOYEE AS E
JOIN DEPARTMENT D ON E.Dno = D.Dnumber;
```

18. Ricavare l'elenco dei nomi completi degli impiegati e il salario ordine crescente di salario
``` MySQL
SELECT CONCAT(Fname, ' ', Minit, ' ', Lname) AS Full_Name, Salary
FROM EMPLOYEE
ORDER BY Salary ASC;
```

19. Ricavare l'elenco dei nomi completi degli impiegati e il salario in ordine decrescente di salario
``` MySQL
SELECT CONCAT(Fname, ' ', Minit, ' ', Lname) AS Full_Name, Salary
FROM EMPLOYEE
ORDER BY Salary DESC;
```

20. Ricavare l'elenco dei nomi completi degli impiegati del dipartimento nr 5 che lavorano almeno 10 ore sul progetto "ProductX"
``` MySQL
SELECT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name
FROM EMPLOYEE AS E
JOIN WORKS_ON AS W ON E.Ssn = W.Essn
JOIN PROJECT AS P ON W.Pno = P.Pnumber
WHERE E.Dno = 5 AND P.Pname = 'ProductX' AND W.Hours >= 10;
```

21. Ricavare l'elenco dei nomi completi degli impiegati del dipartimento nr 5 che lavorano meno di 20 ore sul progetto "ProductX"
``` MySQL
SELECT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name
FROM EMPLOYEE AS E
JOIN WORKS_ON AS W ON E.Ssn = W.Essn
JOIN PROJECT AS P ON W.Pno = P.Pnumber
WHERE E.Dno = 5 AND P.Pname = 'ProductX' AND W.Hours < 20;
```

22. Ricavare l'elenco dei nomi completi degli impiegati che sono direttamente supervisionati da Franklin Wong (senza utilizzare l'SSN di Franklin Wong direttamente nella query)
``` MySQL
SELECT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name
FROM EMPLOYEE AS E
JOIN EMPLOYEE AS S ON E.Super_ssn = S.Ssn
WHERE S.Fname = 'Franklin' AND S.Lname = 'Wong';
```

23. Per ogni progetto, elencare il nome del progetto e le ore che vengono impiegate da tutti gli impiegati sul progetto stesso
``` MySQL
SELECT P.Pname, SUM(W.Hours) AS Total_Hours
FROM PROJECT AS P JOIN WORKS
	 ON P.Pnumber = W.Pno
GROUP BY P.Pname;
```

24. Elencare i nomi completi degli impiegati che non lavorano su alcun progetto
``` MySQL
SELECT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name
FROM EMPLOYEE AS E LEFT JOIN WORKS_ON AS W
     ON E.Ssn = W.Essn
WHERE W.Ssn IS NOT NULL;
```

25. Per ogni dipartimento, il nome e il salario medio degli impiegati che lavorano in quel dipartimento
``` MySQL
SELECT D.Dname, AVG(E.Salary) AS Avg_Salary 
FROM DEPARTMENT AS D JOIN EMPLOYEE AS E 
	 ON D.Dnumber = E.Dno 
GROUP BY D.Dname;
```

26. Ricavare il nome e gli indirizzi di tutti gli impiegati che lavorano ad un progetto di un dipartimento che ha come sede Houston
``` MySQL
SELECT DISTINCT E.Fname, E.Lname, E.Address
FROM EMPLOYEE AS E
JOIN WORKS_ON AS W ON E.Ssn = W.Essn
JOIN PROJECT AS P ON W.Pno = P.Pnumber
JOIN DEPARTMENT AS D ON P.Dnum = D.Dnumber
WHERE Dlocation = 'Houston';
```

27. Elencare i dipartimenti che non hanno impiegati
``` MySQL
SELECT D.Dname 
FROM DEPARTMENT AS D
LEFT JOIN EMPLOYEE AS E ON D.Dnumber = E.Dno
WHERE E.Ssn IS NULL;
```

28. Per ogni progetto, elencare il nome del progetto ed il numero totale di ore spese su quel progetto
``` MySQL
SELECT P.Pname, SUM(W.Hours) AS Total_Hours
FROM PROJECT AS P
JOIN WORKS_ON AS W ON P.Pnumber = W.Pno
GROUP BY P.Pname;
```

29. Recupera il nome di tutti gli impiegati che lavorano ai progetti controllati dal dipartimento 5
``` MySQL
SELECT DISTINCT CONCAT(E.Fname, ' ', E.Minit, ' ', E.Lname) AS Full_Name
FROM EMPLOYEE AS E
JOIN WORKS_ON AS W ON E.Ssn = W.Essn
JOIN PROJECT P ON W.Pno = P.Pnumber
WHERE P.Dnum = 5;
```

30. Recupera la media del salario percepito dagli impiegati di sesso femminile
``` MySQL
SELECT AVG(Salary) AS Avg_Female_Salary
FROM EMPLOYEE
WHERE Sex = 'F';
```