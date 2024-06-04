-- a
SELECT Nome
FROM STUDENTE
WHERE Corso_Laurea=’CS’

-- b
SELECT Nome_Insegnamento
FROM INSEGNAMENTO, MODULO
WHERE INSEGNAMENTO.Codice_Insegnamento=MODULO.Codice_Insegnamento AND Docente=’King’ AND (Anno=’2007’ OR Anno=’2008’)

-- b alt.
SELECT Nome_Insegnamento
FROM INSEGNAMENTO
WHERE Codice_Insegnamento IN ( SELECT Codice_Insegnamento FROM MODULO WHERE Docente=’King’ AND (Anno=’2007’ OR Anno=’2008’) )

-- c
SELECT Codice_Insegnamento, Semestre, Anno, COUNT(*)
FROM MODULO, VOTAZIONE
WHERE Docente=’King’ AND MODULO.Identificatore_Modulo=VOTAZIONE.Identificatore_Modulo GROUP BY Codice_Insegnamento, Semestre, Anno

-- d
SELECT Nome, Nome_Insegnamento, I.Codice_Insegnamento, Crediti, Semestre, Anno, Voto FROM STUDENTE ST, INSEGNAMENTO I, MODULO M, VOTAZIONE V
WHERE Anno_Corso=4 AND Corso_Laurea=’CS’ AND ST.Numero_Studente=V.Numero_Studente AND
V.Identificatore_Modulo=M.Identificatore_Modulo AND M.Codice_Insegnamento=I.Codice_Insegnamento