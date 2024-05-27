CREATE SCHEMA Universita;

USE Universita;

CREATE TABLE STUDENTE (
    Nome VARCHAR(30) NOT NULL,
    Numero_Studente INTEGER NOT NULL,
    Anno_Corso CHAR NOT NULL,
    Corso_Laurea CHAR(4),

    PRIMARY KEY (Numero_Studente);
)

CREATE TABLE INSEGNAMENTO (
    Nome_Insegnamento VARCHAR(30) NOT NULL,
    Codice_Insegnamento CHAR(8) NOT NULL,
    Crediti INTEGER,
    Dipartimento CHAR(4).

    PRIMARY KEY (Codice_Insegnamento),
        UNIQUE (Nome_Insegnamento);
)

CREATE TABLE PROPEDEUTICITA (
    Codice_Insegnamento CHAR(8) NOT NULL,
    Codice_Propedeuticita CHAR(8) NOT NULL.

    PRIMARY KEY (Codice_Insegnamento, Codice_Propedeuticita),
        FOREIGN KEY (Codice_Insegnamento) REFERENCES INSEGNAMENTO (Codice_Insegnamento),
        FOREIGN KEY (Codice_Propedeuticita) REFERENCES INSEGNAMENTO (Codice_Insegnamento);
)

CREATE TABLE MODULO (
    Identificatore_Modulo INTEGER NOT NULL, 
    Codice_Insegnamento CHAR(8) NOT NULL,
    Semestre VARCHAR(7) NOT NULL,
    Anno CHAR(4) NOT NULL,
    Docente VARCHAR(15),

    PRIMARY KEY (Identificatore_Modulo),
        FOREIGN KEY (Codice_Insegnamento) REFERENCES INEGNAMENTO (Codice_Insegnamento);
)

CREATE TABLE VOAZIONE (
    Numero_Studente INTEGER NOT NULL,
    Identificatore_Modulo INTEGER NOT NULL,
    Voto CHAR,

    PRIMARY KEY (Numero_Studente, Identificatore_Modulo),
        Foreign Key (Numero_Studente) REFERENCES STUDENTE (Numero_Studente),
        Foreign Key (Identificatore_Modulo) REFERENCES MODULO (Identificatore_Modulo);
)