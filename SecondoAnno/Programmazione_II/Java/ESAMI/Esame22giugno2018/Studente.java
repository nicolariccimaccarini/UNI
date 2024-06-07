package ESAMI.Esame22giugno2018;

import java.util.LinkedList;
import java.util.List;

public class Studente {
    
    protected int matricola;
    protected String nomeECognomeStudente;
    protected List<Esame> libretto;

    public Studente(int matricola, String nomeECognomeStudente) {
        this.matricola = matricola;
        this.nomeECognomeStudente = nomeECognomeStudente;
        this.libretto = new LinkedList<Esame>();
    }

    public void addEsame(Esame e) {
        libretto.add(e);
    }

    public String toString() {
        return matricola + "\t" + nomeECognomeStudente + "\t";
    }
}
