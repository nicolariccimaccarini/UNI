package Esame24Gennaio2020.Java;

import java.util.List;

public class Soci {
    
    private int codice, eta, categoria;
    private String nome;
    protected List<Prenotazione> prenotazioni;

    public Soci(int codice, String nome, int eta, int categoria) {
        this.codice = codice;
        this.nome = nome;
        this.eta = eta;
        this.categoria = categoria;
    }

    public void addPrenotazione(Prenotazione p) {
        prenotazioni.add(p);
    }

    @Override
    public String toString() {
        return codice + "\t" + nome + "\t" + eta + "\t" + categoria + "\t" + prenotazioni;
    }
}
