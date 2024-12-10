package Esame22Giugno2020.java;

import java.util.*;

public class Cliente {
    
    private String nome;
    private List<Noleggio> noleggi;

    public Cliente(String nome) {
        this.nome = nome;
        this.noleggi = new LinkedList<Noleggio>();
    }

    public void addNoleggio(String targa, int numeroGiorni, double costoGiornaliero) {
        noleggi.add(n);
    }

    public double sommaPrezziNoleggi() {
        double tot = 0;
        for (Noleggio n : noleggi) {
            tot += n.getTot();
        }

        return tot;
    }

    public String toString() {
        return nome + " " + sommaPrezziNoleggi() + " " + noleggi.size() + " " + noleggi;
    }
}
