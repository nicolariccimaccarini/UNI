package Esame22Giugno2020.java;

public class Furgone extends Veicolo {

    private String categoria;
    private int numeroPosti;
    private boolean vanoCarico;
    
    public Furgone(int codice, String targa, String modello, String marca, double costoGiornaliero, String categoria, int numeroPosti, boolean vanoCarico) {
        super(codice, targa, modello, marca, costoGiornaliero);
        this.categoria = categoria;
        this.numeroPosti = numeroPosti;
        this.vanoCarico = vanoCarico;
    }

    public String toString() {
        return "furgone" + "\t" + targa + "\t" + codice + "\t" + modello + "\t" + marca + "\t" + costoGiornaliero + "\t-\t-\t" + categoria + "\t" + numeroPosti + "\t" + vanoCarico;
    }
}
