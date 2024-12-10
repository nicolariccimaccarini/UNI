package Esame22Giugno2020.java;

public class Auto extends Veicolo {

    private int cilindrata;
    private double bagagliaio;

    public Auto(int codice, String targa, String modello, String marca, double costoGiornaliero, int cilindrata, double bagagliaio) {
        super(codice, targa, modello, marca, costoGiornaliero);
        this.cilindrata = cilindrata;
        this.bagagliaio = bagagliaio;
    }

    public String toString() {
        return "auto" + "\t" + targa + "\t" + codice + "\t" + modello + "\t" + marca + "\t" + costoGiornaliero + "\t" + cilindrata +"\t" + bagagliaio + "\t-\t-\t-";
    }
}
