package Esame22Giugno2020.java;

public class Veicolo {
    
    protected int codice;
    protected String targa, modello, marca;
    protected double costoGiornaliero;

    public Veicolo(int codice, String targa, String modello, String marca, double costoGiornaliero) {
        this.codice = codice;
        this.targa = targa;
        this.modello = modello;
        this.marca = marca;
        this.costoGiornaliero = costoGiornaliero;
    }
}
