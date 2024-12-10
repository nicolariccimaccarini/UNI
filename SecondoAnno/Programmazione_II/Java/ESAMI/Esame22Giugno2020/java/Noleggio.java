package Esame22Giugno2020.java;

public class Noleggio {
    
    private int codiceVeicolo, numeroGiorni;
    private double costoGiornaliero;
    private String veicolo;

    public Noleggio(String veicolo, int numeroGiorni, double costoGiornaliero) {
        this.veicolo = veicolo;
        this.numeroGiorni = numeroGiorni;
        this.costoGiornaliero = costoGiornaliero;
    }

    public double getTot() {
        return costoGiornaliero;
    }

    public String toString() {
        return "(" + veicolo + "," + numeroGiorni + ")";
    }
}
