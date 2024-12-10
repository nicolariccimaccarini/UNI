public class Caravan extends Veicolo{

    private int potenza;

    public Caravan(int codice, String marca, float lunghezza, float larghezza, int postiLetto, float costoGiornaliero, int potenza) {
        super(codice, marca, lunghezza, larghezza, postiLetto, costoGiornaliero);
        this.potenza = potenza;
    }

    public String toString() {
        return "Caravan" + "\t" + codice + "\t" + marca + "\t" + larghezza + "\t" + lunghezza + "\t" + postiLetto + "\t" + "-" + "\t" + "-" + "\t" + potenza + "\t" + costoGiornaliero; 
    }
}
