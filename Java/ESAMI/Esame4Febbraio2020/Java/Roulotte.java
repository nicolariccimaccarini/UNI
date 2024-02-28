public class Roulotte extends Veicolo{

    private int peso;
    private String veranda;

    public Roulotte(int codice, String marca, float lunghezza, float larghezza, int postiLetto, float costoGiornaliero, int peso, String veranda) {
        super(codice, marca, lunghezza, larghezza, postiLetto, costoGiornaliero);
        this.peso = peso;
        this.veranda = veranda;
    }

    public String toString() {
        return "Roulotte" + "\t" + codice + "\t" + marca + "\t" + larghezza + "\t" + lunghezza + "\t" + postiLetto + "\t" + peso + "\t" + veranda + "\t-\t" + costoGiornaliero;
    }
}
