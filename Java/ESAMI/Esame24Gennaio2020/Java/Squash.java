package Esame24Gennaio2020.Java;

public class Squash extends Campo {

    private float altezza;
    private int piano;

    public Squash(int codice, String campo, float larghezza, float lunghezza, float costoOrario, float altezza, int piano) {
        super(codice, campo, larghezza, lunghezza, costoOrario);
        this.altezza = altezza;
        this.piano = piano;
    }

    @Override
    public String toString() {
        return "squash" + "\t" + campo + "\t" + codice + "\t" + larghezza + "\t" + lunghezza + "\t-\t-\t" + altezza + "\t" + piano + "\t" + costoOrario;
    }
}
