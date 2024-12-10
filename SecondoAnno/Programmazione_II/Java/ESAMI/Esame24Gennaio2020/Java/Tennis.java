package Esame24Gennaio2020.Java;

public class Tennis extends Campo {
    
    private float temperaturaMedia;
    private String terrno;
    
    public Tennis(int codice, String campo, float larghezza, float lunghezza, float costoOrario, float temperaturaMedia, String terreno) {
        super(codice, campo, larghezza, lunghezza, costoOrario);
        this.temperaturaMedia = temperaturaMedia;
        this.terrno = terreno;
    }

    @Override
    public String toString() {
        return "tennis" + "\t" + campo + "\t" + codice + "\t" + larghezza + "\t" + lunghezza + "\t" + temperaturaMedia + "\t" + terrno + "\t-\t" + costoOrario;
    }
}