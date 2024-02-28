public class Innovazione extends Progetto {

    private int aziende;
    
    public Innovazione(int codice, String titolo, String coordinatore, String organizzazione,
            double importo, int aziende) {
        super(codice, titolo, coordinatore, organizzazione, importo);
        this.aziende = aziende;
    }

    public String toString() {
        return "Ricerca\t" + titolo + "\t" + codice + "\t" + coordinatore + "\t" + organizzazione + "\t" + "-" + "\t" + "-" + "\t" + aziende + "\t" + importo*1000;
    }
    
}
