public class Ricerca extends Progetto {

    private String codiceArgomento;
    private int nPartner;
    
    public Ricerca(int codice, String titolo, String coordinatore, String organizzazione, double importo, String codiceArgomento, int nPartner) {
        super(codice, titolo, coordinatore, organizzazione, importo);
        this.codiceArgomento = codiceArgomento;
        this.nPartner = nPartner;
    }

    public String toString() {
        return "Ricerca\t" + titolo + "\t" + codice + "\t" + coordinatore + "\t" + organizzazione + "\t" + codiceArgomento + "\t" + nPartner + "\t" + "-"  + "\t" + importo*1000;
    }
    
}
