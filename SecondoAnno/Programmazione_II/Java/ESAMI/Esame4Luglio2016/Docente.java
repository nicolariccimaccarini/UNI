public class Docente extends Iscritto {

    private String corsoPrincipale;
    
    public Docente(String nomeCognome, int codice, int eta, String indirizzo, String corsoPrincipale) {
        super(nomeCognome, codice, eta, indirizzo);
        this.corsoPrincipale = corsoPrincipale;
    }

    public String toString() {
        return "docente" + "\t" + nomeCognome + "\t" + codice + "\t" + eta + "\t" + corsoPrincipale + "\t-\t" + indirizzo;
    }
    
}
