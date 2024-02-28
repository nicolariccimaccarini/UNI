public class SuperStore extends Negozio {

    private String ragioneSociale;
    private int partitaIva, nCasse;
    
    public SuperStore(String tipo, int codice, String indirizzo, int superficie, String ragioneSociale, int partitaIva, int nCasse) {
        super(tipo, codice, indirizzo, superficie);
        this.ragioneSociale = ragioneSociale;
        this.partitaIva = partitaIva;
        this.nCasse = nCasse;
    }

    public String getTipo() {
        return tipo;
    }

    public String getRagione() {
        return ragioneSociale;
    }
    
    public String toString() {
        return "Super-Store" + "\t" + codice + "\t" + indirizzo + "\t" + superficie + "\t" + "-" + "\t" + "-" + "\t" + ragioneSociale + "\t" + partitaIva + "\t" + nCasse; 
    }

}
