public class CityStore extends Negozio {

    private String responsabile, codiceFiscale;
    
    public CityStore(String tipo, int codice, String indirizzo, int superficie, String responsabile, String codiceFiscale) {
        super(tipo, codice, indirizzo, superficie);
        this.responsabile = responsabile;
        this.codiceFiscale = codiceFiscale;
    }

    public String getTipo() {
        return tipo;
    }

    public String getNome() {
        return responsabile;
    }

    public String toString() {
        return "City-Store" + "\t" + codice + "\t" + indirizzo + "\t" + superficie + "\t" + responsabile + "\t" + codiceFiscale + "\t-\t-\t-";
    }
    
}
