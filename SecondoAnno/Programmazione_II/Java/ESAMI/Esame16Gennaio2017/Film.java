public class Film extends Spettacolo {

    private String durata;
    
    public Film(String tipo, String titolo, int codice, String produttore, int annoUscita, String durata) {
        super(tipo="film", titolo, codice, produttore, annoUscita);
        this.durata = durata;
    }

    public String toString() {
        return "film" + "\t" + titolo + "\t" + codice + "\t-\t-\t" + durata + "\t" + annoUscita + "\t" + produttore;
    }
    
}
