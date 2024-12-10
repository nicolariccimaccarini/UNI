public class Serie extends Spettacolo {

    private int stagione, nPuntate;
    
    public Serie(String tipo, String titolo, int codice, String produttore, int annoUscita, int stagione, int nPuntate) {
        super(tipo="serie TV", titolo, codice, produttore, annoUscita);
        this.stagione = stagione;
        this.nPuntate = nPuntate;
    }

    public String toString() {
        return "serie TV" + "\t" + titolo + "\t" + codice + "\t" + stagione + "\t" + nPuntate + "\t-\t" + annoUscita + "\t" + produttore;
    }
    
}
