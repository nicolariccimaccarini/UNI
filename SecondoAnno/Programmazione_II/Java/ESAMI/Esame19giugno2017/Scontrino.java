import java.util.LinkedList;
import java.util.List;

public class Scontrino {
    
    private int codiceScontrino, codiceNegozio;
    private String data;
    private List<Prodotto> prodotti;

    public Scontrino(int codiceScontrino, int codiceNegozio, String data) {
        this.codiceScontrino = codiceScontrino;
        this.codiceNegozio = codiceNegozio;
        this.data = data;
        this.prodotti = new LinkedList<Prodotto>();
    }

    public void addProdotto(Prodotto p) {
        prodotti.add(p);
    }

    public List<Prodotto> getListaProdotti() {
        return prodotti;
    }

    public int getCodNegozio() {
        return codiceNegozio;
    }

    public String toString() {
        return codiceScontrino + "\t" + data; 
    }

    public int getTot() {
        int somma = 0;
        for (Prodotto p : prodotti) {
            somma += p.getTot();
        }

        return somma;
    }
}
