public class Prodotto {
    
    private String descrizione;
    private int quantitaVenduta, prezzoUnita;

    public Prodotto(String descrizione, int quantitaVenduta, int prezzoUnita) {
        this.descrizione = descrizione;
        this.quantitaVenduta = quantitaVenduta;
        this.prezzoUnita = prezzoUnita;
    }

    public int getTot() {
        return quantitaVenduta*prezzoUnita;
    }

    public String toString() {
        return descrizione + "\t" + quantitaVenduta + "\t" + prezzoUnita + "\t" + getTot();
    }
}
