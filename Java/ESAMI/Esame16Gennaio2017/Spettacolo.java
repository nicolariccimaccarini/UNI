public class Spettacolo {

    protected String titolo, produttore, tipo;
    protected int codice, annoUscita;

    public Spettacolo(String tipo, String titolo, int codice, String produttore, int annoUscita) {
        this.titolo = titolo;
        this.codice = codice;
        this.produttore = produttore;
        this.annoUscita = annoUscita;
        this.tipo = tipo;
    }

    public String getTitolo() {
        return titolo;
    }

    public String getTipo() {
        return tipo;
    }

    public int getCodice() {
        return codice;
    }

}