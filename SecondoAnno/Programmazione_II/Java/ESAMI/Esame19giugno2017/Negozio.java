public class Negozio {
    
    protected int codice, superficie;
    protected String indirizzo;
    protected String tipo;

    public Negozio(String tipo, int codice, String indirizzo, int superficie) {
        this.tipo = tipo;
        this.codice = codice;
        this.indirizzo = indirizzo;
        this.superficie = superficie;
    }

    public int getCodice() {
        return codice;
    }

    public int getSuperficie() {
        return superficie;
    }

    public String getTipo()
    {
        return tipo;
    }
}
