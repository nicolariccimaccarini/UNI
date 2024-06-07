public class Cliente {

    protected int codice;
    protected String indirizzo;
    protected int totaleAcquisti;

    public Cliente(int codice, String indirizzo) {
        this.codice = codice;
        this.indirizzo = indirizzo;
        this.totaleAcquisti = 0;
    }

    public int getCodice() {
        return codice;
    }

    public int getTotale() {
        return totaleAcquisti;
    }

    public void addVendita(int a) {
        totaleAcquisti += a;
    }

    public String toString() {
        return indirizzo;
    }
}