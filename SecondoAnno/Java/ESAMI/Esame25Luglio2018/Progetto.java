public class Progetto {
    
    protected int codice;
    protected String titolo, coordinatore, organizzazione;
    protected Double importo;

    public Progetto(int codice, String titolo, String coordinatore, String organizzazione, double importo) {
        this.codice = codice;
        this.titolo = titolo;
        this.coordinatore = coordinatore;
        this.organizzazione = organizzazione;
        this.importo = importo;
    }

    public int getCodice() {
        return codice;
    }

    public String getTitolo() {
        return titolo;
    }
}
