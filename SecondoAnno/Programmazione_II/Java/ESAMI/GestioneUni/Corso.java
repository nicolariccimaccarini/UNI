public class Corso {
    
    protected int codice;
    protected String nomeCorso, docente;

    public Corso(int codice, String nomeCorso, String docente) {
        this.codice = codice;
        this.nomeCorso = nomeCorso;
        this.docente = docente;
    }

    public String getNomeCorso() {
        return nomeCorso;
    } 

    public int getCodice() {
        return codice;
    }

    public String toString() {
        return nomeCorso + "\t" + codice + "\t" + docente;
    }
}
