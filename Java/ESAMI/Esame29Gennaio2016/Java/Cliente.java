package Java;
public class Cliente {

    protected int codice, giorno, mese, anno;
    protected String idnirizzo;

    public Cliente(int codice, String indirizzo, int giorno, int mese, int anno) {
        this.codice = codice;
        this.idnirizzo = indirizzo;
        this.giorno = giorno;
        this.mese = mese;
        this.anno = anno;
    }

    public String toString() {
        return codice + "\t" + idnirizzo + "\t" + giorno + "\t" + mese + "\t" + anno;
    }
}