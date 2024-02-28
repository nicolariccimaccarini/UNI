public class Impiegato {
    
    protected int codice;
    protected String tipo, nomeDipendente;
    protected double costoOrario;

    public Impiegato(int codice, String tipo, String nomeDipendente, double costoOrario) {
        this.codice = codice;
        this.tipo = tipo;
        this.nomeDipendente = nomeDipendente;
        this.costoOrario = costoOrario;
    }

    public double getCostoOrario() {
        return costoOrario;
    }

    public String toString() {
        return nomeDipendente + "\t" + codice + "\t" + tipo;
    }
}
