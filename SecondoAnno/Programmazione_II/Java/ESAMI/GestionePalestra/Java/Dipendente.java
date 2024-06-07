public class Dipendente {
    
    protected int codice;
    protected String nomeDipendente;
    protected double costoOrario;

    public Dipendente(int codice, String nomeDipendente, double costoOrario) {
        this.codice = codice;
        this.nomeDipendente = nomeDipendente;
        this.costoOrario = costoOrario;
    }

    public int getCodice() {
        return codice;
    }

    public double getCostoOrario() {
        return costoOrario;
    }

    public String getNomeDipendente() {
        return nomeDipendente;
    }

    public String toString() {
        return nomeDipendente + "\t" + codice;
    }
}
