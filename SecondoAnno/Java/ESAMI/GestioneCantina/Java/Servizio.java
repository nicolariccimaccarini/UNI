public class Servizio {
    
    protected Impiegato i;
    protected double oreServizio;

    public Servizio(Impiegato i, double oreServizio) {
        this.i = i;
        this.oreServizio = oreServizio;
    }

    public double getDurata() {
        return oreServizio * i.getCostoOrario();
    }

    public String toString() {
        return "(" + i.nomeDipendente + " , " + getDurata() + ")";
    }
}
