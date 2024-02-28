public class Impegno {
    
    private double ore;
    private Progetto p;

    public Impegno(Progetto p, double ore) {
        this.p = p;
        this.ore = ore;
    }

    public double getOre() {
        return ore;
    }

    public String getTitolo() {
        return p.getTitolo();
    }

    public String toString() {
        return "(" + p.getCodice() + "," + ore + ")"; 
    }
}
