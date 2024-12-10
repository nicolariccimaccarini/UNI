public class Noleggio {
    
    private int codiceVeicolo, nGiorni;

    public Noleggio(int codiceVeicolo, int nGiorni) {
        this.codiceVeicolo = codiceVeicolo;
        this.nGiorni = nGiorni;
    }

    public String toString() {
        return "(" + codiceVeicolo + "," + nGiorni + ")";
    }

    public int getGiorni() {
        return nGiorni;
    }
}
