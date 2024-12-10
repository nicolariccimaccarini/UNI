public class Pallacanestro extends Squadra{

    private int totPunti;
    private double nFalli;
    
    
    public Pallacanestro(int codice, String nomeSquadra, int nvittorie, int nSconfitte, int totPunti, double nFalli) {
        super(codice, nomeSquadra, nvittorie, nSconfitte);
        this.totPunti = totPunti;
        this.nFalli = nFalli;
    }

    public String toString() {
        return nomeSquadra + "\t" + codice + "\t" + nVittorie + "\t" + nSconfitte + "\t" + totPunti + "\t" + nFalli + "\t-\t" + "pallacanestro";
    }    
}
