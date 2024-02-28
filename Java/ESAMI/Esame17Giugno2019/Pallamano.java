public class Pallamano extends Squadra {

    private double nMedioGol; 

    public Pallamano(int codice, String nomeSquadra, int nvittorie, int nSconfitte, double nMedioGol) {
        super(codice, nomeSquadra, nvittorie, nSconfitte);
        this.nMedioGol = nMedioGol;
    }

    public String toString() {
        return nomeSquadra + "\t" + codice + "\t" + nVittorie + "\t" + nSconfitte + "\t" + "-" + "\t" + "-" + "\t" + nMedioGol + "\t" + "pallamano";
    }
}