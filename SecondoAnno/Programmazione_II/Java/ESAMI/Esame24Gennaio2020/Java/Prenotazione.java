package Esame24Gennaio2020.Java;

public class Prenotazione {
    
    protected int codiceCampo, ora;

    public Prenotazione(int codiceCampo, int ora) {
        this.codiceCampo = codiceCampo;
        this.ora = ora;
    }

    @Override
    public String toString() {
        return "(" + codiceCampo + "," + ora + ")";
    }
}
