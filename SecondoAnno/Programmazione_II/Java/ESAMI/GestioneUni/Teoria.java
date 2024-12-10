public class Teoria extends Corso {

    private int codiceAula, oreSettimanali;
    private double oreLezione;

    public Teoria(int codice, String nomeCorso, String docente, int codiceAula, int oreSettimanali, double oreLezione) {
        super(codice, nomeCorso, docente);
        this.codiceAula = codiceAula;
        this.oreSettimanali = oreSettimanali;
        this.oreLezione = oreLezione;
    }
    
    public String toString() {
        return super.toString() + "\t" +"teoria" + "\t" + codiceAula + "\t" + oreSettimanali + "\t" + oreLezione + "\t-\t-\t-";
    }
}
