package ESAMI.Esame22giugno2018;

public class Teoria extends Corso {

    private int codiceAula, numeroOreSettimanali;
    private Double numeroOreLezione;

    public Teoria(int codice, String nomeCorso, String docente, int codiceAula, int numeroOreSettimanali, Double numeroOreLezione) {
        super(codice, nomeCorso, docente);
        this.codiceAula = codiceAula;
        this.numeroOreSettimanali = numeroOreSettimanali;
        this.numeroOreLezione = numeroOreLezione;
    }

    public String toString() {
        return super.toString() + "\t" + codiceAula + "\t" + numeroOreSettimanali + "\t" + numeroOreLezione + "\t-\t-\t-";
    }
    
}
