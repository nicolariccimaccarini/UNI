package ESAMI.Esame22giugno2018;

public class Esame {
    
    private int codiceCorso;
    private String voto;
    
    public Esame(int codiceCorso, String voto) {
        this.codiceCorso = codiceCorso;
        this.voto = voto;
    }

    public String toString() {
        return codiceCorso + "\t" + voto;
    }
}
