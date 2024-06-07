public class Libretto {

    private int codiceCorso;
    private String voto;

    public Libretto(int codiceCorso, String voto) {
        this.codiceCorso = codiceCorso;
        this.voto = voto;
    }

    public int getVoto() {
        if (voto.equals("30L")) {
            return 31;
        }
        else {
            return Integer.parseInt(voto);
        }
    }

    public int getCodiceCorso() {
        return codiceCorso;
    }

    public String toString() {
        return "(" + codiceCorso + "," + voto + ")";
    }

}