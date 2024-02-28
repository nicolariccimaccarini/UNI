public class Sommelier extends Impiegato {

    private int oreSettimanali;
    private String specialita;
   
    public Sommelier(int codice, String tipo, String nomeDipendente, double costoOrario, int oreSettimanali, String specialita) {
        super(codice, tipo, nomeDipendente, costoOrario);
        this.oreSettimanali = oreSettimanali;
        this.specialita = specialita;   
    }

    public String toString() {
        return super.toString() + "\t" + oreSettimanali + "\t" + specialita + "\t-\t-\t-\t" + costoOrario;
    }
}
