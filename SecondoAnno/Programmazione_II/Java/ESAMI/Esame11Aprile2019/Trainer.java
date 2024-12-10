public class Trainer extends Dipendente {

    private int oreSettimanali;
    private String specialita;
    
    public Trainer(int codice, String nomeDipendente, int oreSettimanali, double costoOrario, String specialita) {
        super(codice, nomeDipendente, costoOrario);
        this.oreSettimanali = oreSettimanali;
        this.specialita = specialita;
    }

    public String toString() {
        return nomeDipendente + "\t" + codice + "\t" + "trainer" + "\t" + oreSettimanali + "\t" + specialita + "\t" + "-" + "\t" + "-" + "\t" + "-" + "\t" + costoOrario;
    }
    
}
