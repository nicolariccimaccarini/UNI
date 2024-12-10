public class Trainer extends Dipendente{

    private int oreSettimanali;
    private double costoOrario;
    private String specialita;
    
    public Trainer(int codice, String nomeDipendente, int oreSettimanali, double costoOrario, String specialita) {
        super(codice, nomeDipendente, costoOrario);
        this.oreSettimanali = oreSettimanali;
        this.costoOrario = costoOrario;
        this.specialita = specialita;
    }

    public String toString() {
        return super.toString() + "\t" + "Trainer" + "\t" + oreSettimanali + "\t" + specialita + "\t-\t-\t-\t" + costoOrario;
    }
    
}
