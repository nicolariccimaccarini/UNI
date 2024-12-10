public class Guida extends Impiegato {

    private int telefono, appuntamentiSettimanali;
    private boolean inglese;
    
    public Guida(int codice, String tipo, String nomeDipendente, double costoOrario, int telefono, boolean inglese, int appuntamentiSettimanali) {
        super(codice, tipo, nomeDipendente, costoOrario);
        this.telefono = telefono;
        this.inglese = inglese;
        this.appuntamentiSettimanali = appuntamentiSettimanali;
    }

    public String toString() {
        return super.toString() + "\t-\t-\t" + telefono + "\t" + inglese + "\t" + appuntamentiSettimanali + "t" + costoOrario; 
    }
}
