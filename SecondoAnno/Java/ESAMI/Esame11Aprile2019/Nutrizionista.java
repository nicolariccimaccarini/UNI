public class Nutrizionista extends Dipendente {

    private int telefono, appuntamentiSettimanali;
    private boolean medico;

    public Nutrizionista(int codice, String nomeDipendente, double costoOrario, int telefono, boolean medico, int appuntamentiSettimanali) {
        super(codice, nomeDipendente, costoOrario);
        this.telefono = telefono;
        this.medico = medico;
        this.appuntamentiSettimanali = appuntamentiSettimanali;
    }

    public String toString() {
        return nomeDipendente + "\t" + codice + "\t" + "nutri" + "\t" + "-" + "\t" + "-" + "\t" + "-" + "\t" + telefono + "\t" + medico + "\t" + appuntamentiSettimanali + "\t" + costoOrario;
    }
    
}
