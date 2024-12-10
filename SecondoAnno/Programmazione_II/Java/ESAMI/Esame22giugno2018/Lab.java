package ESAMI.Esame22giugno2018;

public class Lab extends Corso {

    private String nomeLaboratorio, nomeAssistente;
    private int numeroPostazioni;

    public Lab(int codice, String nomeCorso, String docente, String nomeLaboratorio, String nomeAssistente, int numeroPostazioni) {
        super(codice, nomeCorso, docente);
        this.nomeLaboratorio = nomeLaboratorio;
        this.nomeAssistente = nomeAssistente;
        this.numeroPostazioni = numeroPostazioni;
    }

    public String toString() {
        return super.toString() + "lab.\t-\t-\t-\t" + "\t" + nomeLaboratorio + "\t" + nomeAssistente + "\t" + numeroPostazioni;
    }
    
}
