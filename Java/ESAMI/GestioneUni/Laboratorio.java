public class Laboratorio extends Corso {

    private String nomeLab, assistente;
    private int nPostazioni;
    
    public Laboratorio(int codice, String nomeCorso, String docente, String nomeLab, String assistente, int nPostazioni) {
        super(codice, nomeCorso, docente);
        this.nomeLab = nomeLab;
        this.assistente = assistente;
        this.nPostazioni = nPostazioni;
    }

    public String toString() {
        return super.toString() + "\t" + "laboratorio" + "\t-\t-\t-\t" + nomeLab + "\t" + assistente + "\t" + nPostazioni;
    }
    
}
