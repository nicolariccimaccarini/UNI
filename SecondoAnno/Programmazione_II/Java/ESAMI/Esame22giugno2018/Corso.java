package ESAMI.Esame22giugno2018;

public abstract class Corso {

    private int codice;
    private String nomeCorso, docente;

    public Corso(int codice, String nomeCorso, String docente) {
        this.codice = codice;
        this.nomeCorso = nomeCorso;
        this.docente = docente;
    }

    public String toString(){
		return codice + "\t" + nomeCorso +"\t" + docente;
	}
}
