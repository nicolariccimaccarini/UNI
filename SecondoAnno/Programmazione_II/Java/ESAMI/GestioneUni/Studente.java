import java.util.LinkedList;
import java.util.List;

public class Studente {
    
    private int matricola;
    private String nomeStudente;
    private List<Libretto> libretti;
    private double media;

    public Studente(int matricola, String nomeStudente) {
        this.matricola = matricola;
        this.nomeStudente = nomeStudente;
        this.libretti = new LinkedList<Libretto>();
        this.media = 0;
    }

    public void addLibretto(Libretto l) {
        libretti.add(l);
    }

    public int getMatricola() {
        return matricola;
    }

    public double getMedia() {
        int somma=0;
        for (Libretto l : libretti) {
            somma += l.getVoto();
        }
        return media = somma/libretti.size();
    }

    public int getPiuAlto() {
        int max = 0;
        for (Libretto l : libretti) {
            if (l.getVoto() > max) {
               max = l.getVoto();

            }
        }
        return max;
    }

    public int getCodCorsoMax() {
        int max = 0;
        int codCorso = 0;
        for (Libretto l : libretti) {
            if (l.getVoto() > max) {
                max = l.getVoto();
                codCorso = l.getCodiceCorso();
            }
        }
        return codCorso;
    }

    public String getNome() {
        return nomeStudente;
    }


    public String toString() {
        return nomeStudente + "\t" + getMedia() + "\t" + libretti;
    }
}
