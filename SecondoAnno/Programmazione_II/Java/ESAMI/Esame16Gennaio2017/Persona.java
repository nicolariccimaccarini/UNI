import java.util.ArrayList;
import java.util.List;

public class Persona {
    
    private int codiceUtente;
    private String nome, cognome;
    private List<Integer> codSpettacoli;

    public Persona(int codiceUtente, String nome, String cognome) {
        this.codiceUtente = codiceUtente;
        this.nome = nome;
        this.cognome = cognome;
        this.codSpettacoli = new ArrayList<Integer>();
    }

    public void addcodSpettacoli(int s) {
        codSpettacoli.add(s);
    }

    public List<Integer> getVisti() {
        return codSpettacoli;
    }

    public String getNome() {
        return nome;
    }

    public String getCognome() {
        return cognome;
    }

    public String getSpettacoli(Persona p) {
        return p.getNome() + " " + p.getCognome() + "\t" + codSpettacoli.size();
    }

    public String toString() {
        return nome + " " + cognome + " " + codSpettacoli + " "; 
    }
}
