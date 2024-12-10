import java.util.LinkedList;
import java.util.List;

public class Clienti {
    
    private int codiceCliente;
    private String nome, cognome, indirizzo;
    private List<Noleggio> noleggi;

    public Clienti(int codiceCliente, String nome, String cognome, String indirizzo) {
        this.codiceCliente = codiceCliente;
        this.nome = nome;
        this.cognome = cognome;
        this.indirizzo = indirizzo;
        this.noleggi = new LinkedList<Noleggio>();
    }

    public void addNoleggio(Noleggio n) {
        noleggi.add(n);
    }

    public String toString() {
        return codiceCliente + "\t" + cognome + "\t" + nome + "\t" + indirizzo + "\t" + noleggi.toString();
    }
}
