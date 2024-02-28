import java.util.LinkedList;
import java.util.List;

public class Cliente {
    
    private String nomeCognome;
    private List<Posteggio> posteggi;

    public Cliente(String nomeCognome) {
        this.nomeCognome = nomeCognome;
        this.posteggi = new LinkedList<Posteggio>();
    }

    public void addPosteggio(String targa, int giorni, double costo) {
        posteggi.add(new Posteggio(targa, giorni, costo));
    }

    public double getTot() {
        double tot = 0;
        for (Posteggio p : posteggi) {
            tot += p.getTot();
        }
        return tot;
    }

    public String toString()
    {
        return nomeCognome+" "+getTot();
    }
}
