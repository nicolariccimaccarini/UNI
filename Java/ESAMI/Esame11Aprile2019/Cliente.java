import java.util.LinkedList;
import java.util.List;

public class Cliente {

    private int codiceCliente;
    private String nomeCliente;
    private List<Servizio> servizi;

    public Cliente(int codiceCliente, String nomeCliente) {
        this.codiceCliente = codiceCliente;
        this.nomeCliente = nomeCliente;
        this.servizi = new LinkedList<Servizio>();
    }

    public void addServizioo(Dipendente dip, double ore) {
        servizi.add(new Servizio(dip, ore));
    }

    public double getTot() {
        double tot=0;
        for (Servizio s : servizi) {
            tot+=s.getValore();
        }
        return tot;
    }

    public String toString()
    {
        return codiceCliente + " " +nomeCliente+" " + getTot();
    }
}