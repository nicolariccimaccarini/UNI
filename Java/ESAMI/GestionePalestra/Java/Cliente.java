import java.util.LinkedList;
import java.util.List;

public class Cliente {
    
    private int codiceCliente;
    private String nomeCliente;
    List<Servizio> servizi;

    public Cliente(int codiceCliente, String nomeCliente) {
        this.codiceCliente = codiceCliente;
        this.nomeCliente = nomeCliente;
        this.servizi = new LinkedList<Servizio>();
    }

    public void addServizio(Servizio s) {
        servizi.add(s);
    }

    public List<Servizio> getListServizi() {
        return servizi;
    }

    public double getTot() {
        double tot=0;
        for (Servizio s : servizi) {
            tot += s.getValore();
        }
        return tot;
    }

    public String toString() {
        return codiceCliente + "\t" + nomeCliente + "\t" + getTot(); 
    }
}
