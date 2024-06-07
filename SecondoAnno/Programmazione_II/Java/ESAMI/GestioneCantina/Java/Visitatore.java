import java.util.LinkedList;
import java.util.List;

public class Visitatore {
    
    protected int codiceVisitatore;
    protected String nomeVisitatore;
    List<Servizio> servizi;

    public Visitatore(int codiceVisitatore, String nomeVisitatore) {
        this.codiceVisitatore = codiceVisitatore;
        this.nomeVisitatore = nomeVisitatore;
        this.servizi = new LinkedList<Servizio>();
    }

    public void addServizio(Servizio s) {
        servizi.add(s);
    }

    public List<Servizio> getListServizi() {
        return servizi;
    }

    public int getSizeServizi() {
        return servizi.size();
    }

    public String toString() {
        return codiceVisitatore + "\t" + nomeVisitatore + "\t" + servizi;
    }
}
