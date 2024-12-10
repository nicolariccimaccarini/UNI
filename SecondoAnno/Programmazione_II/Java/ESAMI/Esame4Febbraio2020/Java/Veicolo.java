import java.util.LinkedList;
import java.util.List;

public class Veicolo {
    
    protected int codice, postiLetto;
    protected String marca;
    protected float lunghezza, larghezza, costoGiornaliero;
    protected List<Noleggio> noleggi;

    public Veicolo(int codice, String marca, float lunghezza, float larghezza, int postiLetto, float costoGiornaliero) {
        this.codice = codice;
        this.marca = marca;
        this.lunghezza = lunghezza;
        this.larghezza = larghezza;
        this.postiLetto = postiLetto;
        this.costoGiornaliero = costoGiornaliero;
        this.noleggi = new LinkedList<Noleggio>();
    }

    public void addNoleggio(Noleggio n) {
        this.noleggi.add(n);
    }

    public int getCode() {
        return codice;
    }

    public float incasso() {
        int giorni = 0;
        for (Noleggio n : noleggi) {
            giorni += n.getGiorni();
        }

        return costoGiornaliero * giorni;
    }
}