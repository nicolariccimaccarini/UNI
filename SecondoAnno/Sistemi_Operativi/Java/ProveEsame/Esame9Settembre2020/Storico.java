package Java.ProveEsame.Esame9Settembre2020;

public class Storico {
    
    private int CambiRepentini = 0;

    public synchronized void IncrementaCambiRepentini() {
        CambiRepentini++;
    }

    public synchronized int getCambiRepentini() {
        return CambiRepentini;
    }
}
