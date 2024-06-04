package Java.ProveEsame.Esame18Giugno2020;

import java.io.Serializable;

public class Rilevazione implements Serializable {
    
    int valore = 0;
    long timestamp = 0L;

    public Rilevazione(int valore, long timestamp) {
        this.valore = valore;
        this.timestamp = timestamp;
    }

    public int getValore() {
        return valore;
    }

    public void setValore(int valore) {
        this.valore = valore;
    }
}
