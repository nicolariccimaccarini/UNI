package Java.Es9;

import java.io.Serializable;

public class ControlloProduzione implements Serializable {
    
    private int semilavorati = 0;
    private int finiti = 0;

    public synchronized void incrementaSemilavorati() {
        semilavorati++;
    }

    public synchronized void decrementaSemilavorati() {
        semilavorati--;
    }

    public synchronized int getSemilavorati() {
        return semilavorati;
    }

    public synchronized void incrementaFiniti() {
        finiti++;
    }

    public synchronized void decrementaFiniti() {
        finiti--;
    }

    public synchronized int getFiniti() {
        return finiti;
    }
}
