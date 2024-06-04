package Java.ProveEsame.Esame9Settembre2020;

import java.util.concurrent.atomic.AtomicBoolean;

public class Avvisi extends Thread {
    
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private Storico storico = null;

    public Avvisi(Storico storico) {
        this.storico = storico;
    }

    @Override
    public void run() {
        isRunning.set(true);

        while (isRunning.get()) {
            
            try {
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                e.printStackTrace();
                System.exit(-1);
            }

            if (storico.getCambiRepentini() > 0) {
                System.out.println("Attenzione! " + storico.getCambiRepentini());
            }
        }
    }

    public void Finisci() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }
}