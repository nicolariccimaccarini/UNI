package Java.ProveEsame.Esame9Settembre2020;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class GeneraDati implements Runnable {

    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private PipedOutputStream pos = null;
    private Misure misure = null;

    public GeneraDati(PipedOutputStream pos) {
        this.pos = pos;
    }

    public void Finisci() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }

    @Override
    public void run() {
        isRunning.set(true);
         
        while (isRunning.get()) {
            float temperatura = (float) ((-5) + Math.random() * 30);;
            misure.setTemperatura(temperatura);
            
            int umidita = (int) (15 + Math.random() * 70);;
            misure.setUmidita(umidita);
            
            try {
                ObjectOutputStream oos = new ObjectOutputStream(pos);
                oos.writeObject(misure);
                oos.flush();
            } catch (IOException e) {
                e.printStackTrace();
                System.exit(-1);
            }

            try {
                Thread.sleep(250);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
