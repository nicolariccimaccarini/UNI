package Java.ProveEsame.Esame18Giugno2020;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class GeneraRilevazioni extends Thread {

    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private PipedOutputStream pos = null;

    public GeneraRilevazioni(PipedOutputStream pos) {
        this.pos = pos;
    }

    @Override
    public void run() {
        isRunning.set(true);

        ObjectOutputStream oos = null;
        try {
            oos = new ObjectOutputStream(pos);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        while (isRunning.get()) {
            Rilevazione r = new Rilevazione((int) (200 * Math.random()), System.currentTimeMillis());

            try {
                oos.writeObject(r);
                oos.flush();
            } catch (IOException e) {
                e.printStackTrace();
            }

            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    public void terminaRilevazione() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }
}