package Java.Es8;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class MacchinaB implements Runnable {
    
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private PipedInputStream pis = null;

    public MacchinaB(PipedInputStream pis) {
        this.pis = pis;
    }

    public void TerminaMacchinaB() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }

    @Override
    public void run() {
        isRunning.set(true);

        while (isRunning.get()) {
            BufferedReader br = new BufferedReader(new InputStreamReader(pis));
            String message = null;

            try {
                message= br.readLine();

                if (message == null) {
                    continue;
                }

                System.out.println("Messaggio ricevuto: " + message);
            } catch (IOException e) {
                e.printStackTrace();
            }

            try {
                Thread.sleep(200);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
