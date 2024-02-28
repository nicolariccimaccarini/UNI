package Java.Es9;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class MacchinaB implements Runnable {
    
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private ControlloProduzione cp = null;
    private PipedOutputStream pos = null;

    public MacchinaB(ControlloProduzione cp, PipedOutputStream pos) {
        this.cp = cp;
        this.pos = pos;
    }
    
    public void TerminaMacchinaB() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }

    @Override
    public void run() {
        isRunning.set(true);
        
        while (isRunning.get()) {

            if (cp.getSemilavorati() > 0) {
                cp.decrementaSemilavorati();
                
                try {
                    int sleepTime = (int) (100 + Math.random() * 50);
                    Thread.sleep(sleepTime);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }

                cp.incrementaFiniti();
                String message = "terminazione 1 prodotto finito";
                try {
                    BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(pos));
                    bw.write(message);
                    bw.flush();
                } catch (IOException e) {
                    e.printStackTrace();
                }

            }
            else {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
