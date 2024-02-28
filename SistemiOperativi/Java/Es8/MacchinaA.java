package Java.Es8;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedOutputStream;
import java.util.concurrent.atomic.AtomicBoolean;

public class MacchinaA implements Runnable {
    
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private PipedOutputStream pos = null;

    public MacchinaA(PipedOutputStream pos) {
        this.pos = pos;
    }

    public void TerminaMacchinaA() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }

    @Override
    public void run() {
        isRunning.set(true);

        while (isRunning.get()) {
            BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(pos));
            String message = null;

            try {
                message = "Ok";
                bw.write(message);
                bw.newLine();
                bw.flush();
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