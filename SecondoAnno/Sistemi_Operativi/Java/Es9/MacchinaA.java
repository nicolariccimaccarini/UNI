package Java.Es9;

import java.util.concurrent.atomic.AtomicBoolean;

public class MacchinaA implements Runnable {
    
    private final AtomicBoolean isRunning = new AtomicBoolean(false);
    private ControlloProduzione cp = null;

    public MacchinaA(ControlloProduzione cp) {
        this.cp = cp;
    }

    public void TerminaMacchinaA() {
        isRunning.set(false);
        Thread.currentThread().interrupt();
    }

    @Override
    public void run() {
        isRunning.set(true);

        while (isRunning.get()) {
            cp.incrementaSemilavorati();

            try {
                int sleepTime = (int) (400 + Math.random() * 100);
                Thread.sleep(sleepTime);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
