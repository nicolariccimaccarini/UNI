package Java.Es3;

import java.util.concurrent.atomic.AtomicBoolean;

public class WorkerThread implements Runnable{

    //AtomicBoolean --> a boolean value that may be updated automatically 
    // to prevents conflicts in setting and checking the variable from different threads

    private final AtomicBoolean running = new AtomicBoolean(false);

    private int i;
    public WorkerThread(int i) {
        this.i = i;
    }

    // stop the sub-thread
    public void stop() {
        running.set(false);
    }

    public void run() {
        running.set(true);

        while (running.get()) {
            try {
                System.out.println("Thread: " + i);
                Thread.sleep(1000);
            } 
            catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                e.printStackTrace();
            }
        }
    }
}