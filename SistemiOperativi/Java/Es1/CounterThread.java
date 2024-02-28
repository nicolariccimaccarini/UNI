public class CounterThread implements Runnable{
    
    private Thread t;
    private Accumulatore acmltr;

    public CounterThread(Accumulatore x) {

        acmltr = x;
    }

    public void start() {
        t = new Thread(this);
        t.start();
    }
    
    public void run() {
        double value = Math.random();
        acmltr.addValue(value);
        System.out.println("Thread" + Thread.currentThread() + " accumulatore: " + acmltr.getValue());
    } 

    public void join() {

    }
}