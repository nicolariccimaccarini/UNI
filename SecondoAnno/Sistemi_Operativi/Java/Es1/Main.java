import java.util.*;

public class Main {
    public static void main(String[] args) {

        Accumulatore acc = new Accumulatore(0.0);
        Scanner consoleInput = new Scanner(System.in); 

        System.out.println("Inserisci quanti thread vuoi creare:");
        int nThreads = consoleInput.nextInt(); 
        consoleInput.close();

        Thread cThreads[] = new Thread[nThreads]; 

        for(int i = 0; i < nThreads; i++) {

            cThreads[i] = new Thread(new CounterThread(acc)); 
            cThreads[i].start(); 
        }

        for(int i = 0; i < nThreads; i++) {

            try {
                cThreads[i].join();
            }
            catch(InterruptedException e) {
                e.printStackTrace();
            }
        }

        System.out.println("Thread MAIN accumulatore vale: " + acc.getValue());
    }
}