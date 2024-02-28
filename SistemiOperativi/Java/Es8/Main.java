package Java.Es8;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

public class Main {
    
    public static void main(String[] args) {
        
        PipedInputStream pis = new PipedInputStream();
        PipedOutputStream pos = null;

        try {
            pos = new PipedOutputStream(pis);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        MacchinaA macchinaA = new MacchinaA(pos);
        MacchinaB macchinaB = new MacchinaB(pis);
        Thread tA = new Thread(macchinaA);
        Thread tB = new Thread(macchinaB);

        tA.start();
        tB.start();

        try {
            Thread.sleep(10000);
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.exit(-2);
        }

        macchinaA.TerminaMacchinaA();
        macchinaB.TerminaMacchinaB();

        
    }
}
