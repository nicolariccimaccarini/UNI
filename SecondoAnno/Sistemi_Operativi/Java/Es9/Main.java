package Java.Es9;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

public class Main {
    
    public static void main(String[] args) {
         
        ControlloProduzione cp = new ControlloProduzione();
        PipedInputStream pis = new PipedInputStream();
        PipedOutputStream pos = null;

        try {
            pos = new PipedOutputStream(pis);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(-1);
        }

        MacchinaA macchinaA = new MacchinaA(cp);
        MacchinaA macchinaA1 = new MacchinaA(cp);
        MacchinaB macchinaB = new MacchinaB(cp, pos);

        Thread tA = new Thread(macchinaA);
        Thread tA1 = new Thread(macchinaA1);
        Thread tB = new Thread(macchinaB);

        tA.start();
        tA1.start();
        tB.start();

        for (int i=0; i<15; i++) {
            BufferedReader br = new BufferedReader(new InputStreamReader(pis));
            String messaggio = "";

            try {
                messaggio = br.readLine();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        macchinaA.TerminaMacchinaA();
        macchinaA1.TerminaMacchinaA();
        macchinaB.TerminaMacchinaB();
    }
}
