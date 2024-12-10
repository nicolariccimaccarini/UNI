package Java.Es2;

import java.io.*;

public class Main {
    
    public static void main(String[] args) {
        
        PipedInputStream pis = new PipedInputStream();

        try {
            PipedOutputStream pos = new PipedOutputStream();
            
            FromInput fi = new FromInput(pos);
            ToOutput to = new ToOutput(pis);
            fi.start();
            to.start();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
