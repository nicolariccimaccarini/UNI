package Java.Es2;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.charset.Charset;

class FromInput implements Runnable {

    private Thread t = null;
    private PipedOutputStream pos = null;

    FromInput(PipedOutputStream pos) {
        this.pos = pos;
    }

    public void start() {
        t = new Thread(this);
        t.start();
    }

    @Override
    public void run() {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        // il thread deve continuare a leggere dallo standard input
        // per scrivere l'output sul PipedOutputStream
        String line = null;

        try {
            System.out.println(("FromInput - inserire uno o piu' messaggi:"));
            while ((line = br.readLine()) != null) {
                System.out.println("From stdin: " + line);
                byte[] bytes = line.getBytes(Charset.forName("UTF-8"));
                pos.write(bytes, 0, bytes.length);
                pos.flush(); // forza la scrittura dei byte all'interno della PipedStream
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}