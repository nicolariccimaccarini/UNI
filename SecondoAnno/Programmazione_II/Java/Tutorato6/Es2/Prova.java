package Tutorato6.Es2;

import java.io.*;
import java.util.*;

public class Prova {
    
    public static void main(String[] args) {

        List<Call> ListaChiamate = new ArrayList<Call>();
        
        try {
            BufferedReader br = new BufferedReader(new FileReader("data.txt"));
            String line = br.readLine();
            while (line != null) {
                String[] items = line.split(";");
                String sender = items[0];
                String receiver = items[1];
                int start = Integer.parseInt(items[2]);
                int stop = Integer.parseInt(items[3]);
                Call call = new Call(sender, receiver, start, stop);

                if (call.getDuration() > 550000) {
                    ListaChiamate.add(call);
                }

                line = br.readLine();
            }
            br.close();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println("La dimensione della lista e': " + ListaChiamate.size());

    }
}
