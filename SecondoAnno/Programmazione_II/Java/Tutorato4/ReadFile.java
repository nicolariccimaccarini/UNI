package Tutorato4;

import java.io.*;

public class ReadFile {
    
    public ReadFile(String filename) throws IOException {

        // FileReader e' uno stream di dati che permette di collegarsi ad un file di testo in lettura
        FileReader fr = new FileReader(filename);

        // BufferedReader e' uno stream di manipolazione a caratteri che consente di leggere le stringhe
        BufferedReader inFile = new BufferedReader(fr);

        System.out.println("Il testo salvato e': ");
        String linea = inFile.readLine();

        while (linea != null) {
            System.out.println(linea);
            linea = inFile.readLine();
        }
        System.out.println();
        
        inFile.close();
    }
}
