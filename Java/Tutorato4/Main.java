package Tutorato4;

import java.io.*;

public class Main {
 
    public static void main(String[] args) {
        
        // Scrittura su file
        try {
            WriteLines wr = new WriteLines(args[0]);
        } catch (IOException e) {
            System.out.println("Errore di I/O");
            System.exit(1);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Errore nel passaggio di argomenti: specificare il nome del file");
            System.exit(1);
        }

        // Lettura da file
        try {
            ReadFile rf = new ReadFile(args[0]);
        } catch (FileNotFoundException e) {
            System.out.println("File " + args[0] + " non trovato");
            System.exit(1);
        } catch (IOException e) {
            System.out.println(e);
            System.exit(1);
        }
    }
}
