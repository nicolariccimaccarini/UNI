import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

public class Gestione {
    
    static List<Iscritto> iscritti = new LinkedList<Iscritto>();

    public static void main(String[] args) {
        
        // leggo il primo file (iscritti.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("iscritti.txt"));
            String line = br.readLine();

            while (line != null) {
                String nomeCognome = line;
                line = br.readLine();
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                int eta = Integer.parseInt(tok.nextToken());

                line = br.readLine();
                if (tipo.equals("studente")) {
                    float votoMedio = Float.parseFloat(line);
                    String indirizzo = br.readLine();

                    Iscritto i = new Studente(nomeCognome, codice, eta, indirizzo, votoMedio);
                    iscritti.add(i);
                }
                else {
                    String corsoPrincipale = line;
                    String indirizzo = br.readLine();

                    Iscritto i = new Docente(nomeCognome, codice, eta, indirizzo, corsoPrincipale);
                    iscritti.add(i);
                }
                line = br.readLine();
            }
            br.close();


        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }




        // stampo gli iscritti
        System.out.println("\ntipo, nome e cognome, codice, eta', corso principale, media, indirizzo");
        for (Iscritto i : iscritti) {
            System.out.println(i);
        }
        System.out.println();

        // punto 4
        
    }
}
