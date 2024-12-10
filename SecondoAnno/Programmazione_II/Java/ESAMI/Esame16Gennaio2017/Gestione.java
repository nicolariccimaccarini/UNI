import java.io.*;
import java.net.SocketException;
import java.util.*;

public class Gestione {
    
    static List<Spettacolo> spettacoli = new LinkedList<Spettacolo>();
    static List<Persona> persone = new LinkedList<Persona>();
    static List<Integer> codici = new LinkedList<Integer>();
    static Map<Integer, Spettacolo> codSpettacoli = new HashMap<Integer, Spettacolo>();

    public static void main(String[] args) {
        
        // leggo il primo file (spettacoli.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("spettacoli.txt"));
            String line = br.readLine();

            while (line != null) {
                String titolo = line;
                line = br.readLine();
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();

                line = br.readLine();
                if (tipo.equals("serie")) {
                    tok = new StringTokenizer(line);
                    int stagione = Integer.parseInt(tok.nextToken());
                    int nPuntate = Integer.parseInt(tok.nextToken());
                    String produttore = br.readLine();
                    int annoUscita = Integer.parseInt(br.readLine());

                    Spettacolo s = new Serie(tipo, titolo, codice, produttore, annoUscita, stagione, nPuntate);
                    spettacoli.add(s);
                    codSpettacoli.put(codice, s);
                }
                else {
                    String durata = line;
                    String produttore = br.readLine();
                    int annoUscita = Integer.parseInt(br.readLine());

                    Spettacolo s = new Film(tipo, titolo, codice, produttore, annoUscita, durata);
                    spettacoli.add(s);
                    codSpettacoli.put(codice, s);
                }
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        
        // leggo il secondo file (visualizzazioni.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("visualizzazioni.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codiceUtente = Integer.parseInt(tok.nextToken());
                String nome = tok.nextToken();
                String cognome = tok.nextToken();
                Persona p = new Persona(codiceUtente, nome, cognome);

                line = br.readLine();
                tok = new StringTokenizer(line);
                while (tok.hasMoreTokens()) {
                    int codSpet = Integer.parseInt(tok.nextToken());
                    p.addcodSpettacoli(codSpet);
                }
                persone.add(p);
                
                line = br.readLine();
            }
            br.close();
            
        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }


        // stampo gli spettacoli
        System.out.println();
        for (Spettacolo s : spettacoli) {
            System.out.println(s);
        }
        System.out.println();

        // stampo il punto 4 
        for (Persona p : persone) {
            for (Integer i : p.getVisti()) {
                Spettacolo s = codSpettacoli.get(i);
                System.out.println(p.getNome() + " " + p.getCognome() + "\t" + s.getTitolo() + " " + s.getTipo());
            }
            System.out.println();
        }

        // punto 5
        for (Persona p : persone) {
            System.out.println(p.getSpettacoli(p));
        }
        System.out.println();
    }
}
