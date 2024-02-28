import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class Gestione {

    static List<Progetto> progetti = new LinkedList<Progetto>();
    static List<Ricercatore> ricercatori = new LinkedList<Ricercatore>();
    static Map<Integer, Progetto> codProgetto = new HashMap<Integer, Progetto>();

    public static void main(String[] args) {
        
        // leggo il primo file (progetti.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("progetti.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String titolo = br.readLine();
                String cognome = br.readLine();
                String organizzazione = br.readLine();

                line = br.readLine();
                if (tipo.equals("ricerca")) {
                    tok = new StringTokenizer(line);
                    String codiceArgomento = tok.nextToken();
                    int nPartner = Integer.parseInt(tok.nextToken());
                    double importo = Double.parseDouble(br.readLine());

                    Progetto p = new Ricerca(codice, titolo, cognome, organizzazione, importo, codiceArgomento, nPartner);
                    progetti.add(p);
                    codProgetto.put(codice, p);
                }
                else {
                    tok = new StringTokenizer(line);
                    int aziende = Integer.parseInt(tok.nextToken());
                    double importo = Double.parseDouble(tok.nextToken());

                    Progetto p = new Innovazione(codice, titolo, cognome, organizzazione, importo, aziende);
                    progetti.add(p);
                    codProgetto.put(codice, p);
                }
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) { 
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        // leggo il secondo file (ricercatori.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("ricercatori.txt"));
            String line = br.readLine();

            while (line != null) {
                String nome = line;
                String cognome = br.readLine();
                Ricercatore r = new Ricercatore(nome, cognome);
                ricercatori.add(r);
                
                line = br.readLine();
                while (line != null && !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codice = Integer.parseInt(tok.nextToken());
                    double ore = Double.parseDouble(tok.nextToken());
                    r.addImpegno(codProgetto.get(codice), ore);
                    line = br.readLine();
                }
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        // stampa dei progetti
        System.out.println("\ntitolo, codice, coordinatore, organizzazione, argomento, partner, aziende, importo totale in migiaia di euro");
        for (Progetto p : progetti) {
            System.out.println(p);
        }
        System.out.println();

        // stampa dei ricercatori
        for (Ricercatore r : ricercatori) {
            System.out.println(r);
        }
        System.out.println();

        // punto 5
        for (Ricercatore r : ricercatori) {
            if (r.getCognome().equals(args[0])) {
                System.out.println(r.getNome() + "\t" + r.getCognome() + "\t" + r.getPiuAlto() + "\t" + r.getProgPiuAlto());
            }
        }

    }
}