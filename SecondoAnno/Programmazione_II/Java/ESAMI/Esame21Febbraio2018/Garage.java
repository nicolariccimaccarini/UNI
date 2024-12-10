import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class Garage {
    
    static List<Veicolo> veicoli   = new LinkedList<Veicolo>();
    static List<Cliente> clienti   = new LinkedList<Cliente>();
    static Map<Integer,String> codTarga = new HashMap<Integer,String>();
    static Map<Integer,Double> codCosto = new HashMap<Integer,Double>();
    static Map<Integer,Double> codTot = new HashMap<Integer,Double>();
    
    public static void main(String[] args) {
        
        // Leggo il primo file
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("veicoli.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String targa = tok.nextToken();
                
                line = br.readLine();
                if (tipo.equals("auto")) {
                    tok = new StringTokenizer(line);
                    int cilindrata = Integer.parseInt(tok.nextToken());
                    boolean diesel = Boolean.parseBoolean(tok.nextToken());
                    String modello = br.readLine();
                    String marca = br.readLine();

                    Veicolo v = new Auto(codice, targa, modello, marca, cilindrata, diesel);
                    veicoli.add(v);
                    codTarga.put(codice, targa);
                    codTot.put(codice, 0.0);
                    codCosto.put(codice, 10.0);
                } 
                else {
                    String categoria = line;
                    int numeroPosti = Integer.parseInt(br.readLine());
                    String modello = br.readLine();
                    String marca = br.readLine();

                    Veicolo v = new Furgone(codice, targa, modello, marca, categoria, numeroPosti);
                    veicoli.add(v);
                    codTarga.put(codice,targa);
                    codTot.put(codice,0.0);
                    codCosto.put(codice,15.0);
                }
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        // Leggo i posteggi
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("posteggi.txt"));
            String line = br.readLine();
    
            while (line != null) {
                String nomeCognome = line;
                Cliente c = new Cliente(nomeCognome);
                clienti.add(c);
                line = br.readLine();
                StringTokenizer tok = new StringTokenizer(line);
                while (tok.hasMoreTokens()) {
                    int codice = Integer.parseInt(tok.nextToken());
                    int giorni = Integer.parseInt(tok.nextToken());
                    String targa = codTarga.get(codice);
                    double costo = codCosto.get(codice);
                    c.addPosteggio(targa, giorni, costo);
                    double current = codTot.get(codice);
                    codTot.put(codice, current+giorni*costo);
                }
            }

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }



        // stampo i veicoli
        System.out.println("\ntipo, targa, codice, modello, cilindrata, diesel, categoria, numero di posti");
        for (Veicolo v : veicoli) {
            System.out.println(v);
        }
        System.out.println();

        // stampa dei clienti
        for (Cliente c : clienti) {
            System.out.println(c);
        }
        System.out.println();

        // stampa del totale dei noleggi
        int targa = Integer.parseInt(args[0]);
        double tot = codTot.get(targa);
        System.out.println(targa + " " + tot);
    }
}
