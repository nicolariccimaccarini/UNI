import java.io.*;
import java.util.*;

public class Gestione {

    static List<Negozio> negozi = new LinkedList<Negozio>();
    static List<Scontrino> scontrini = new LinkedList<Scontrino>();
    static Map<Integer, Scontrino> codScontrino = new HashMap<Integer, Scontrino>();

    public static void main(String[] args) {
        
        // leggo il primo file (negozi.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("negozi.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                String tipo = tok.nextToken();
                int codice = Integer.parseInt(tok.nextToken());
                String indirizzo = br.readLine();

                line = br.readLine();
                if (tipo.equals("city-store")) {
                    String responsabile = line;
                    String codiceFiscale = br.readLine();
                    int superficie = Integer.parseInt(br.readLine());

                    Negozio n = new CityStore(tipo, codice, indirizzo, superficie, responsabile, codiceFiscale);
                    negozi.add(n);
                }
                else {
                    String ragioneSociale = line;
                    line = br.readLine();
                    tok = new StringTokenizer(line);
                    int partitaIva = Integer.parseInt(tok.nextToken());
                    int nCasse = Integer.parseInt(tok.nextToken());
                    int superficie = Integer.parseInt(br.readLine());

                    Negozio n = new SuperStore(tipo, codice, indirizzo, superficie, ragioneSociale, partitaIva, nCasse);
                    negozi.add(n);
                }
                line = br.readLine();
            }
            br.close();
            
        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }


        // leggo il secondo file (scontrini.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("scontrini.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codiceScontrino = Integer.parseInt(tok.nextToken());
                int codiceNegozio = Integer.parseInt(tok.nextToken());
                String data = br.readLine();
                Scontrino s = new Scontrino(codiceScontrino, codiceNegozio, data);
                scontrini.add(s);
                codScontrino.put(codiceNegozio, s);
                
                line = br.readLine();
                while (line != null && !line.equals("")) {
                    String descrizione = line;
                    line = br.readLine();
                    tok = new StringTokenizer(line);
                    int quantitaVenduta = Integer.parseInt(tok.nextToken());
                    int prezzoUnita = Integer.parseInt(tok.nextToken());
                    
                    Prodotto p = new Prodotto(descrizione, quantitaVenduta, prezzoUnita);
                    s.addProdotto(p);
                    
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


        // stampo i negozi
        System.out.println("\nTipo, ID, Indirizzo, Superficie, Nome e Cognome Responsabile, Codice Fiscale, Ragione Sociale, Partita Iva, Casse");
        for (Negozio n : negozi) {
            System.out.println(n);
        }
        System.out.println();

        // stampo scontrini
        System.out.println("Codice Scontrino, Data, Descrizione Prodotto, Quantità, Prezzo Unitario, Totale");
        for (Negozio n : negozi) {
            System.out.println(n.getCodice());
            //Scontrino s = codScontrino.get(n.getCodice());
            for (Scontrino s : scontrini) {
                if (n.getCodice() == s.getCodNegozio()) {
                    System.out.println(s + "\t" + s.getListaProdotti());
                }
            }
        }
        System.out.println();
        
        // punto 5: 
        System.out.println("Codice Negozio, Fatturato per metro quadro");
        for (Negozio n : negozi) {
            double fatturato = 0.0;
            for (Scontrino s : scontrini) {
                if (s.getCodNegozio() == n.getCodice()) {
                    fatturato += s.getTot();
                }
            }
            System.out.println(n.getCodice() + "\t" + fatturato/n.getSuperficie());
        }
    }

}