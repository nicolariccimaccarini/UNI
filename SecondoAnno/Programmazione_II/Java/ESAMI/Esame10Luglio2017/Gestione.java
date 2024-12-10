import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class Gestione {
    
    static List<Cliente> clienti = new LinkedList<Cliente>();
    static List<Vendita> vendite = new LinkedList<Vendita>();
    static Map<Integer,String> ind = new HashMap<Integer,String>();
    static Map<Integer,Cliente> cod_cli = new HashMap<Integer,Cliente>();

    public static void main(String[] args) {
        
        // leggo il primo file (clienti.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("clienti.txt"));
            String line = br.readLine();
            String indirizzo;

            while (line != null) {
                Cliente c;
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();

                line = br.readLine();
                if (tipo.equals("privato")) {
                    String nomeCognome = line;
                    String numeroCartaDiCredito = br.readLine();
                    indirizzo = br.readLine();

                    c = new Privato(codice, indirizzo, nomeCognome, numeroCartaDiCredito);
                    clienti.add(c);
                }
                else {
                    String ragioneSociale = line;
                    line = br.readLine();
                    tok = new StringTokenizer(line);
                    int partitaIva = Integer.parseInt(tok.nextToken());
                    int numeroDipendenti = Integer.parseInt(tok.nextToken());
                    String IBAN = br.readLine();
                    indirizzo = br.readLine();

                    c = new Azienda(codice, indirizzo, ragioneSociale, partitaIva, numeroDipendenti, IBAN);
                    clienti.add(c);
                }
                ind.put(codice, indirizzo);
                cod_cli.put(codice, c);
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        // leggo il secondo file (vendite.txt)
        int sommaNProdotti=0, sommaPrezzi=0, nVendite=0;
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("vendite.txt"));
            String line = br.readLine();

            while (line != null) {
                nVendite++;
                StringTokenizer tok = new StringTokenizer(line);
                int codiceVendita = Integer.parseInt(tok.nextToken());
                int codiceCliente = Integer.parseInt(tok.nextToken());
                String data = br.readLine();
                String descrizioneProdotto = br.readLine();
                int quantitaVenduta = Integer.parseInt(br.readLine());
                int prezzoUnita = Integer.parseInt(br.readLine());
                sommaNProdotti += quantitaVenduta;
                sommaPrezzi += prezzoUnita;

                Vendita v = new Vendita(codiceVendita, codiceCliente, data, descrizioneProdotto, quantitaVenduta, prezzoUnita);
                vendite.add(v);
                cod_cli.get(codiceCliente).addVendita(quantitaVenduta*prezzoUnita);

                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        System.out.println();
        // stampo i clienti
        System.out.println("Tipo, ID, Nome e Cognome, Numero Carta di Credito, Ragione Sociale, Partita Iva, Dipendenti, IBAN, Indirizzo");
        for (Cliente c : clienti) {
            System.out.println(c);
        }
        System.out.println();

        // stampo le vendite
        System.out.println("Codice Vendita, Codice Cliente, Indirizzo Cliente, Data, Descrizione Prodotto, Quantita', Prezzo Unitario, Totale, Totale Iva Esclusa");
        for (Vendita v : vendite) {
            System.out.println(v.getCodice() + "\t" + v.getCodiceCliente() + "\t" + ind.get(v.getCodiceCliente()) + "\t" + v);
        }
        System.out.println();

        // stampa del totale spese
        System.out.println("Stampa spese:");
        for (Cliente c : clienti) {
            System.out.println(c.getCodice() + "\t" + c.getTotale());
        }
        System.out.println();

        // punto 6
        double mediaProdotti = (double) sommaNProdotti/nVendite;
        double mediaPrezzi = (double) sommaPrezzi/nVendite;
        System.out.println("Media n. prodotti: " + mediaProdotti);
        System.out.println("Media prezzi " + mediaPrezzi);

        System.out.println();
    }
}
