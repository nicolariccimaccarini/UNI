import java.io.*;
import java.util.*;

public class Main {

    static List<Veicolo> veicoli = new LinkedList<Veicolo>();
    static List<Clienti> clienti = new LinkedList<Clienti>();
    static Map<Integer, Veicolo> codVeicolo = new HashMap<Integer,Veicolo>();

    public static void main(String[] args) {
        
        try {
            BufferedReader br = new BufferedReader(new FileReader("veicoli.txt"));
            String line = br.readLine();
            
            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);

                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String marca = br.readLine();

                line = br.readLine();
                if (tipo.equals("roulotte")) {
                    tok = new StringTokenizer(line);
                    int peso = Integer.parseInt(tok.nextToken());
                    float lunghezza = Float.parseFloat(tok.nextToken());
                    float larghezza = Float.parseFloat(tok.nextToken());
                    int postiLetto = Integer.parseInt(tok.nextToken());

                    line = br.readLine();
                    tok = new StringTokenizer(line);
                    String veranda = tok.nextToken();
                    float costoGiornaliero = Float.parseFloat(tok.nextToken());

                    Veicolo roulotte = new Roulotte(codice, marca, lunghezza, larghezza, postiLetto, costoGiornaliero, peso, veranda);
                    veicoli.add(roulotte);

                    codVeicolo.put(codice, roulotte);
                }
                else {
                    tok = new StringTokenizer(line);
                    float larghezza = Float.parseFloat(tok.nextToken());
                    float lunghezza = Float.parseFloat(tok.nextToken());
                    int potenza = Integer.parseInt(tok.nextToken());
                    float costoGiornaliero = Float.parseFloat(tok.nextToken());

                    Veicolo caravan = new Caravan(codice, marca, lunghezza, larghezza, codice, costoGiornaliero, potenza);
                    veicoli.add(caravan);

                    codVeicolo.put(codice, caravan);
                }
                line = br.readLine();
                //line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (NumberFormatException e) {
            System.err.println(e);
        }

        try {
            BufferedReader br = new BufferedReader(new FileReader("clienti.txt"));
            String line = br.readLine();

            while (line != null) {
                int codiceCliente = Integer.parseInt(line);
                String nome = br.readLine();
                String cognome = br.readLine();
                String indirizzo = br.readLine();

                Clienti cliente = new Clienti(codiceCliente, nome, cognome, indirizzo);
                clienti.add(cliente);

                line = br.readLine();
                while (line != null && line != "\n") {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codiceVeicolo = Integer.parseInt(tok.nextToken());
                    int nGiorni = Integer.parseInt(tok.nextToken());

                    Noleggio noleggio = new Noleggio(codiceVeicolo, nGiorni);
                    cliente.addNoleggio(noleggio);

                    Veicolo v = codVeicolo.get(codiceVeicolo);
                    v.addNoleggio(noleggio);

                    line = br.readLine();
                }
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (NumberFormatException e) {
            System.err.println(e);
        }


        for (Veicolo v : veicoli) {
            System.out.println(v);
        }

        System.out.println();

        for (Clienti c : clienti) {
            System.out.println(c);
        }

        for (Veicolo v : veicoli) {
            System.out.println(v.getCode()+ "\t" +v.incasso());
        }

    }
}