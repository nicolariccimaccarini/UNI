package Java;
import java.io.*;
import java.util.*;

public class Main {
    
    public static void main(String[] args) {
        
        List<Cliente> clienti = new LinkedList<Cliente>();
        Map<String, Integer> sommaPremi = new HashMap<String, Integer>();
        int somma_premi = 0;

        try {
            BufferedReader br = new BufferedReader(new FileReader("clienti.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                String tipo = tok.nextToken();
                int codice = Integer.parseInt(tok.nextToken());
                String indirizzo = br.readLine();
                line = br.readLine();
                tok = new StringTokenizer(line);
                int giorno = Integer.parseInt(tok.nextToken());
                int mese = Integer.parseInt(tok.nextToken());
                int anno = Integer.parseInt(tok.nextToken());
                String nomeRagioneSociale = br.readLine();
                
                if (tipo.equals("privati")) {
                    Cliente c = new Privato(codice, indirizzo, giorno, mese, anno, nomeRagioneSociale);
                    clienti.add(c);
                }
                else {
                    int fatturato = Integer.parseInt(br.readLine());
                    Cliente c = new Azienda(codice, indirizzo, giorno, mese, anno, nomeRagioneSociale, fatturato);
                    clienti.add(c);
                }

                line = br.readLine();
                while (line != null) {
                    int premio = Integer.parseInt(line);
                    somma_premi += premio;
                    line = br.readLine();
                }
                sommaPremi.put(nomeRagioneSociale, new Integer(somma_premi));
                line = br.readLine();
            }
            br.close();
        } catch(IOException e) {
            e.printStackTrace();
        } catch(Exception e) {
            e.printStackTrace();
        }

        System.out.println("Tipo\tCodice\tNome\tRagioneSociale\tIndirizzo\tData\tFatturato");
        for (Cliente c : clienti) {
            System.out.println(c);
        }

        System.out.println("Somma premi: " + sommaPremi);
    }
}
