import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

public class Gestione {
    
    static List<Squadra> squadre = new LinkedList<Squadra>();
    static List<Giocatore> giocatori = new LinkedList<Giocatore>();

    public static void main(String[] args) {
        
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("squadre.txt")); 
            String line = br.readLine();
            
            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);

                int codice = Integer.parseInt(tok.nextToken());
                String sport = tok.nextToken();
                String nomeSquadra = br.readLine();
                line = br.readLine();
                tok = new StringTokenizer(line);
                int nVittorie = Integer.parseInt(tok.nextToken());
                int nSconfitte = Integer.parseInt(tok.nextToken());

                if (sport.equals("pallacanestro")) {
                    double nFalli = Double.parseDouble(tok.nextToken());
                    int totPunti = Integer.parseInt(tok.nextToken());

                    Squadra s = new Pallacanestro(codice, nomeSquadra, nVittorie, nSconfitte, totPunti, nFalli);
                    squadre.add(s);
                }
                else {
                    double nMedioGol = Double.parseDouble(tok.nextToken());

                    Squadra s = new Pallamano(codice, nomeSquadra, nVittorie, nSconfitte, nMedioGol);
                    squadre.add(s);
                }

                line = br.readLine();
                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        try {
            
            BufferedReader br = new BufferedReader(new FileReader("giocatori.txt"));
            String line = br.readLine();

            while (line != null) {
                int codiceSquadra = Integer.parseInt(line);
                String cognome = br.readLine();
                String nome = br.readLine();
                line = br.readLine();
                StringTokenizer tok = new StringTokenizer(line);
                boolean titolare = Boolean.parseBoolean(tok.nextToken());
                int eta = Integer.parseInt(tok.nextToken());
                int numeroMaglia = Integer.parseInt(tok.nextToken());
                String ruolo = tok.nextToken();

                Giocatore g = new Giocatore(codiceSquadra, cognome, nome, titolare, eta, numeroMaglia, ruolo);
                giocatori.add(g);

                line = br.readLine();
            }
            br.close();

        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }

        System.out.println();
        System.out.println("nome della squadra, codice, n. partite vinte, n. partite perse, punti totali, n.medio di falli, n.medio di reti, sport");
        for(Squadra s : squadre) {
            System.err.println(s);
        }

        System.out.println();

        System.out.println("nome, cognome, età, numero di maglia, ruolo, nome squadra");
        for (Giocatore g : giocatori) {
            for (Squadra s : squadre) {
                if (g.getCode() == s.getCode()) {
                    g.setNome(s.getNome());
                }
            }

            System.out.println(g);
        }

        System.out.println();

        System.out.println("Numero di giocatori per squadra: ");
        for (Squadra s : squadre) {
            int counter = 0;
            for (Giocatore g : giocatori) {
                if (g.getCode() == s.getCode()) {
                    counter++;
                }
            }
            System.out.println(s.getNome() + "\t" + counter);
        }
    }
}
