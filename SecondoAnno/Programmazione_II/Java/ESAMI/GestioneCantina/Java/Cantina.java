import java.io.*;
import java.util.*;

public class Cantina {

    static List<Impiegato> impiegati = new LinkedList<Impiegato>();
    static List<Visitatore> visitatori = new LinkedList<Visitatore>();
    static Map<Integer, Impiegato> codImp = new HashMap<Integer, Impiegato>();
    static Map<Integer, Integer> codNserv = new HashMap<Integer, Integer>();

    public static void main(String[] args) {
        
        // leggo il primo file (impiegati.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("impiegati.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String nomeDipendente = br.readLine();

                line = br.readLine();
                if (tipo.equals("guida")) {
                    tok = new StringTokenizer(line);
                    int telefono = Integer.parseInt(tok.nextToken());
                    boolean inglese = Boolean.parseBoolean(tok.nextToken());
                    int appuntamentiSettimanali = Integer.parseInt(tok.nextToken());
                    double costoOrario = Double.parseDouble(tok.nextToken());

                    Impiegato i = new Guida(codice, tipo, nomeDipendente, costoOrario, telefono, inglese, appuntamentiSettimanali);
                    impiegati.add(i);
                    codImp.put(codice, i);
                    codNserv.put(codice, 0);
                }
                else {
                    tok = new StringTokenizer(line);
                    int oreSettimanali = Integer.parseInt(tok.nextToken());
                    double costoOrario = Double.parseDouble(tok.nextToken());
                    String specialita = br.readLine();

                    Impiegato i = new Sommelier(codice, tipo, nomeDipendente, costoOrario, oreSettimanali, specialita);
                    impiegati.add(i);
                    codImp.put(codice, i);
                    codNserv.put(codice, 0);
                }
                line = br.readLine();
            }
            br.close();
            
        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }


        // lettura secondo file (visitatori.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("visitatori.txt"));
            String line = br.readLine();

            while (line != null) {
                int codiceVisitatore = Integer.parseInt(line);
                String nomeVisitatore = br.readLine();
                
                Visitatore v = new Visitatore(codiceVisitatore, nomeVisitatore);
                visitatori.add(v);

                line = br.readLine();
                while (line != null && !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codiceImpiegato = Integer.parseInt(tok.nextToken());
                    double oreServizio = Double.parseDouble(tok.nextToken());
                    
                    Impiegato i = codImp.get(codiceImpiegato);
                    Servizio s = new Servizio(i, oreServizio);
                    v.addServizio(s);

                    int current = codNserv.get(codiceImpiegato);
                    codNserv.put(codiceImpiegato, current+1);

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



        // stampo i dipendenti (punto 3)
        System.out.println("\nnome, codice, tipo, ore settimanali, specialita, telefono, inglese, app.sett., costo orario");
        for (Impiegato i : impiegati) {
            System.out.println(i);
        }
        System.out.println();

        // stampo i visitatori (punto 4)
        for (Visitatore v : visitatori) {
            System.out.println(v);
        }
        System.out.println();

        // leggo da tastiera il codice impiegato e stampo il numero dei servizi svolti
        int codice = Integer.parseInt(args[0]);
        System.out.println("Numero di servizi svolti: " + codNserv.get(codice));
    }
}