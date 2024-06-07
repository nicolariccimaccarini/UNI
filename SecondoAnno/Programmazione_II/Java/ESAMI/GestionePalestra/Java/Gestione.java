import java.io.*;
import java.util.*;

public class Gestione {

    static List<Dipendente> dipendenti = new LinkedList<Dipendente>();
    static List<Cliente> clienti = new LinkedList<Cliente>();
    static Map<Integer, Dipendente> codDip = new HashMap<Integer, Dipendente>();
    static Map<Integer, Integer> codNServ = new HashMap<Integer, Integer>();

    public static void main(String[] args) {
        
        // leggo il primo file (dipendenti.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("dipendenti.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String nomeDipendente = br.readLine();

                line = br.readLine();
                tok = new StringTokenizer(line);
                if (tipo.equals("trainer")) {
                    int oreSettimanali = Integer.parseInt(tok.nextToken());
                    double costoOrario = Double.parseDouble(tok.nextToken());
                    String specialita = br.readLine();

                    Dipendente d = new Trainer(codice, nomeDipendente, oreSettimanali, costoOrario, specialita);
                    dipendenti.add(d);
                    codDip.put(codice, d);
                    codNServ.put(codice, 0);
                }
                else {
                    int telefono = Integer.parseInt(tok.nextToken());
                    boolean medico = Boolean.parseBoolean(tok.nextToken());
                    int appuntamentiSettimanali = Integer.parseInt(tok.nextToken());
                    double costoOrario = Double.parseDouble(tok.nextToken());

                    Dipendente d = new Nutrizionista(codice, nomeDipendente, costoOrario, telefono, medico, appuntamentiSettimanali);
                    dipendenti.add(d);
                    codDip.put(codice, d);
                    codNServ.put(codice, 0);
                }
                line = br.readLine();
            }
            br.close();
            
        } catch (IOException e) {
            System.err.println(e);
        } catch (Exception e) {
            System.err.println(e);
        }


        // lettura secondo file (clienti.txt)
        try {

            BufferedReader br = new BufferedReader(new FileReader("clienti.txt"));
            String line = br.readLine();

            while (line != null) {
                int codiceCliente = Integer.parseInt(line);
                String nomeCliente = br.readLine();
                Cliente c = new Cliente(codiceCliente, nomeCliente);
                clienti.add(c);

                line = br.readLine();
                while (line != null && !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codiceDipendente = Integer.parseInt(tok.nextToken());
                    double ore = Double.parseDouble(tok.nextToken());

                    Dipendente d = codDip.get(codiceCliente);

                    Servizio s = new Servizio(d, ore);
                    c.addServizio(s);
                    
                    int current = codNServ.get(codiceDipendente);
                    codNServ.put(codiceCliente, current+1);

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


        // stampo i dipendenti
        System.out.println("\nnome, codice, tipo, ore settimanali, specialita', telefono, medico, app.sett., costo orario");
        for (Dipendente d : dipendenti) {
            System.out.println(d);
        }
        System.out.println();

        // punto 4
        for (Cliente c : clienti) {
            System.out.println(c);
        }
        System.out.println();

        // punto 5
        System.out.println();
        int max = -1;
        String nome = null;
        for (Dipendente d : dipendenti) {
            int count = codNServ.get(d.getCodice());
            if (count > max) {
                max = count;
                nome = d.getNomeDipendente();
            }
        }
        System.out.println(nome + " " + max);
        
    }
}