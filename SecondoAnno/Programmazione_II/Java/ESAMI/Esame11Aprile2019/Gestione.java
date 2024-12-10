import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public class Gestione {

    static List<Dipendente> dipendenti = new LinkedList<Dipendente>();
    static List<Cliente> clienti = new LinkedList<Cliente>();
    static Map<Integer,Dipendente> codDip = new HashMap<Integer,Dipendente>();
    static Map<Integer,Integer> codNServ = new HashMap<Integer,Integer>();
    
    public static void main(String[] args) {
        
        // leggo dal primo file (dipendenti.txt)
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("dipendenti.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String nomeDipendente = br.readLine();
                
                line = br.readLine();
                if (tipo.equals("trainer")) {
                    tok = new StringTokenizer(line);
                    int oreSettimanali = Integer.parseInt(tok.nextToken());
                    double costoOrario = Double.parseDouble(tok.nextToken());
                    String specialita = br.readLine();

                    Dipendente d = new Trainer(codice, nomeDipendente, oreSettimanali, costoOrario, specialita);
                    dipendenti.add(d);
                    codDip.put(codice,d);
                    codNServ.put(codice,0);
                }
                else {
                    tok = new StringTokenizer(line);
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

        // leggo dal secondo file (clienti.txt)
        try {
    
            BufferedReader br = new BufferedReader(new FileReader("dipendenti.txt"));
            String line = br.readLine();

            while (line != null) {

                int codiceCliente = Integer.parseInt(line);
                String nomeCliente = br.readLine();
                Cliente c = new Cliente(codiceCliente, nomeCliente);
                clienti.add(c);
                line = br.readLine();

                while (line != null && !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codice = Integer.parseInt(tok.nextToken());
                    double ore = Double.parseDouble(tok.nextToken());
                    Dipendente dip = codDip.get(codice);
                    c.addServizioo(dip, ore);
                    int current = codNServ.get(codice);
                    codNServ.put(codice, current+1);
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

        // stampa dei dipendenti
        System.out.println("nome, codice, tipo, ore settimanali, specialità, telefono, medico, app.sett., costo orario");
        for (Dipendente d : dipendenti) {
            System.out.println(d);
        }
        System.out.println();

        // stampa dei clienti
        for (Cliente c : clienti) {
            System.out.println(c);
        }
        System.out.println();

        // stampa del dipendente che ha fatto piu' servizi
        int max = -1;
        String nome = null;
        for (Dipendente d : dipendenti) {
            int counter=codNServ.get(d.getCodice());
            if (counter > max) {
                max = counter;
                nome = d.getNome();
            }
        }
        System.out.println(nome + " " + max);
    }
}
