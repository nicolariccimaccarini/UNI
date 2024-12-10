import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

public class Gestione {

    static List<Corso> corsi = new LinkedList<Corso>();
    static List<Studente> studenti = new LinkedList<Studente>();
    static List<Libretto> libretti = new LinkedList<Libretto>();
    
    public static void main(String[] args) {
        
        // leggo il primo file (corsi.txt) 
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("corsi.txt"));
            String line = br.readLine();

            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String nomeCorso = br.readLine();
                String docente = br.readLine();

                line = br.readLine();
                if (tipo.equals("teoria")) {
                    tok = new StringTokenizer(line);
                    int codiceAula = Integer.parseInt(tok.nextToken());
                    int oreSettimanali = Integer.parseInt(tok.nextToken());
                    double oreLezione = Double.parseDouble(tok.nextToken());

                    Corso c = new Teoria(codice, nomeCorso, docente, codiceAula, oreSettimanali, oreLezione);
                    corsi.add(c);
                }
                else {
                    String nomeLab = line;
                    String assistente = br.readLine();
                    int nPostazioni = Integer.parseInt(br.readLine());

                    Corso c = new Laboratorio(codice, nomeCorso, docente, nomeLab, assistente, nPostazioni);
                    corsi.add(c);
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


        // leggo studenti.txt
        try {
            
            BufferedReader br = new BufferedReader(new FileReader("studenti.txt"));
            String line = br.readLine();

            while (line != null) {
                int matricola = Integer.parseInt(line);
                String nomeStudente = br.readLine();
                Studente s = new Studente(matricola, nomeStudente);
                studenti.add(s);

                line = br.readLine();
                while (line != null & !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codiceCorso = Integer.parseInt(tok.nextToken());
                    String voto = tok.nextToken();

                    Libretto l = new Libretto(codiceCorso, voto);
                    libretti.add(l);
                    s.addLibretto(l);
                    
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


        // stampo i corsi
        System.out.println("\nnome, codice, docente, tipo, aula, ore sett., ore/lez., lab., assistente, postazioni");
        for (Corso c : corsi) {
            System.out.println(c);
        }
        System.out.println();

        //stampo gli studenti
        for (Studente s : studenti) {
            System.out.println(s);
        }
        System.out.println();

        // punto 5
        for (Studente s : studenti) {
            if (Integer.parseInt(args[0]) == s.getMatricola()) {
                for (Corso c : corsi) {
                    if (s.getCodCorsoMax() == c.getCodice()) {
                        System.out.println(s.getNome() + " " + c.getNomeCorso() + " " + s.getPiuAlto());
                    }
                }
            }
        }

    }
}