package ESAMI.Esame22giugno2018;

import java.util.*;
import java.io.*;

public class Main {

    static List<Corso> corsi   = new LinkedList<Corso>();
    static List<Studente> studenti   = new LinkedList<Studente>();
    static Map<Integer,String> nomeCorsi = new HashMap<Integer,String>();
    
    public static void main(String[] args) {
        
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
                tok = new StringTokenizer(line);

                if (tipo.equals("teoria")) {
                    int codiceAula = Integer.parseInt(tok.nextToken());
                    int numeroOreSettimanali = Integer.parseInt(tok.nextToken());
                    Double numeroOreLezione = Double.parseDouble(tok.nextToken());
                    Teoria t = new Teoria(codice, nomeCorso, docente, codiceAula, numeroOreSettimanali, numeroOreLezione);
                    corsi.add(t);
                }
                else {
                    String nomeLaboratorio = br.readLine();
                    String nomeAssistente = br.readLine();
                    int numeroPostazioni = Integer.parseInt(br.readLine());
                    Lab l = new Lab(codice, nomeCorso, docente, nomeLaboratorio, nomeAssistente, numeroPostazioni);
                    corsi.add(l);
                }

                line = br.readLine();
                line = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
