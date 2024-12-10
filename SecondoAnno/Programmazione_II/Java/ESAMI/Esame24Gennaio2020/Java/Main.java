package Esame24Gennaio2020.Java;

import java.util.*;
import java.io.*;

public class Main {
    
    public static void main(String[] args) {
        
        List<Campo> campo = new LinkedList<Campo>();
        List<Soci> soci = new LinkedList<Soci>();
        Map<Integer, Campo> codCampo = new HashMap<Integer, Campo>();

        try {
            BufferedReader br = new BufferedReader(new FileReader("campi.txt"));
            String line = br.readLine();
            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String nomeCampo = br.readLine();
                line = br.readLine();
                tok = new StringTokenizer(line);
                float larghezza = Float.parseFloat(tok.nextToken());
                float lunghezza = Float.parseFloat(tok.nextToken());

                if (tipo.equals("tennis")) {
                    float temperatura = Float.parseFloat(tok.nextToken());
                    String terreno = br.readLine();
                    float costoOrario = Float.parseFloat(br.readLine());
                    Campo tennis = new Tennis(codice, nomeCampo, larghezza, lunghezza, costoOrario, temperatura, terreno);
                    campo.add(tennis);
                }
                else {
                    float altezza = Float.parseFloat(tok.nextToken());
                    int piano = Integer.parseInt(tok.nextToken());
                    float costoOrario = Float.parseFloat(tok.nextToken());
                    Campo squash = new Squash(codice, nomeCampo, larghezza, lunghezza, costoOrario, altezza, piano);
                    campo.add(squash);
                }
                line = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }

        try {
            BufferedReader br = new BufferedReader(new FileReader("soci.txt"));
            String line = br.readLine();
            while (line != null) {
                int codice = Integer.parseInt(line);
                String nome = br.readLine();
                line = br.readLine();
                StringTokenizer tok = new StringTokenizer(line);
                int eta = Integer.parseInt(tok.nextToken());
                int categoria = Integer.parseInt(tok.nextToken());
                Soci s = new Soci(codice, nome, eta, categoria);
                soci.add(s);

                line = br.readLine();
                tok = new StringTokenizer(line);

                while (tok.hasMoreTokens()) {
                    int codiceCampo = Integer.parseInt(tok.nextToken());
                    int ora = Integer.parseInt(tok.nextToken());
                    Prenotazione p = new Prenotazione(codiceCampo, ora);
                    s.addPrenotazione(p);
                    codCampo.get(codCampo).addPrenotazione();
                }
                line = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("sport, nome del campo, codice, larghezza, lunghezza, temperatura, terreno, altezza, piano, costo");
        for (Campo c : campo) {
            System.err.println(c);
        }

        try {
            int codice = Integer.parseInt(args[0]);
            System.out.println(codCampo.get(codice).incasso());
        } catch (ArrayIndexOutOfBoundsException e) {
            e.printStackTrace();
        }
    }
}
