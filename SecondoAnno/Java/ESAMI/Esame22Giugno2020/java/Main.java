package Esame22Giugno2020.java;

import java.io.*;
import java.util.*;

public class Main {
    
    static List<Veicolo> veicoli = new LinkedList<Veicolo>();
    static List<Cliente> clienti = new LinkedList<Cliente>(); 

    public static void main(String[] args) {
      
        // leggo il primo file
        try {
            BufferedReader br = new BufferedReader(new FileReader("veicoli.txt"));
            String line = br.readLine();
            while (line != null) {
                StringTokenizer tok = new StringTokenizer(line);
                int codice = Integer.parseInt(tok.nextToken());
                String tipo = tok.nextToken();
                String targa = tok.nextToken();
                String modello = br.readLine();
                String marca = br.readLine();

                line = br.readLine();
                if (tipo == "auto") {
                    tok = new StringTokenizer(line);
                    int cilindrata = Integer.parseInt(tok.nextToken());
                    double bagagliaio = Double.parseDouble(tok.nextToken());
                    double costoGiornaliero = Double.parseDouble(br.readLine());
                    Veicolo v = new Auto(codice, targa, modello, marca, costoGiornaliero, cilindrata, bagagliaio);
                    veicoli.add(v);
                }
                else {
                    String categoria = line;
                    line = br.readLine();
                    tok = new StringTokenizer(line);
                    int numeroPosti = Integer.parseInt(tok.nextToken());
                    boolean vanoCarico = Boolean.parseBoolean(tok.nextToken());
                    double costoGiornaliero = Double.parseDouble(br.readLine());
                    Veicolo v = new Furgone(codice, targa, modello, marca, costoGiornaliero, categoria, numeroPosti, vanoCarico);
                    veicoli.add(v);
                }
                line = br.readLine();
            }
            br.close();
        } catch (IOException e) {
            System.err.println(e);
        } catch (NumberFormatException e) {
            System.err.println(e);
        }

        // leggo secondo file
        try {
            BufferedReader br = new BufferedReader(new FileReader("clienti.txt"));
            String line = br.readLine();    
            while (line != null) {
                String nome = line;
                Cliente c = new Cliente(nome);
                clienti.add(c);
                while (line != null && !line.equals("")) {
                    StringTokenizer tok = new StringTokenizer(line);
                    int codiceVeicolo = Integer.parseInt(tok.nextToken());
                    int numeroGiorni = Integer.parseInt(tok.nextToken());
                    Noleggio n = new Noleggio(codiceVeicolo, numeroGiorni);
                    c.addNoleggio(n);
                    line = br.readLine();
                } 
            }
            br.close();
        } catch (IOException e) {
            System.err.println(e);
        } catch (NumberFormatException e) {
            System.err.println(e);
        }

        System.out.println("tipo, targa, codice, modello, marca, costo giornaliero, cilindrata, bagagliaio, categoria, numero posti, vano di carico");

        for (Veicolo v : veicoli) {
            System.out.println(v);
        }

        for (Cliente c : clienti) {
            System.out.println(c);
        }

    } //main
}