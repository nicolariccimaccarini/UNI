import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.io.*;

public class Gestione 
{
	public static void main(String[] args) 
	{
		List<Progetto> progetti = new LinkedList<Progetto>();
		List<Ricercatore> ricercatori = new LinkedList<Ricercatore>();
		Map<Integer, Progetto> codProg = new HashMap<Integer, Progetto>();
		
		try
		{
			BufferedReader br = new BufferedReader(new FileReader("progetti.txt"));
			String line = br.readLine();
			
			while (line != null)
			{
				StringTokenizer tok = new StringTokenizer(line);
				int codice = Integer.parseInt(tok.nextToken());
				String tipo = tok.nextToken();
				String titolo = br.readLine();
				String coord = br.readLine();
				String org = br.readLine();
				line = br.readLine();
				tok = new StringTokenizer(line);
				
				if (tipo.equals("ricerca"))
				{
					String arg = tok.nextToken();
					int partner = Integer.parseInt(tok.nextToken());
					line = br.readLine();
					double importo = Double.parseDouble(line);
					
					Ricerca p = new Ricerca(codice, titolo, coord, org, importo, arg, partner);
					progetti.add(p);
					codProg.put(codice, p);
				}
				else
				{
					int aziende = Integer.parseInt(tok.nextToken());
					double importo = Double.parseDouble(tok.nextToken());
					
					Innovazione p = new Innovazione(codice, titolo, coord, org, importo, aziende);
					progetti.add(p);
					codProg.put(codice, p);
				}
				line = br.readLine();
			}
				
		}
		catch (Exception e)
		{
			System.out.println(e);
		}
		
		try
		{
			BufferedReader br = new BufferedReader(new FileReader("ricercatori.txt"));
			String line = br.readLine();
			
			while (line != null)
			{
				String nome = line;
				String cognome = br.readLine();
				Ricercatore r = new Ricercatore(nome, cognome);
				ricercatori.add(r);
				line = br.readLine();
				
				while (line != null && !line.equals(" "))
				{
					StringTokenizer tok = new StringTokenizer(line);
					int codice = Integer.parseInt(tok.nextToken());
					double ore = Double.parseDouble(tok.nextToken());
					r.addImpegno(codProg.get(codice), ore);
					line = br.readLine();
				}
				line = br.readLine();
			}
			
		}
		catch (Exception e)
		{
			System.out.println(e);
		}
		
		System.out.println("tipo, titolo, codice, coordinatore, organizzazione, argomento, partner, aziende, importo totale in migliaia di euro");
		for (Progetto p : progetti)
		{
			System.out.println(p);
		}
		
		for (Ricercatore r : ricercatori)
		{
			System.out.println(r);
		}
		
		for (Ricercatore r : ricercatori)
		{
			if (r.getCognome().equals(args[0]))
			{
				System.out.println(r.getNome() + "\t" + r.getCognome() + "\t" + r.getPiuAlto() + "\t" + r.getProgPiuAlto());
			}
		}
	}

}
