import java.util.LinkedList;
import java.util.List;

public class Ricercatore 
{
	private String nome, cognome;
	List<Impegno> impegni;
	
	public Ricercatore(String nome, String cognome)
	{
		this.nome = nome;
		this.cognome = cognome;
		impegni = new LinkedList<Impegno>();
	}
	
	public void addImpegno(Progetto p, double ore)
	{
		impegni.add(new Impegno(p, ore));
	}
	
	public double getTot()
	{
		double tot = 0.0;
		
		for (Impegno i : impegni)
		{
			tot = tot + i.getOre();
		}
		
		return tot;
	}
	
	public String getNome()
	{
		return nome;
	}
	
	public String getCognome()
	{
		return cognome;
	}
	
	public double getPiuAlto()
	{
		double max = 0.0;
		
		for (Impegno i : impegni)
		{
			if (i.getOre() > max)
				max = i.getOre();
		}
		
		return max;
	}
	
	public String getProgPiuAlto()
	{
		double max = 0.0;
		String titolo = "";
		
		for (Impegno i : impegni)
		{
			if (i.getOre() > max)
			{
				max = i.getOre();
				titolo = i.getTitolo();
			}
		}
		
		return titolo;
	}
	
	public String toString()
	{
		return nome + "\t" + cognome + "\t" + getTot() + "\t" + impegni.size() + "\t" + impegni;
	}
}
