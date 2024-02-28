
public class Progetto 
{
	protected int codice;
	protected String titolo, coordinatore, org;
	protected double importo;
	
	public Progetto(int codice, String titolo, String coordinatore, String org, double importo)
	{
		this.codice = codice;
		this.titolo = titolo;
		this.coordinatore = coordinatore;
		this.org = org;
		this.importo = importo;
	}
	
	public int getCodice()
	{
		return codice;
	}
	
	public String getTitolo()
	{
		return titolo;
	}
	
	public String toString()
	{
		return titolo + "\t" + codice + "\t" + coordinatore + "\t" + org;
	}
}
