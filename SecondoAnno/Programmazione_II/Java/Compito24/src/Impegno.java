
public class Impegno 
{
	private double ore;
	private Progetto p;
	
	public Impegno(Progetto p, double ore)
	{
		this.ore = ore;
		this.p = p;
	}
	
	public Double getOre()
	{
		return ore;
	}
	
	public String getTitolo()
	{
		return p.getTitolo();
	}
	
	public String toString()
	{
		return "(" + p.getCodice() + "," + ore + ")";
	}
}
