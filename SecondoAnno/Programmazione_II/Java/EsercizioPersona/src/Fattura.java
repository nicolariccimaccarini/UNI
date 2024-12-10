
public class Fattura extends Persona
{
	private Dottore d;
	private Paziente p;
	
	public Fattura(Dottore d, Paziente p)
	{
		this.d = d;
		this.p = p;
	}
	
	public String getNomePaziente()
	{
		return p.getNome();
	}
	
	public String getNomeDottore()
	{
		return d.getNome();
	}
	
	public double importo()
	{
		return d.getParcella();
	}
}
