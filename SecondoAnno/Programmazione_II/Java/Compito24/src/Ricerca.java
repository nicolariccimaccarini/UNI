
public class Ricerca extends Progetto
{
	private String argomento;
	private int partner;
	
	public Ricerca(int codice, String titolo, String coordinatore, String org, double importo, String argomento, int partner)
	{
		super(codice, titolo, coordinatore, org, importo);
		this.argomento = argomento;
		this.partner = partner;
	}
	
	public String toString()
	{
		return "ricerca\t" + super.toString() + "\t" + argomento + "\t" + partner + "\t" + importo*1000;
	}
}
