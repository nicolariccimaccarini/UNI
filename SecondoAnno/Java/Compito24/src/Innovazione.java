
public class Innovazione extends Progetto
{
	private int aziende;
	
	public Innovazione(int codice, String titolo, String coordinatore, String org, double importo, int aziende)
	{
		super(codice, titolo, coordinatore, org, importo);
		this.aziende = aziende;
	}
	
	public String toString()
	{
		return "innovazione\t" + super.toString() + "\t-\t-\t" + aziende + "\t" + importo*1000;
	}
}
