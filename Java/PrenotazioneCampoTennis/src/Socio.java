import java.util.*;

public class Socio 
{
	private String nomeCognome;
	private int codice, eta, categoria;
	protected List<Prenotazione> prenotazioni;
	
	public Socio(int c, String n, int e, int categoria)
	{
		codice = c;
		nomeCognome = n;
		eta = e;
		this.categoria = categoria;
		prenotazioni = new LinkedList<Prenotazione>();
	}
	
	public void addPrenotazione(Prenotazione p)
	{
		prenotazioni.add(p);
	}
	
	public String toString()
	{
		return codice + "\t" + nomeCognome + "\t" + eta + "\t" + categoria + "\t" + prenotazioni;
	}
}
