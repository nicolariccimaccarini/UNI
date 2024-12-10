
public class Persona implements Comparable
{
	private String nome, cognome;
	public Persona(String nome, String cognome)
	{
		this.nome = nome;
		this.cognome = cognome;
	}
	
	public String nome()
	{
		return nome;
	}
	
	public String cognome()
	{
		return cognome;
	}
	
	public String toString()
	{
		return nome + " " + cognome;
	}
	
	public int compareTo(Object x)
	{
		Persona p = (Persona) x;
		int confrontoCognomi = cognome.compareTo(p.cognome);
		return (confrontoCognomi!=0 ? confrontoCognomi : nome.compareTo(p.nome));
	}
	
}
