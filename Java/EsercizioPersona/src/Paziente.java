
public class Paziente extends Persona
{
	private String id;
	
	public Paziente()
	{
		super();
	}
	
	public Paziente(String nome, String id)
	{
		super(nome);
		this.id = id;
	}
	
	public String getId()
	{
		return id;
	}
}
