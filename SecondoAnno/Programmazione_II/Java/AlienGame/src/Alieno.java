
public class Alieno 
{
	protected int salute, danno;
	protected String nome;
	
	public Alieno(int salute, String nome)
	{
		this.salute = salute;
		this.nome = nome;
	}
	
	public int getDanno()
	{
		return danno;
	}
}

class AlienoSerpente extends Alieno
{
	public AlienoSerpente(int salute, String nome)
	{
		super(salute, nome);
		danno = 10;
	}
}

class AlienoOrco extends Alieno
{
	public AlienoOrco(int salute, String nome)
	{
		super(salute, nome);
		danno = 6;
	}
}

class AlienoUomoMarshmallow extends Alieno
{
	public AlienoUomoMarshmallow(int salute, String nome)
	{
		super(salute, nome);
		danno = 1;
	}
}