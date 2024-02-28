
public class EsempioOrologio {

	public static void main(String[] args) 
	{
		Orologio o;
		o = new Orologio();
		
		for(int i=0; i<70; i++)
		{
			o.tic(); 
		} 
		System.out.println("Ore: " + o.getOre() + ", Minuti: " + o.getMinuti());
		o.reset();
	}

}
