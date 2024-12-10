
public class EsempioAlieno {

	public static void main(String[] args) 
	{
		GruppoAlieni ga = new GruppoAlieni(3);
		ga.aggiungiAlieno(new AlienoSerpente(100, "a"), 0);
		ga.aggiungiAlieno(new AlienoOrco(100, "b"), 1);
		ga.aggiungiAlieno(new AlienoUomoMarshmallow(100, "c"), 2);
		
		System.out.println("Danno: " + ga.calcolaDanno());
	}

}
