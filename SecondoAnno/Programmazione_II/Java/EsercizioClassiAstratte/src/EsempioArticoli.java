
public class EsempioArticoli 
{
	public static void main(String[] args)
	{
		double sconto1, sconto2;
		
		ScontoQuantita sq = new ScontoQuantita(10, 12);
		sconto1 = sq.CalcolaSconto(12, 23.3);
		System.out.println(sconto1);
		
		CompraNArticoliPrendiUnoGratis cnapug = new CompraNArticoliPrendiUnoGratis(3);
		sconto2 = cnapug.CalcolaSconto(3, 10);
		System.out.println(sconto2);
	}
}
