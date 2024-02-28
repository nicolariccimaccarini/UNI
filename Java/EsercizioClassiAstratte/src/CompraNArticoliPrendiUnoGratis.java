
public class CompraNArticoliPrendiUnoGratis extends PoliticaSconto
{
	private int n;
	private double sconto;
	
	public CompraNArticoliPrendiUnoGratis(int n)
	{
		this.n = n;
	}
	
	public double CalcolaSconto(int numeroArticoli, double prezzoArticolo)
	{
		sconto = ((double) numeroArticoli/n) * prezzoArticolo;
		return sconto;
	}

	@Override
	public double PoliticaSconto(int numeroArticoli, double prezzoArticolo) {
		// TODO Auto-generated method stub
		return 0;
	}
}
