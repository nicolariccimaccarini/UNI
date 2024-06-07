
public class ScontoQuantita extends PoliticaSconto
{
	private int minimo, percentuale;
	private double sconto;
	
	public ScontoQuantita(int minimo, int percentuale)
	{
		this.minimo = minimo;
		this.percentuale = percentuale;
	}
	
	public double CalcolaSconto(int numeroArticoli, double prezoArticolo)
	{
		if(numeroArticoli > this.minimo)
		{
			sconto = ((prezoArticolo*numeroArticoli)*percentuale)/100;
		}
		else
			sconto = 0;
		
		return sconto;
	}

	@Override
	public double PoliticaSconto(int numeroArticoli, double prezzoArticolo) {
		// TODO Auto-generated method stub
		return 0;
	}
}
