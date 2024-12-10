
public class Controllo 
{
	public  Controllo(String[] args) throws NumberFormatException, IllegalArgumentException
	{
		for (int i=0; i<args.length; i++)
		{
			String numero = args[i];
			int num = Integer.parseInt(numero);
			if((num<-10) || (num>10))
				throw new IllegalArgumentException("Fuori intervallo");
			
			System.out.println("Numero: " + num);
		}
	}
}
