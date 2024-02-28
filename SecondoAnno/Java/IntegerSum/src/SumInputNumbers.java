
public class SumInputNumbers {

	public static void main(String[] args) 
	{
		if (args.length != 2)
		{
			System.out.println("Invalid arguments in the command line!");
		}
		
		int numero1 = Integer.parseInt(args[0]);
		int numero2 = Integer.parseInt(args[1]);
		int somma = numero1 + numero2;
		
		System.out.println("La somma di " + numero1 + " + " + numero2 + " e' = " + somma); 
		
	}

}
