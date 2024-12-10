
public class Main {

	public static void main(String[] args) 
	{
		try
		{
			Controllo esempio = new Controllo(args);
		}
		
		catch(NumberFormatException e)
		{
			System.out.println("Format exception 1: " + e.getMessage());
		}
		
		catch(IllegalArgumentException e)
		{
			System.out.println("Format exception 2: " + e.getMessage());
		}
	}

}
