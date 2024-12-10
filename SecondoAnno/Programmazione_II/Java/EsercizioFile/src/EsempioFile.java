import java.io.*;

public class EsempioFile 
{
	public static void main(String[] args)
	{
		// scrittura su file
		try
		{
			WriteLine wr = new WriteLine(args[0]);
		}
		
		catch(IOException e)
		{
			System.out.println("Error I/O");
			System.exit(1);
		}
		
		catch(ArrayIndexOutOfBoundsException e)
		{
			System.out.println("errore passaggio parametri");
			System.exit(1);
		}
		
		
		// lettura da file
		try
		{
			ReadFile rf = new ReadFile(args[0]);
		}
		
		catch(IOException e)
		{
			System.out.println("Error I/O");
			System.exit(1);
		}
		
		catch(ArrayIndexOutOfBoundsException e)
		{
			System.out.println("errore passaggio parametri");
			System.exit(1);
		}
	}
}
