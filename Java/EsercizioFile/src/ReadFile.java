import java.io.*;

public class ReadFile 
{
	public ReadFile(String filename) throws IOException
	{
		FileReader fr = new FileReader(filename);
		BufferedReader inFile = new BufferedReader(fr);
		
		System.out.println("Il testo salvato e': ");
		String linea = inFile.readLine();
		while(linea != null)
		{
			System.out.println(linea);
			linea = inFile.readLine();
		}
		
		System.out.println();
		
		inFile.close();
	}
}
