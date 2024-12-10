import java.io.*;
import java.util.Scanner;

public class CheckInventoryWithScanner 
{
	public static void main (String[] args)
	{
		String name;
		int units, count = 0;
		float price;
		String file = "inventory.dat";
		
		InventoryItem[] items = new InventoryItem[100];
		
		try
		{
			Scanner sc = new Scanner(new File(file));
			
			while (sc.hasNext())
			{
				name = sc.next();
				try
				{
					units = sc.nextInt();
					price = sc.nextFloat();
					items[count++] = new InventoryItem(name, units, price);
				}
				
				catch (NumberFormatException e)
				{
					System.out.println("Error in input");
				}
			}
			
			sc.close();
			
			// Scrive a video i dati letti
			for (int scan=0; scan<count; scan++)
				System.out.println(items[scan]);
		}
		
		// Geestione delle eccezioni di cascata
		
		catch (FileNotFoundException e)
		{
			System.out.println("File" + file + " not found.");
		}
	}
}
