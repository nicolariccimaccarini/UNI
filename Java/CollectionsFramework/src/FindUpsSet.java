import java.util.*;

public class FindUpsSet
{
	public static void main(String[] args)
	{
		Set s = new HashSet(); // oppure TreeSet (inserimento ordinato, ma tempo di accesso non costante
		
		for (int i=0; i<args.length; i++)
			if (!s.add(args[i]))
				System.out.println("Parola duplicata: " + args[i]);
		
		System.out.println(s.size() + " parole distinte: " + s); 
	}
}
