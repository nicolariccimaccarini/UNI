import java.util.*;

public class NameSort 
{
	public static void main(String[] args)
	{
		Persona elencoPersone[] = 
			{
					new Persona("Eugenio", "Bennato"),
					new Persona("Roberto", "Benigni"), 
					new Persona("Edoardo", "Bennato"), 
					new Persona("Bruno", "Vespa")
			};
		List l = Arrays.asList(elencoPersone);
		Collections.sort(l);
		System.out.println(l);
	}
}
