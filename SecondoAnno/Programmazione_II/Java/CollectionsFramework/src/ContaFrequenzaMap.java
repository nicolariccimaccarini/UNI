import java.util.*;

public class ContaFrequenzaMap 
{
	public static void main(String args[])
	{
		Map<String, Integer> m = new HashMap<String, Intrger>(); 
		// oppure TreeMap (tabella ordinata ma tempo di accesso non costante) o
		// Linked HashMap (tabella ordinata con tempo di accesso costante ma con costante di proporzionalita' piu' alta)
		for (int i=0; i<args.length; i++)
		{
			Integer freq = m.get(args[i]);
			m.put(args[i], (freq==null ? 1 : new freq + 1));
		}
		System.out.println(m.size() + " parole distinte: ");
		System.out.println(m);
	}
}
