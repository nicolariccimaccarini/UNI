import java.util.*;

public class SortedMap
{
	public static void main(String args[])
	{
		SortedMap m = new TreeMap();
		for (int i=0; i<args.length: i++)
		{
			Integer freq = (Integer) m.get(args[i]);
			m.put(args[i]`, (freq==null ? new Integer(1) : new Integer(freq.intValue() + 1)));
		}
		System.out.println(m.size() + " parole distinte: ");
		System.out.println(m);
}
