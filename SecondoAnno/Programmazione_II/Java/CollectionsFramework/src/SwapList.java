import java.util.*;

public class SwapList 
{
	static void swap(List a, int i, int j)
	{
		Object tmp = a.get(i);
		a.set(i, a.get(j));
		a.set(j, tmp);
	}
	
	public static void main(String args[])
	{
		List list = new LinkedList(); // ooure ArrayList --> piu' veloce ma non implementa le interfacce di Queue e Deque
		for (int i=0; i<args.length; i++)
			list.add(args[i]);
		
		System.out.println(list);
		swap(list, 2, 3);
		System.out.println(list);
	}
}
