import java.util.*;

public class EsListIt 
{
	public static void main(String args[])
	{
		List l = new ;
		for (int i=0; i<args.length; i++)
			l.add(args[i]);
		
		for (ListIterator i = l.listIterator(l.size()); i.hasPrevious();)
			System.out.println(i.previous() + " ");
			
				
	}
}
