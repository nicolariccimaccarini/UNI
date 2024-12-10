public class Stack 
{
	private int count;
	private String[] items;
	
	public Stack(int max)
	{
		count = 0;
		items = new String[max];
	}
	
	public boolean isEmpty()
	{
		return (count == 0);
	}
	
	public void push(String value)
		throws StackOverflowException
	{
		try
		{
			items[count] = value;  // possibile eccezione
			count++;
		}
		
		catch (ArrayIndexOutOfBoundsException ae)
		{
			StackOverflowException oe = new StackOverflowException();
			throw oe;
		}
	}
	
	public String pop()
		throws StackUnderflowException
	{
		String value;
		
		try
		{
			count--;	// se ==0 diventa -1!
			value = items[count];	// possibile ecezione
			return value;
		}
		
		catch (ArrayIndexOutOfBoundsException ae)
		{
			count = 0; // azione correttiva!
			StackUnderflowException ue =
			new StackUnderflowException();
			throw ue;
		}
	}
		
	
}
