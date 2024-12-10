
public class Counter 
{
	private int val;
	
	public Counter()
	{
		val = 0;
	}
	
	public void reset()
	{
		val = 0;
	}
	
	public void inc()
	{
		val++;
	}
	
	public int getValue()
	{
		return val;
	}
}
