
public class CounterDec {

	public static void main(String[] args) 
	{
		int n;
		BiCounter c1;
		c1 = new BiCounter();
		c1.reset();
		c1.inc();
		c1.inc();
		n = c1.getValue();
		System.out.println("n = " + n);
		c1.dec();
		n = c1.getValue();
		System.out.println("n decremented = " + n);
	}

}
