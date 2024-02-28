package CounterFolder;

public class Decremento 
{
    
    public static void main(String[] args)
    {
        int n;
        Counter c;
        c = new Counter();
        c.inc();
        c.inc();
        n = c.getValue();
        System.out.println(n);
        c.dec();
        n = c.getValue();
        System.out.println(n);
    }

}
