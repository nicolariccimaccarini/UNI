package CounterFolder;

public class Esempio2 
{
    public static void main(String[] args)
    {
        int n;
        Counter c1, c2;
        c1 = new Counter();
        c1.inc();
        c2 = new Counter();
        c2.copy(c1);
        n = c2.getValue();
        System.out.println(n);
    }   
}