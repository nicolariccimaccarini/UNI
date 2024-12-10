// stesso procedimento di Esempio2,java ma tutto in un unico file

package CounterFolder;

class Counter 
{
    private int val;
    public void reset(){
        val=0;
    }
    public void inc(){
        val++;
    }
    public int getValue(){
        return val;
    }

}

public class Esempio 
{
    public static void main(String[] args)
    {
        int n;
        Counter c1;
        c1 = new Counter();
        c1.reset();
        c1.inc();
        n = c1.getValue();
        System.out.println(n);
    }   
}
