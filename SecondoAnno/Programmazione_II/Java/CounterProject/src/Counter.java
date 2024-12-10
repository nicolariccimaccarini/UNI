
public class Counter {
	protected int val;
    public Counter(){
        val=0;
    }
    
    public void reset(){
        val=0;
    }
    public void inc(){
        val++;
    }
    public void dec(){
        val--;
    }
    public int getValue(){
        return val;
    }
    public void copy(Counter x){
        val=x.val;
    }
}
