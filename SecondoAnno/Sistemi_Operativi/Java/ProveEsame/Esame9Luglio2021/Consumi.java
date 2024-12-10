package Java.ProveEsame.Esame9Luglio2021;

public class Consumi {
    
    private float consumi = 0.0F;

    public synchronized float getConsumi() {
        return consumi;
    }

    public synchronized void setConsumi(float consumi) {
        this.consumi = consumi;
    }
}
