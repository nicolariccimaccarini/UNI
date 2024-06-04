public class Accumulatore {
    
    private double contatore;

    public Accumulatore(double value) {
        contatore = value;
    }

    public void addValue(double value) {
        double temp = contatore;
        temp += value;
        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {}
        contatore = temp;
    }

    public double getValue() {
        return contatore;
    }
}