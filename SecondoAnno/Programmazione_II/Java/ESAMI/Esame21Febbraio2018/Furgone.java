public class Furgone extends Veicolo {

    private String categoria; 
    private int numeroPosti;
    
    public Furgone(int codice, String targa, String modello, String marca, String categoria, int numeroPosti) {
        super(codice, targa, modello, marca, 15);
        this.categoria = categoria;
        this.numeroPosti = numeroPosti;
    }

    public String toString() {
        return "commerciale" + "\t" + super.toString() + "\t-\t-\t" + categoria + "\t" + numeroPosti;
    }
    
}
