public class Privato extends Cliente {

    private String nomeCognome, numeroCartaDiCredito;
   
    public Privato(int codice, String indirizzo, String nomeCognome, String numeroCartaDiCredito) {
        super(codice, indirizzo);
        this.nomeCognome = nomeCognome;
        this.numeroCartaDiCredito = numeroCartaDiCredito;
    }

    public String toString() {
        return "Privato" + "\t" + codice + "\t" + nomeCognome + "\t" + numeroCartaDiCredito + "\t-\t-\t-\t" + indirizzo;
    }
    
}
