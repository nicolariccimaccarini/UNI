public class Studente extends Iscritto {

    private float votoMedio;
    
    public Studente(String nomeCognome, int codice, int eta, String indirizzo, float votoMedio) {
        super(nomeCognome, codice, eta, indirizzo);
        this.votoMedio = votoMedio;
    }

    public String toString() {
        return "studente" + "\t" + nomeCognome + "\t" + codice + "\t" + eta + "\t-\t" + votoMedio + "\t" + indirizzo;
    }
    
}
