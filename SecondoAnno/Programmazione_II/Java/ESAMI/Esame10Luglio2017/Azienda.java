public class Azienda extends Cliente {

    private String ragioneSociale;
    private int partitaIva, numeroDipendenti;
    private String IBAN;
    
    public Azienda(int codice, String indirizzo, String ragioneSociale, int partitaIva, int numeroDipendenti, String IBAN) {
        super(codice, indirizzo);
        this.ragioneSociale = ragioneSociale;
        this.partitaIva = partitaIva;
        this.numeroDipendenti = numeroDipendenti;
        this.IBAN = IBAN;
    }

    public String toString() {
        return "Azienda" + "\t-\t-\t" + ragioneSociale + "\t" + partitaIva + "\t" + numeroDipendenti + "\t" + IBAN + "\t" + indirizzo;
    }
    
}
