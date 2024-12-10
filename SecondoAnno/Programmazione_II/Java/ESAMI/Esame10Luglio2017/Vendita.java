public class Vendita {
    
    private int codiceVendita, codiceCliente;
    private String data, descrizioneProdotto;
    private int quantitaVenduta, prezzoUnita;
    private int totale;
    private double totaleSenzaIva;

    public Vendita(int codiceVendita, int codiceCliente, String data, String descrizioneProdotto, int quantitaVenduta, int prezzoUnita) {
        this.codiceVendita = codiceVendita;
        this.codiceCliente = codiceCliente;
        this.data = data;
        this.descrizioneProdotto = descrizioneProdotto;
        this.quantitaVenduta = quantitaVenduta;
        this.prezzoUnita = prezzoUnita;
        this.totale = quantitaVenduta*prezzoUnita;
        this.totaleSenzaIva = totale/1.22;
    }

    public int getCodice() {
        return codiceVendita;
    }

    public int getCodiceCliente() {
        return codiceCliente;
    }

    public String toString() {
        return data + "\t" + descrizioneProdotto + "\t" + quantitaVenduta + "\t" + prezzoUnita + "\t" + totale + "\t" + totaleSenzaIva;
    }
}
