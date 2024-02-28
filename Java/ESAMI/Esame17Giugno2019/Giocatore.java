public class Giocatore {
    
    private int codiceSquadra, eta, numeroMaglia;
    private String cognome, nome, ruolo;
    private boolean titolare;

    private String nomeSquadra;

    public Giocatore(int codiceSquadra, String cognome, String nome, boolean titolare, int eta, int numeroMaglia, String ruolo) {
        this.codiceSquadra = codiceSquadra;
        this.cognome = cognome;
        this.nome = nome;
        this.titolare = titolare;
        this.eta = eta;
        this.numeroMaglia = numeroMaglia;
        this.ruolo = ruolo;
        this.nomeSquadra = "";
    }

    public int getCode() {
        return codiceSquadra;
    }

    public void setNome(String nome) {
        this.nomeSquadra = nome;
    }

    public String toString() {
        return nome + "\t" + cognome + "\t" + eta + "\t" + numeroMaglia + "\t" + ruolo + "\t" + nomeSquadra ;
    }

}
