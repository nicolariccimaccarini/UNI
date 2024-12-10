public class Squadra {
    
    protected int codice, nVittorie, nSconfitte;
    protected String nomeSquadra;

    public Squadra(int codice, String nomeSquadra, int nvittorie, int nSconfitte) {
        this.codice = codice;
        this.nomeSquadra = nomeSquadra;
        this.nVittorie = nvittorie;
        this.nSconfitte = nSconfitte;
    }

    public int getCode() {
        return codice;
    }

    public String getNome() {
        return nomeSquadra;
    }
}