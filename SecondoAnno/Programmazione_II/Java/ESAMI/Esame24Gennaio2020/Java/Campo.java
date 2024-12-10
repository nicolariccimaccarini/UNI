package Esame24Gennaio2020.Java;

public class Campo {

    protected int codice, prenotazione;
    protected String campo;
    protected float larghezza, lunghezza, costoOrario;

    public Campo(int codice, String campo, float larghezza, float lunghezza, float costoOrario) {
        this.codice = codice;
        this.campo = campo;
        this.larghezza = larghezza;
        this.lunghezza = lunghezza;
        this.costoOrario = costoOrario;
    }

    public void addPrenotazione() {
        prenotazione++;
    }

    public float incasso() {
        return prenotazione*costoOrario;
    }
}
