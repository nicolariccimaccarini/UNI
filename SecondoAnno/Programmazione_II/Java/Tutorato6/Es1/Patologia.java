package Tutorato6.Es1;

public class Patologia {

    private String nomePatologia;
    private int gradoGravita;

    public Patologia(String nomePatologia, int gradoGravita) {
        this.nomePatologia = nomePatologia;
        this.gradoGravita = gradoGravita;
    }

    public String getNomePatologia() {
        return nomePatologia;
    }

    public int getGradogravita() {
        return gradoGravita;
    }

    public void aggrava() throws MoltoGraveException {

        if (gradoGravita < 10) {
            gradoGravita++;
        } 
        else {
            throw new MoltoGraveException(nomePatologia);
        }
    }

    public void attenua() throws GuaritaException {

        if (gradoGravita > 1) {
            gradoGravita--;
        }
        else {
            throw new GuaritaException(nomePatologia);
        }
    }

    public boolean equalsa(Object o) {
        Patologia p = (Patologia) o;
        boolean esito = p.getNomePatologia().equals(this.nomePatologia);
        return esito;
    }

    public String toString() {
        return this.nomePatologia + ", Gravita': " + this.gradoGravita;
    }
}
