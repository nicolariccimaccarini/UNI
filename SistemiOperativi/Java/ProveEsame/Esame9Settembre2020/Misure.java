package Java.ProveEsame.Esame9Settembre2020;

import java.io.Serializable;

public class Misure implements Serializable {
    float temperatura = 0.0F;
    int umidita = 0;
    
    public Misure(float temperatura, int umidita) {
        this.temperatura = temperatura;
        this.umidita = umidita;
    }

    public float getTemperatura(float temperatura) {
        return temperatura;
    }

    public int getUmidita() {
        return umidita;
    }

    public void setTemperatura(float temperatura) {
        this.temperatura = temperatura;
    }

    public void setUmidita(int umidita) {
        this.umidita = umidita;
    }
}
