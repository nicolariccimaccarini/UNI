package Tutorato6.Es1;

import java.util.ArrayList;
import java.util.List;

public class Pazienti {
    
    private String cognome;
    private List<Patologia> pat;

    public Pazienti(String cognome) {
        this.cognome = cognome;
        this.pat = new ArrayList<Patologia>();
    }

    public String getCognome() {
        return cognome;
    }

    public void aggiungiPatologia(Patologia p) {
        
        if (!this.pat.contains(p)) {
            this.pat.add(p);
        }
    }

    public boolean rimuoviPatologia(Patologia p) {

        if (this.pat.contains(p)) {
            this.pat.remove(p);
            return true;
        }
        else {
            return false;
        }
    }

    public String toString() {
        String temp="*****************\nPaziente: "+this.cognome+" \nPatologie:\n";
        
        for(int i=0; i<this.pat.size(); i++){
            temp=temp+"- "+this.pat.get(i).toString();
        }

        return temp;
    }
}
