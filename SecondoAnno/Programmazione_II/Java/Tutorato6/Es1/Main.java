package Tutorato6.Es1;

public class Main {

    public static void main(String[] args) {
        
        Patologia asma = new Patologia("asma", 10);
        Patologia graminacee = new Patologia("graminacee", 2);
        Pazienti Rossi = new Pazienti("Rossi");

        Rossi.aggiungiPatologia(graminacee);
        Rossi.aggiungiPatologia(asma);
        Rossi.toString();

        try {
            asma.aggrava();
        } catch (MoltoGraveException e) {
            System.out.println(e.getMessage());
        }

        try {
            graminacee.attenua();
            graminacee.attenua();
        } catch (GuaritaException e) {
            System.out.println(e.getMessage());
        }

        Rossi.toString();
    }
}
