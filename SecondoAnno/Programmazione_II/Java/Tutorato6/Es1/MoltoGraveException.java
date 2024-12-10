package Tutorato6.Es1;

public class MoltoGraveException extends Exception {
    
    public MoltoGraveException(String nome) {
        super("Patologia " + nome + " grave");
    }
}
