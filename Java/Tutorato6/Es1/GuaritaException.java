package Tutorato6.Es1;

public class GuaritaException extends Exception {
    
    public GuaritaException(String nome) {
        super(nome + " guarita");
    }
}
