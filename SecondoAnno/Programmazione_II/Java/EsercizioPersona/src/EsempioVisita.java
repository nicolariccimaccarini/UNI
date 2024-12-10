
public class EsempioVisita {

	public static void main(String[] args) 
	{
		Paziente p1 = new Paziente("Mario Rossi", "martiorossi");
		Paziente p2 = new Paziente("Giovanni Verdi", "giovanniverdi");
		
		Dottore d1 = new Dottore("Franco Milani", "Chirurgo", 100.00);
		Dottore d2 = new Dottore("Giuseppe Neri", "Pediatra", 200.00);
		
		Fattura f1 = new Fattura(d1, p1);
		Fattura f2 = new Fattura(d2, p2);
		
		System.out.println("Totale fattura: " + (f1.importo() + f2.importo()));
	}

}
