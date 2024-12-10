<?php

	$file = fopen('Insegnamento.csv', 'r');
    
	while (($row = fgetcsv($file, 1000, ";")) !== FALSE) {
		// Posso visualizzare il contenuto strutturato con print_r()
		print_r($row);
		
		// Posso accedere ai dati con $row[$index] ... 
		// $row[0] = str_replace("'", "\'", $row[0]); // Eseguo l'escape dell'apostrofo
 		
		// Costruisco la query per l'inserimento  ... 
		//$sql = "INSERT INTO INSEGNAMENTO VALUES('$row[0]', '$row[1]', '$row[2]', '$row[3]')"
		
		// Eseguo la query ... 
		// [...]
	}
	
?>