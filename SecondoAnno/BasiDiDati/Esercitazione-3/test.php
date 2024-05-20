<?php
/**
 * Questo file PHP esegue solamente il test della connessione al database.
 * Mostra "Ok!" oppure la spiegazione dell'errore.
 */

// Da PHP 8.x la connessione lancia un'eccezzione all'errore.   
try {
	// Connessione 
	$link = mysqli_connect("172.25.3.54", "nicola", "password", "Azienda");	// db_ip, db_username, db_password, db_name

} catch (mysqli_sql_exception $e) {
	die("Non posso stabilire la connessione al db: " . $e->getMessage());
}

echo "OK!" . PHP_EOL;

mysqli_close($link);

?>