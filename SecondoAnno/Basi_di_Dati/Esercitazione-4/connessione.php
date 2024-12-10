<?php

    // Connessione
    $link = mysqli_connect("172.25.3.54", "nicola", "password", "Azienda");

    // Controllo se e' avvenuta la connessione al database
    if (!$link) {
        echo "Si e' verificato un errore: non riesco a collegarmi al database <br/>";
        echo "Codice di errore: " . mysqli_connect_errno() . "<br/>";
        echo "Messaggio di errore: " . mysqli_connect_error() . "<br/>";
        exit;
    }

?>