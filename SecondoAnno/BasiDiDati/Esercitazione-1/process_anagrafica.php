<?php

// Array multidimensionale predefinito con alcune anagrafiche "hard=coded"
$anagrafiche = [
    ['nome' => 'Mario', 'cognome' => 'Rossi', 'data_di_nascita' => '1980-01-01', 'indirizzo' => 'Via Roma 1, Roma'],
    ['nome' => 'Luigi', 'cognome' => 'Bianchi', 'data_di_nascita' => '1990-02-02', 'indirizzo' => 'Via Milano 2, Milano'],
];

// Se il modulo e' stato inviato, aggiungi i dati ricevuti all'array
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    $nuova_anagrafica = [
        'nome' => htmlspecialchars($_POST['nome']),
        'cognome' => htmlspecialchars($_POST['cognome']),
        'data_di_nascita' => htmlspecialchars($_POST['data_di_nascita']),
        'indirizzo' => htmlspecialchars($_POST['indirizzo']),
    ];
    $anagrafiche[] = $nuova_anagrafica;
}

// Funzione per visualizzare l'array multidimensioale in forma tabellare
function visualizzaAnagrafiche($anagrafiche) {
    echo '<table border="1">';
    echo '<tr><th>Nome</th><th>Cognome</th><th>Data di Nascita</th><th>Indirizzo</th></tr>';
    foreach ($anagrafiche as $anagrafica) {
        echo '<tr>';
        echo '<td>' . $anagrafica['nome'] . '</td>';
        echo '<td>' . $anagrafica['cognome'] . '</td>';
        echo '<td>' . $anagrafica['data_di_nascita'] . '</td>';
        echo '<td>' . $anagrafica['indirizzo'] . '</td>';
        echo '</tr>';
    }
    echo '</table>';
}

visualizzaAnagrafiche($anagrafiche)

?>