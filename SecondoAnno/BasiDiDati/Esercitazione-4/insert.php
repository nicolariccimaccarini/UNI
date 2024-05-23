<?php

    include_once('connsessione.php');

    $NOME_BATT  = $_POST['NOME_BATT'];
    $INIZ_INT   = $_POST['INIZ_INT'];
    $COGNOME    = $_POST['COGNOME'];
    $SSN        = $_POST['SSN'];
    $DATA_N     = $_POST['DATA_N'];
    $INDIRIZZO     = $_POST['INDIRIZZO'];
    $SESSO         = $_POST['SESSO'];
    $STIPENDIO     = $_POST['STIPENDIO'];
    $SUPER_SSN     = $_POST['SUPER_SSN'];
    $N_D         = $_POST['N_D'];

    $sql = "INSERT INTO IMPIEGATO
                VALUES ('$NOME_BATT', '$INIZ_INT', '$COGNOME', '$SSN', 
                        '$DATA_N', '$INDIRIZZO', '$SESSO', $STIPENDIO
                        '$SUPER_SSN', $N_D)";

    $query = mysqli_query($link, $sql);

    if (!$query) {
        echo "Si e' verificato un errore: " . mysqli_error($link);
        exit;
    }

    mysqli_close($link);

?>


<html lang="en">

    <head>
        <meta charset="UTF-8">
        
        <title>Esercitazione 4 - insert.php</title>

        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/water.css@2/out/water.css">

        <style>
            body {
                max-width: 1200px;
            }
        </style>
    </head>

    <body>
            <p>Ho inserito il nuovo parametro impiegato <?php echo $NOME_BATT; ?></p>
    </body>

</html>