from Campo import Campo

class Squash:

    def __init__(self, codice, campo, larghezza, lunghezza, costoOrario, altezza, piano) -> None:
        super().__init__(codice, campo, larghezza, lunghezza, costoOrario)
        self.altezza = altezza
        self.piano = piano

    def __str__(self) -> str:
        return "squash" + "\t" + self.campo + "\t" + str(self.codice) + "\t" + str(self.larghezza) + "\t" + str(self.lunghezza) + "\t" + str(self.altezza) + "\t" + str(self.piano) + "\t" + str(self.costoOrario)