from Campo import Campo

class Tennis:

    def __init__(self, codice, campo, larghezza, lunghezza, temperatura, costoOrario, terreno) -> None:
        super().__init__(codice, campo, larghezza, lunghezza, costoOrario)
        self.temperatura = temperatura
        self.terreno = terreno

    def __str__(self) -> str:
        return "tennis" + "\t" + self.campo + "\t" + str(self.codice) + "\t" + str(self.larghezza) + "\t" + str(self.lunghezza) + "\t" + str(self.temperatura) + "\t" + self.terreno + "\t" + str(self.costoOrario)