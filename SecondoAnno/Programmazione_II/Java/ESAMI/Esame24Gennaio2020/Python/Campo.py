class Campo:
    
    def __init__(self, codice, campo, larghezza, lunghezza, costoOrario):
        super.codice = codice
        super.campo = campo
        self.larghezza = larghezza
        self.lunghezza = lunghezza
        self.costoOrario = costoOrario
        self.prenotazioni = 0

    def addPrenotazione(self):
        self.prenotazioni = self.prenotazioni + 1

    def incasso(self) -> float:
        return self.prenotazioni * self.costoOrario
    
    def __str__(self) -> str:
        return self.campo + "\t" + str(self.codice) + "\t" + str(self.larghezza) + "\t" + str(self.lunghezza) + "\t" + str(self.costoOrario)