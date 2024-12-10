class Servizio:
    numero_ore: float

    def __init__(self, impiegato, numero_ore: float) -> None:
        self.impiegato = impiegato
        self.numero_ore = numero_ore

    def get_costo(self) -> float:
        costo = self.impiegato.costo_orario * self.numero_ore
        return costo
    
    def __str__(self) -> str:
        return self.impiegato.nome_dipendente + ", " + str(self.numero_ore) + ", " + str(self.get_costo())