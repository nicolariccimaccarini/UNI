from servizio import Servizio

class Impiegato: 
    codice: int
    tipo: str
    nome_dipendente: str
    costo_orario: float

    servizi: list[Servizio]

    def __init__(self, codice: int, tipo: str, nome_dipendente: str, costo_orario: float, servizi: list[Servizio] = None) -> None:
        self.codice = codice
        self.tipo = tipo
        self.nome_dipendente = nome_dipendente
        self.costo_orario = costo_orario

        if servizi is None:
            self.servizi = []
        else:
            self.servizi = servizi

    def add_servizio(self, servizio: Servizio):
        self.servizi.append(servizio)

    def __str__(self) -> str:
        return self.nome_dipendente + ", " + str(self.codice) + ", " + self.tipo