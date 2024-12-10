from servizio import Servizio

class Visitatore:
    codice: int
    nome: str 
    servizi: list[Servizio]

    def __init__(self, codice: int, nome: str, servizi = None) -> None:
        self.codice = codice
        self.nome = nome

        if servizi is None:
            self.servizi = []
        else:
            self.servizi = servizi

    def add_servizio(self, servizio: Servizio):
        self.servizi.append(servizio)

    def __str__(self) -> str:
        return str(self.codice) + ", " + self.nome + ", " + str(list(map(str, self.servizi)))