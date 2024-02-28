class Dipendente:
    codice: int 
    nome: str
    costo_orario: float
    servizi: list

    def __init__(self, codice: int, nome: str, costo_orario: float, servizi: list) -> None:
        self.codice = codice
        self.nome = nome
        self.costo_orario = costo_orario

        if servizi is None:
            self.servizi = []
        else:
            self.servizi = servizi

    def add_servizio(self, servizio):
        self.servizi.append(servizio)

    def __str__(self) -> str:
        return self.nome + " , " + str(self.codice) + " , "