class Socio:

    def __init__(self, codice, nome, eta, categoria) -> None:
        self.codice = codice
        self.nome = nome
        self.eta = eta
        self.categoria = categoria
        self.prenotazioni = []

    def addPrenotazione(self, pren):
        self.prenotazioni.append(pren)

    def __str__(self) -> str:
        return str(self.codice) + "\t" + self.nome + "\t" + str(self.eta) + "\t" + str(self.categoria) + "\t" + str([str(pren for pren in self.prenotazioni)])