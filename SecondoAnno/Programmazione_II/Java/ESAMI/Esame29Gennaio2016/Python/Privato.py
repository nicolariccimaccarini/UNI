from Cliente import Cliente

class Privato(Cliente):

    nome: str

    def __init__(self, codice, indirizzo, giorno, mese, anno, nome):
        super().__init__(codice, indirizzo, giorno, mese, anno)
        self.nome = nome