from Cliente import Cliente

class Azienda(Cliente):

    ragione_sociale: str
    fatturato: int

    def __init__(self, codice, indirizzo, giorno, mese, anno, ragione_sociale, fatturato):
        super().__init__(codice, indirizzo, giorno, mese, anno)
        self.ragione_sociale = ragione_sociale
        self.fatturato = fatturato