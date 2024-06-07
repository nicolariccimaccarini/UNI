from negozio import Negozio

class SuperStore:

    ragione_sociale: str
    partita_iva: int
    n_casse:int 

    def __init__(self, tipo: str, codice: int, indirizzo: str, superficie: int, ragione_sociale: str, partita_iva: int, n_casse: int) -> None:
        super.__init__(tipo, codice, indirizzo, superficie)
        self.ragione_sociale = ragione_sociale
        self.partita_iva = partita_iva
        self.n_casse = n_casse

    def __str__(self) -> str:
        return "Super-Store" + "\t" + super().__str__()+ "\t-\t-\t" + self.ragione_sociale + "\t" + str(self.partita_iva) + "\t" + self.n_casse