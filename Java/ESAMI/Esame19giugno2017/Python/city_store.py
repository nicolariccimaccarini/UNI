from negozio import Negozio

class CityStore:

    responsabile: str
    codice_fiscale: str

    def __init__(self, tipo: str, codice: int, indirizzo: str, superficie: int, responsabile: str, codice_fiscale: str) -> None:
        super().__init__(tipo, codice, indirizzo, superficie)
        self.responsabile = responsabile
        self.codice_fiscale = codice_fiscale

    def __str__(self) -> str:
        return "City-Store" + "\t" + super().__str__() + "\t" + self.responsabile + "\t" + self.codice_fiscale + "\t-\t-\t-"