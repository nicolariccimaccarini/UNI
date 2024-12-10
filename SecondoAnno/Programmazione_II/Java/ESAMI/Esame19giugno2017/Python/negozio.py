class Negozio:
    
    codice: int
    superficie: int
    indirizzo: str
    tipo: str 
    
    def __init__(self, tipo: str, codice: int, indirizzo: str, superficie: int) -> None:
        self.tipo = tipo
        self.codice = codice
        self.indirizzo = indirizzo
        self.superficie = superficie

    def __str__(self) -> str:
        return str(self.codice) + "\t" + self.indirizzo + "\t" + str(self.superficie)