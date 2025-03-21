from dipendente import Dipendente

class Trainer(Dipendente):
    ore_settimanali: int
    specialita: str 

    def __init__(self, codice: int, tipo: str, nome_dipendente: str, costo_orario: float, ore_settimanali: int, specialita: str) -> None:
        super().__init__(codice, tipo, nome_dipendente, costo_orario)
        self.ore_settimanali = ore_settimanali
        self.specialita = specialita

    def __str__(self) -> str:
        return super().__str__() + ", " + str(self.ore_settimanali) + ", " + self.specialita + ", -, -, -, " + str(self.costo_orario)