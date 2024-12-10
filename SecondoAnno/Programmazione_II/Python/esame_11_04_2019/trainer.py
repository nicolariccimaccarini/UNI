from dipendente import Dipendente

class Trainer(Dipendente):
    ore_settimanali: int
    specialita: str

    def __init__(self, codice: int, nome: str, costo_orario: float, ore_settimanali: int, specialita: str) -> None:
        super().__init__(codice, nome, costo_orario)
        self.ore_settimanali = ore_settimanali
        self.specialita = specialita

    def __str__(self) -> str:
        return super().__str__() + "trainer, " + str(self.ore_settimanali) + " , " + self.specialita + " , - , - , -, " + str(self.costo_orario)