from dipendente import Dipendente

class Nutrizionista(Dipendente):
    telefono: int
    medico: bool 
    appuntamenti_settimanali: int 

    def __init__(self, codice: int, nome: str, costo_orario: float, telefono: int, medico: bool, appuntamenti_settimanali: int) -> None:
        super().__init__(codice, nome, costo_orario)
        self.telefono = telefono
        self.medico = medico
        self.appuntamenti_settimanali = appuntamenti_settimanali

    def __str__(self) -> str:
        return super().__str__() + " nutri , - , - , " + str(self.telefono) + " , " + str(self.medico) + " , " + str(self.appuntamenti_settimanali) + " , " + str(self.costo_orario)