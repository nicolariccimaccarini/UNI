from dipendente import Dipendente

class Nutrizionista(Dipendente):
    telefono: int
    medico: bool
    appuntamenti_settimanali: int 

    def __init__(self, codice: int, tipo: str, nome_dipendente: str, costo_orario: float, telefono: int, medico: bool, appuntamenti_settimanali: int) -> None:
        super().__init__(codice, tipo, nome_dipendente, costo_orario)
        self.telefono = telefono
        self.medico = medico
        self.appuntamenti_settimanali = appuntamenti_settimanali

    def __str__(self) -> str:
        return super().__str__() + ", -, -, " + str(self.telefono) + ", " + str(self.medico) + ", " + str(self.appuntamenti_settimanali) + ", " + str(self.costo_orario)