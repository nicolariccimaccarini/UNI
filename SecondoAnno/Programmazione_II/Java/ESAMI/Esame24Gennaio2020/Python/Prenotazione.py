class Prenotazione:

    def __init__(self, codiceCampo, ora) -> None:
        self.codiceCampo = codiceCampo
        self.ora = ora

    def __str__(self) -> str:
        return "(" + str(self.codiceCampo) + "," + str(self.ora) + ")"