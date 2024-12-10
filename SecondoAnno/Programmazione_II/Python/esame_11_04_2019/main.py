from dipendente import Dipendente
from nutrizionista import Nutrizionista
from trainer import Trainer
from servizio import Servizio
from cliente import Cliente

def main():
    dipendenti: list[Dipendente] = []
    clienti: list[Cliente] = []
    map_dipendenti = []

    # Punto 1
    try:
        file = open("dipendenti.txt" , "r", encoding="utf-8")
        line = file.readline().strip()

        while line != "":
            tok = line.split()
            codice = int(tok[0])
            tipo = tok[1]
            nome = file.readline().strip()
            line = file.readline().strip()

            if tipo == "trainer":
                tok = line.split()
                ore_settimanali = int(tok[0])
                costo_orario = float(tok[1])
                specialita = file.readline().strip()
                dipendente = Trainer(codice, nome, costo_orario, ore_settimanali, specialita)
                dipendenti.append(dipendente)
                map_dipendenti[codice] = dipendente
            elif tipo == "nutrizionista":
                tok=line.split()
                telefono = int(tok[0])
                medico = tok[1]=="true"
                appuntamenti_settimanali = int(tok[2])
                costo_orario = float(tok[3])
                dipendente = Nutrizionista(codice, costo_orario, telefono, medico, appuntamenti_settimanali)
                dipendenti.append(dipendente)
                map_dipendenti[codice] = dipendente

            line = file.readline().strip()
        file.close()

    except Exception as e:
        print(e)

    # Punto 2
    try:
        file = open("clienti.txt", "r", encoding="utf-8")
        line = file.readline().strip()
        
        while line != "":
            codice = int(line)
            nome = file.readline().strip()
            cliente = Cliente(codice, nome)
            clienti.append(cliente)
            line = file.readline().strip()

            while line != "" and line != "\n":
                tok = line.split()
                codice_dipendente = int(tok[0])
                numero_ore = float(tok[1])
                dipendente = map_dipendenti[codice_dipendente]
                servizio = Servizio(dipendente, numero_ore)
                cliente.add_servizio(servizio)
                dipendente.add_servizio(servizio)
                line = file.readline()

            line = file.readline().strip()
        file.close()

    except Exception as e:
        print(e)

    # Punto 3
    print("nome, codice, tipo, ore_sett, spec, telefono, medico, app_sett, costo_orario")
    for d in dipendenti:
        print(d)
    print("\n\n")

    # Punto 4
    for c in clienti:
        print(d)
    print("\n\n")

    # Punto 5
    max = 0
    nmax = None
    for d in dipendenti:
        if len(d.servizi) > max:
            max = len(d.servizi)
            nmax = d

        if max is None:
            print("Errore")
        else:
            print("Dipendente con maggior numero di servizi: ", nmax.nome)

if __name__ == "__main__":
    main()