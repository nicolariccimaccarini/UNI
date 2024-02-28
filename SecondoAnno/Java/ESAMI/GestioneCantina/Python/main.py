from impiegato import Impiegato
from guida import Guida
from sommelier import Sommelier
from visitatore import Visitatore
from servizio import Servizio

def main():
    impiegati: list[]
    visitatori: list[]

    map_impiegati = {}

    # leggo gli impiegati
    try:
        file = open("impiegati.txt", "r")
        line = file.readline().strip()

        while (line != ""):
            splitted_line = line.split()
            codice = int(splitted_line[0])
            tipo = splitted_line[1]
            nome = file.readline().strip()

            line = file.readline().strip()
            if (tipo == "guida"):
                splitted_line = line.split()
                telefono = int(splitted_line[0])
                conoscenza_inglese =splitted_line[1] == 'true'
                appuntamenti_settimanali = int(splitted_line[2])
                costo_orario = float(splitted_line[3])

                impiegato = Guida(codice, tipo, nome, costo_orario, telefono, conoscenza_inglese, appuntamenti_settimanali)
                impiegati.append(impiegato)
                map_impiegati[codice] = impiegato

            elif (tipo == "sommelier"):
                splitted_line = line.split()
                ore_settimanali = int(splitted_line[0])
                costo_orario = float(splitted_line[1])
                specialita = file.readline().strip()

                impiegato = Sommelier(codice, tipo, nome, costo_orario, ore_settimanali, specialita)
                impiegati.append(impiegato)
                map_impiegati[codice] = impiegato

            line = file.readline().strip()
        file.close()
    except IOError as io_exception:
        print(io_exception)
    except Exception as e:
        print(e)

    
    # leggo i visitatori
    try: 

        file = open("visitatori.txt", "r")
        line = file.readline().strip()

        while (line != ""):
            codice_visitatore = int(line)
            nome = file.readline().strip()

            visitatore = Visitatore(codice_visitatore, nome)
            visitatori.append(visitatore)

            line = file.readline().strip()
            while (line != "" and line != "\n"):
                splitted_line = line.split()
                codice_impiegato = int(splitted_line[0])
                numero_ore = float(splitted_line[1])

                impiegato = map_impiegati[codice_impiegato]

                servizio = Servizio(impiegato, numero_ore)
                visitatore.add_servizio(servizio)
                
                impiegato.add_servizio(servizio)

                line = file.readline().strip()
            file.readline().strip()
        file.close()

    except IOError as io_exception:
        print(io_exception)
    except Exception as e:
        print(e)


    # stampo gli impiegati
    print("nome, codice, tipo, ore settimanali, specialità, telefono, inglese, app.sett., costo orario")
    for impiegato in impiegati:
        print(impiegato)

    print("\n\n")

    # stampo i visitatori
    for visitatore in visitatori:
        print(visitatore)

    print("\n\n")

    # PUNTO 5
    try:
        codice = int(input("Inserire il codice di un impiegato: "))

        impiegato = map_impiegati[codice]

        print("Numero di servizi svolti:", impiegato.numero_servizi())
    except IOError as io_exception:
        print(io_exception)
    except KeyError as _:
        print("Codice non trovato.")

if __name__ == "__main__":
    main()