from negozio import Negozio
from city_store import CityStore
from super_store import SuperStore

def main():
    negozi = []
    scontrini = []

    cod_scontrino = {}

    ## leggo il primo file (clienti.txt)
    try:
        file = open("negozi.txt", "r")
        line = file.readline().strip()

        while (line != ""):
            splitted_line = line.split()
            tipo = splitted_line[0]
            codice = int(splitted_line[1])
            indirizzo = splitted_line[2]

            line = file.readline().strip()
            if (tipo == "city-store"):
                responsabile = line
                codice_fiscale = file.readline().strip()
                superficie = int(file.readline().strip())

                n = CityStore(tipo, codice, indirizzo, superficie, responsabile, codice_fiscale)
                negozi.append(n)
            elif (tipo == "super-store"):
                ragione_sociale = line
                line = file.readline().strip()
                splitted_line = line.split()
                partita_iva = int(splitted_line[0])
                n_casse = int(splitted_line[1])
                superficie = int(splitted_line[2])

                n = SuperStore(tipo, codice, indirizzo, superficie, ragione_sociale, partita_iva, n_casse)
                negozi.append(n)
            
            line = file.readline().strip()
        
        file.close()
    
    except IOError as io_exception:
        print(io_exception)
    except Exception as e:
        print(e)

    

    ## stampo i negozi
    print("\nTipo, ID, Indirizzo, Superficie, Nome e Cognome Responsabile, Codice Fiscale, Ragione Sociale, Partita Iva, Casse")
    for negozio in negozi:
        print(negozio)

if __name__ == "__main__":
    main()