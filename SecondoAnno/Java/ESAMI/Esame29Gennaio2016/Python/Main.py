import sys
import argparse

from Cliente import Cliente
from Privato import Privato
from Azienda import Azienda

def main():

    clienti: list[Cliente] = []
    sommaPremi = {}

    try: 
        f = open("clienti.txt", "r", "UTF-8")
        line = f.readline().strip()
        while (line != ''):
            tok = line.split()
            tipo = tok[0]
            codice = int(tok[1])
            indirizzo = f.readline().strip()
            line = f.readline().strip()
            tok = line.split()
            giorno = int(tok[0])
            mese = int(tok[1])
            anno = int(tok[2])

            nome_ragione = f.readline().strip()

            if (tipo == "privato"):
                privato = Privato(codice, indirizzo, giorno, mese, anno, nome_ragione)
                clienti.append(privato)
            else:
                fatturato = int(f.readline().strip())
                azienda = Azienda(codice, nome_ragione, indirizzo, giorno, mese, anno)
                clienti.append(azienda)

            line = f.readline().strip()

            while (line != ''):
                premio = int(line)
                somma_premi += premio
                line = f.readline().strip()

            sommaPremi[nome_ragione] = somma_premi
            line = f.readline().strip()

            if premio_max < somma_premi:
                premio_max = somma_premi
                nome = nome_ragione
            
            f.close()

    except IOError as e:
        print(e)
    except Exception as e:
        print(e)

# stampo i clienti
    print("Tipo, Codice, Nome, Ragione Sociale, Indirizzo, Data, Fatturato")
    for Cliente in clienti:
        print(Cliente)

    print("Somma per cliente")
    print(sommaPremi)
    print("Nome o ragione sociale: " + nome_ragione)


if __name__ == "__main__":
    main()