import pickle

class ToSave:
    def __init__(self, stringa, intero, decimale, booleano):
        self.stringa = stringa
        self.intero = intero
        self.decimale = decimale
        self.booleano = booleano
    
    def __str__(self):
        return f"ToSave: {self.stringa}, {self.intero}, {self.decimale}, {self.booleano}"
    
# Funzione per scrivere un'istanza di ToSave su un file utilizzando pickle
def scrivi_su_file(tosave, filename):
    with open(filename, 'wb') as file:
        pickle.dump(tosave, file)

# Funzione per leggere un'istanza di ToSace su un file utilizzando pickle
def leggi_da_file(filename):
    with open(filename, 'rb') as file:
        tosave = pickle.load(file)
    return tosave

def main():
    # Input da tastiera
    input_stringa = input("Inserisci una stringa: ")
    input_intero = int(input("Inserisci un numero intero: "))
    input_decimale = float(input("Inserisci un numero decimale: "))
    input_booleano = bool(input("Inserisci True o False: "))

    tosave_instance = ToSave(input_stringa, input_intero, input_decimale, input_booleano)
    scrivi_su_file(tosave_instance, 'file_to_save.pkl')
    tsp = leggi_da_file('file_to_save.pkl')
    print(tsp)

if __name__ == "__main__":
    main()
