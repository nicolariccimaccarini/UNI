# Creazione del dizionario
original_dict = {
    'Boss Nass': 'Star Wars',
    'Tom Bombadil': 'The Lord of the Rings',
    'Hari Seldon': 'Foundation series',
    'Polliver': 'Game of Thrones',
    'Jules Winnfield': 'Pulp Fiction',
    'The Mule': 'Foundation series',
    'Flynn Rider': 'Rapunzel',
    'Yoda': 'Star Wars',
    'Vince Vega': 'Pulp Fiction'
}

# Stampa del dizionario originale
print("Dizionario originale:")
for key, value in original_dict.items():
    print(f"'{key}': '{value}'")

# Inversione del dizionario
inverted_dict = {}
for key, vlaue in original_dict.items():
    if value not in inverted_dict:
        inverted_dict[value] = [key]
    else:
        inverted_dict[value].append(key)

# Stampa del dizionario invertito
print("\nDizionario invertito:")
for key, value in inverted_dict.items():
    print(f"'{key}': '{value}'")