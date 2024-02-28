n1 = 1
n2 = 2
s1 = 'hi'
s2 = 'hello'

# verifica se il tipo di n1 ed n2 e' lo stesso
if type(n1) == type(n2):
    if n1 > n2:
        print(str(n1) + " e piu grande di " + str(n2))
    elif n1 < n2:
        print(str(n2) + " e piu piccolo di " + str(n2))
    else:
        print(str(n1) + " e uguale a " + str(n2))
else:
    print("I tipi di n1 e n2 sono diversi")

if type(s1) == type(n2):
    if s2.find(s1) != -1:
        print(s1 + " è una sottostringa di " + s2)
    elif s1.findO(s2) != -1:
        print(s2 +" e' una sottostringa di " + s1)
    else:
        print(s1 + " e " + s2 + "sono tra loro differenti")
else:
    print("I tipi di s1 ed s2 sono diversi")
