def f(x, y=10):
    sum = x + y
    return sum

def g(x):
    return x**3

def main():
    a = f(2)
    print("f(2) = ", a)
    b = f(g(2))
    print("f(g(2)) = ", b)

if __name__ == "__main__":
    main() 