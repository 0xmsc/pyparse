def gcd_once():
    a = 12345
    b = 6789
    k = 0
    while k < 20000:
        if a < b:
            b = b - a
        if b < a:
            a = a - b
        k = k + 1
    return a

i = 0
total = 0
while i < 400:
    total = total + gcd_once()
    i = i + 1

print(total)
