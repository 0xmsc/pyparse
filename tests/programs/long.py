def sum_to():
    i = 0
    total = 0
    while i < 1000001:
        total = total + i
        i = i + 1
    return total
print(sum_to())
