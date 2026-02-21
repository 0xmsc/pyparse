values = {1: 10, 2: 20}
print(values[1])
values[2] = 70
print(values[2])
print(len(values))

if values:
    print(1)

empty = {}
if empty:
    print("unexpected")
else:
    print(0)
