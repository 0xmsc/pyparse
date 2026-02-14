values = [1, 2]
print(values)
print(values[0])
values[1] = 7
print(values)
print(len(values))
if values:
    print(1)

empty = []
if empty:
    print("unexpected")
else:
    print(0)
