class Counter:
    def __init__(self):
        self.n = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.n < 3:
            value = self.n
            self.n = self.n + 1
            return value
        raise StopIteration()

for x in Counter():
    print(x)
print("done")
