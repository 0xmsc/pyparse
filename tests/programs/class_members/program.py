class Counter:
    def __init__(self, start):
        self.value = start

    def inc(self):
        self.value = self.value + 1
        return self.value

c = Counter(4)
print(c.value)
print(c.inc())
c.value = c.value + 3
print(c.value)
