class Key:
    def __init__(self, value):
        self.value = value

    def __hash__(self):
        return self.value

    def __eq__(self, other):
        if self.value < other.value + 1:
            if other.value < self.value + 1:
                return True
        return False

a = Key(7)
b = Key(7)
values = {a: 1}
print(values[b])
