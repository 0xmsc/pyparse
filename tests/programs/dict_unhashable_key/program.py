class BadKey:
    def __eq__(self, other):
        return True

key = BadKey()
values = {}
values[key] = 1
