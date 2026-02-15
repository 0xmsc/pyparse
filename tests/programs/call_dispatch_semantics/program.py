class C:
    pass

c = C()

def hacked():
    return 123

c.__call__ = hacked
print(c.__call__())
print(callable(c))
