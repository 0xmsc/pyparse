def f(n):
    try:
        if n < 1:
            raise Exception("neg")
        return n + 1
    except:
        print("caught")
        return 7
    finally:
        print("finally")

print(f(1))
print(f(0))

try:
    raise Exception("boom")
except:
    print("outer-caught")
finally:
    print("outer-finally")
