inp = input()

print("[|" + str(ord(inp[0])), end="")
for i in range(1, len(inp)):
    print(";", str(ord(inp[i])), end="")
print("|]")
