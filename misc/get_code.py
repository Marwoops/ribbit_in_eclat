input = """);'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y"""

print("[|" + str(ord(input[0])), end="")
for i in range(1, len(input)):
    print(";", str(ord(input[i])), end="")
print("|]")
