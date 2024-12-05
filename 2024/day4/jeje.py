import re
with open("input") as f:
    src=f.read()
    print(sum([int(x)*int(y) for x,y in ( r.split(",") for r in re.findall("mul\((\d{1,3},\d{1,3})\)",src))]))
    print(sum(map(lambda a: eval(a, {'mul':lambda x,y: x*y }), re.findall(r"mul\(\d*,\d*\)", src))))
