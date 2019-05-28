import os

table = {
    "birds": [],
    "medical": [],
    "intsum": [],
}

def importance(key):
    if key.startswith("instrumental"):
        return 1
    elif key.startswith("relevant"):
        return 2
    elif key.startswith("num"):
        return 3
    elif key.startswith("noise"):
        return 4
    else:
        return 0

def cell_colour(importance):
    return [
        "\\cellcolor{lightpurple}",
        "\\cellcolor{lightblue}",
        "\\cellcolor{lightgreen}",
        "\\cellcolor{lightyellow}",
        "\\cellcolor{lightred}"
    ][importance]

def tidy(values):
    values = [v.strip() for v in values.split(",")]
    math = {"\\Pr", "\\cap", "\\cup", "\\Omega",
            "\\div", "\\sum", "\\setminus", "\\emptyset"}
    needs_arg = {"\\bar", "\\sqrt"}
    def tidier(v):
        if v == "^":
            return "$\\exp(\\cdot)$"
        elif len(v) == 1:
            return f"${v}$"
        elif any(m in v for m in math):
            return f"${v}$"
        elif v in needs_arg:
            return f"${v}" + "{\phantom{X}}$"
        else:
            return v
    return ', '.join(tidier(v) for v in values)

def read_file(filename):
    with open(filename) as f:
        try:
            (name, rs) = [t.strip() for t in f.readline().split(',', maxsplit=1)]
        except Exception as e:
            print(filename, f.readline())
            raise e
        kvs = []
        for line in f.readlines():
            (k, v) = [t.strip().strip('"') for t in line.split(',', maxsplit=1)]
            kvs.append((k.replace("_", " "),
                        v.replace("$", "\\$").replace("_", "\\_"),
                        importance(k)))
        return ((name, rs), kvs)

def create_tex_table(tabledata):
    (name, rs), properties = tabledata
    ans = []
    ans.append("\\begin{tabular}{l p{10cm}}")
    ans.append("\\toprule")
    ans.append("Kind & Value \\\\")
    ans.append("\\midrule")
    for (k, v, i) in sorted(properties, key=lambda p: (p[2], p[0], p[1])):
        colour = cell_colour(i)
        v = tidy(v)
        ans.append(f"{colour}{k} & {v} \\\\")
    ans.append("\\bottomrule")
    ans.append("\\end{tabular}")
    return "\n".join(ans)


def csv_to_tex(infilename, outfilename):
    output = create_tex_table(read_file(infilename))
    with open(outfilename, "w") as f:
        f.write(output)

def split_name(filename):
    tokens = filename[:-4].split("_") + ["ENDOFNAME"]
    groups = []
    group = []
    for token in tokens:
        if token in {"from", "to", "ENDOFNAME"}:
            groups.append("_".join(group))
            group = []
        else:
            group.append(token)
    return groups


def allPairs(labels):
    for i in labels:
        js = []
        for j in labels:
            js.append(j)
        yield (i, js)

if not os.path.exists("pseudo_tables/tex"):
    os.makedirs("pseudo_tables/tex")

for filename in os.listdir("pseudo_tables/csv"):
    if filename.endswith(".csv"):
        newfile = filename.replace("csv", "tex")
        (problem, fromRS, toRS) = split_name(filename)
        table[problem].append((fromRS, toRS))
        csv_to_tex(f"pseudo_tables/csv/{filename}", f"pseudo_tables/tex/{newfile}")

for filename in os.listdir("tables"):
    if filename.endswith(".csv") and filename.startswith("Q_table"):
        (problem, rs) = filename.split(".")[0].split("_")[2:]
        if rs == "algebra": rs = "arithmetic_algebra"
        table[problem].append((rs, rs))
        newfile = f"{problem}_from_{rs}_to_{rs}.tex"
        csv_to_tex(f"tables/{filename}", f"pseudo_tables/tex/{newfile}")


for key in table:
    table[key] = sorted(table[key])

ans = []
ans.append("\\documentclass{article}")
ans.append("")
ans.append("\\usepackage[a0paper, landscape, margin=1in]{geometry}")
ans.append("")
ans.append("\\usepackage[usenames]{color} %used for font color")
ans.append("\\usepackage{amssymb} %maths")
ans.append("\\usepackage{amsmath} %maths")
ans.append("\\usepackage[utf8]{inputenc}")
ans.append("")
ans.append("\\usepackage{booktabs}")
ans.append("\\usepackage{colortbl}")
ans.append("\\usepackage{xcolor}")
ans.append("\\definecolor{lightpurple}{cmyk}{0.225,0.25,0,0.1}")
ans.append("\\definecolor{lightblue}{cmyk}{0.3,0,0,0}")
ans.append("\\definecolor{lightgreen}{cmyk}{0.35,0,0.4,0.15}")
ans.append("\\definecolor{lightyellow}{cmyk}{0,0.05,0.4,0}")
ans.append("\\definecolor{lightred}{cmyk}{0,0.5,0.4,0.15}")
ans.append("\\definecolor{lightgray}{RGB}{220, 220, 220}")
ans.append("")
ans.append("\\begin{document}")

for problem, replist in table.items():
    fromRSs = set(k for k, v in replist)
    toRSs = set(v for k, v in replist)
    reps = set(fromRSs | toRSs)
    width = len(reps)
    ans.append("\\begin{tabular}{c " + " ".join(["c"] * width) + "}")
    ans.append("& " + " & ".join(r.replace("_", "\\_") for r in reps))
    for (source, targets) in allPairs(reps):
        if source in fromRSs:
            ans.append("\\\\")
            ans.append(source.replace("_", "\\_"))
            for target in targets:
                if target in toRSs:
                    ans.append("& \\input{tex/" + f"{problem}_from_{source}_to_{target}.tex" + "}")
    ans.append("\\end{tabular}")
    ans.append("\\clearpage\\relax")

ans.append("\\end{document}")

output = "\n".join(ans)
with open("pseudo_tables/bigtable.tex", "w") as f:
    f.write(output)
