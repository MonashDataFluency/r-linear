# Extract code blocks from an .Rmd file

import sys, textwrap

next(sys.stdin)
while not next(sys.stdin).startswith("---"): pass

in_code = False
in_challenge = False
need_blank = False
last_comment = False
counts = [ ]
for line in sys.stdin:
    line = line.rstrip()

    if line.startswith("knitr::"): continue

    if line.startswith("```"):
        #print()
        in_code = not in_code
        assert in_code or line == "```", line
        need_blank = True
    elif in_code:
        if need_blank:
            print()
            need_blank = False
        print(line)
        last_comment = False
        need_blank = False
    elif line.startswith("#"):
        print("#" if in_challenge else "")
        in_challenge = "{.challenge}" in line
        if in_challenge:
            line = line.replace("{.challenge}","").rstrip()
        n = line.count("#")
        #banner = "#"*n + " " + ("-" if n > 1 else "=") * (len(line)-n-1)
        #print(banner)

        while n < len(counts): del counts[-1]
        while n > len(counts): counts.append(0)
        counts[-1] += 1

        banner = "# "+".".join(str(i) for i in counts) + line[n:] + " ----"
        if n == 1:
            print()
            print()
            print("#"+"/"*(len(banner)-1))
        print(banner)
        last_comment = True
        need_blank = True
    elif not line.strip():
        need_blank = True
    elif line.strip():
        if need_blank:
            print("#" if last_comment else "")
            need_blank = False
        for line2 in textwrap.wrap(line.replace("`","")) or [""]:
            print("# " + line2)
        last_comment = True


