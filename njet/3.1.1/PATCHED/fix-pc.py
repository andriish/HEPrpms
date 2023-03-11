#!/usr/bin/python3

import argparse
import os.path


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Generate njet3.pc.in")

    parser.add_argument(
        "filename",
        type=str,
        help="path to njet3.pc",
    )

    parser.add_argument(
        "--flags",
        type=str,
        help="space separated list of CPPFLAGS",
    )

    parser.add_argument(
        "--deps",
        type=str,
        help="space separated list of library dependencies",
    )

    parser.add_argument(
        "--libs",
        type=str,
        help="space separated list of libraries to link",
    )

    parser.add_argument(
        "--njetan",
        type=str,
        help="space separated list of analytic library processes",
    )

    args = parser.parse_args()

    flags = args.flags.strip() if args.flags else ""

    deps = args.deps.strip().replace(" ", ", ") if args.deps else ""

    libs = (
        " ".join(["-l" + a for a in args.libs.strip().split(" ") if a != ""])
        if args.libs
        else ""
    )

    njetan = (
        " ".join(["-lnjet3an" + a for a in args.njetan.strip().split(" ") if a != ""])
        if args.njetan
        else ""
    )

    filename = args.filename.strip()

    if not os.path.isfile(filename):
        print("{} not found!".format(filename))
        exit()

    with open(filename, "r") as f:
        content = f.read()

    content = content.replace("RRRRR", deps)
    content = content.replace("LLLLL", libs)
    content = content.replace("AAAAA", njetan)
    content = content.replace("FFFFF", flags)

    with open(filename, "w") as f:
        f.write(content)
