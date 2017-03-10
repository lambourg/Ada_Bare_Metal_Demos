#!/usr/bin/env python

try:
    import png
except:
    raise Exception ('pypng is missing.')
import os


def gen_ada(filename, pkg, dest):
    content = []

    argb_mode = False

    with open(filename, "rb") as finput:
        r = png.Reader(file=finput)
        (width, height, pixels, properties) = r.asRGBA()
        for line in pixels:
            cols = []
            for j in range(width):
                a = line[4 * j + 3]
                if a < 128:
                    argb_mode = True
                    cols.append([0, 0, 0, 0])
                else:
                    r = line[4 * j]
                    g = line[4 * j + 1]
                    b = line[4 * j + 2]
                    cols.append([255, r, g, b])
            content.append (cols)

    if argb_mode:
        print "ARGB_1555 mode"
    else:
        print "RGB_565 mode"

    with open(dest, 'w') as fout:
        n_line = 1
        fout.write("package %s is\n\n" % pkg)
        fout.write("   Bmp : aliased constant Texture :=\n")
        for y in range(height):
            cols = []
            for x in range(width):
                values = content[x][y]
                if argb_mode:
                    a = values[0] >> 7
                    r = values[1] >> 3
                    g = values[2] >> 3
                    b = values[3] >> 3
                    val = (a << 15) | (r << 10) | (g << 5) | b
                else:
                    r = values[1] >> 3
                    g = values[2] >> 2
                    b = values[3] >> 3
                    val = (r << 11) | (g << 5) | b
                cols.append('16#%x#' % val)
            if n_line == 1:
                fout.write("      (")
            else:
                fout.write("       ")
            fout.write("(%s)" % ", ".join(cols))
            if n_line == height:
                fout.write(");\n")
            else:
                fout.write(",\n")
            n_line += 1
        fout.write("\nend %s;\n" % pkg)


def is_png(pics, fname):
    base, ext = os.path.splitext(fname)
    return os.path.isfile(os.path.join(pics, fname)) and ext == '.png'


pics = os.path.abspath("./pics")
files = [f for f in os.listdir(pics) if is_png(pics, f)]
for f in files:
    pkg, _ = os.path.splitext(os.path.basename(f))
    src = os.path.join('..', 'pics', 'textures-%s.ads' % pkg)
    pkg = "Textures.%s" % pkg.title()
    print src
    gen_ada(os.path.join(pics, f), pkg, src)
