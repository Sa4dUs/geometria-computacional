# ============================================================
# File: 9-invented-triangulation-method.py
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-28
# Last modified: 2026-04-28
# License: MIT
# ============================================================

import math
import matplotlib.pyplot as plt

def orient(a, b, c):
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])

def intersect(a, b, c, d):
    o1 = orient(a, b, c)
    o2 = orient(a, b, d)
    o3 = orient(c, d, a)
    o4 = orient(c, d, b)
    return (o1 * o2 < 0) and (o3 * o4 < 0)

def distance(a, b):
    return math.hypot(a[0] - b[0], a[1] - b[1])

def valid_diagonal(poly, i, j):
    n = len(poly)
    a, b = poly[i], poly[j]
    if (i + 1) % n == j or (j + 1) % n == i:
        return False
    for k in range(n):
        c = poly[k]
        d = poly[(k + 1) % n]
        if k in [i, j] or (k + 1) % n in [i, j]:
            continue
        if intersect(a, b, c, d):
            return False
    return True

def choose_best_diagonal(poly, i):
    n = len(poly)
    candidates = []
    for j in range(2, n - 1):
        if valid_diagonal(poly, i, j):
            d = distance(poly[i], poly[j])
            size1 = (j - i) % n
            size2 = n - size1
            balance = abs(size1 - size2)
            score = d + 0.5 * balance
            candidates.append((score, j))
    if not candidates:
        return None
    candidates.sort()
    return candidates[0][1]

def divide_polygon(poly, i, j):
    n = len(poly)
    p1 = []
    k = i
    while True:
        p1.append(poly[k])
        if k == j:
            break
        k = (k + 1) % n
    p2 = []
    k = j
    while True:
        p2.append(poly[k])
        if k == i:
            break
        k = (k + 1) % n
    return p1, p2

def triangulation(poly):
    if len(poly) == 3:
        return [poly]
    i = 0
    j = choose_best_diagonal(poly, i)
    if j is None:
        raise ValueError("No hay diagonales válidas")
    p1, p2 = divide_polygon(poly, i, j)
    return triangulation(p1) + triangulation(p2)

def draw_before_after(polygon, triangles):
    fig, ax = plt.subplots(1, 2, figsize=(10, 5))

    x = [p[0] for p in polygon] + [polygon[0][0]]
    y = [p[1] for p in polygon] + [polygon[0][1]]

    ax[0].plot(x, y)
    ax[0].set_title("Polygon without triangulation")
    ax[0].set_aspect('equal')

    ax[1].plot(x, y)
    for tri in triangles:
        tx = [p[0] for p in tri] + [tri[0][0]]
        ty = [p[1] for p in tri] + [tri[0][1]]
        ax[1].plot(tx, ty)
    ax[1].set_title("Polygon triangulated")
    ax[1].set_aspect('equal')

    plt.show()

if __name__ == "__main__":
    polygon = [
        (0, 0),
        (3, 0),
        (5, 2),
        (4, 4),
        (2, 5),
        (0, 3),
    ]

    triangles = triangulation(polygon)

    draw_before_after(polygon, triangles)
    triangles = triangulation(polygon)

    print("Triángulos:")
    for t in triangles:
        print(t)

    draw(polygon, triangles)