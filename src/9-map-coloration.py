
# ============================================================
# File: 9-map-coloration.py
#
# Authors:
#   - Marcelo Domínguez (@sa4dus)
#   - Alejandro García Prada (@AlexGarciaPrada)
#
# Created: 2026-04-28
# Last modified: 2026-04-28
# License: MIT
# ============================================================


import matplotlib.pyplot as plt
import networkx as nx


mapa = {
    "A": ["B", "C", "D"],
    "B": ["A", "C", "E", "F"],
    "C": ["A", "B", "D", "F", "G"],
    "D": ["A", "C", "G"],
    "E": ["B", "F", "H"],
    "F": ["B", "C", "E", "G", "H", "I"],
    "G": ["C", "D", "F", "I"],
    "H": ["E", "F", "I", "J"],
    "I": ["F", "G", "H", "J"],
    "J": ["H", "I"]
}

def welsh_powell_coloring(graph):
    degrees = {v: len(neighbors) for v, neighbors in graph.items()}

    ordered_nodes = sorted(graph.keys(), key=lambda x: degrees[x], reverse=True)

    colors = {}

    for node in ordered_nodes:
        used_colors = set()

        for neighbor in graph[node]:
            if neighbor in colors:
                used_colors.add(colors[neighbor])

        color = 1
        while color in used_colors:
            color += 1

        colors[node] = color

    return colors



import matplotlib.pyplot as plt
import networkx as nx

def draw_graph_plain(graph):
    G = nx.Graph()

    for node, neighbors in graph.items():
        for n in neighbors:
            G.add_edge(node, n)

    pos = nx.spring_layout(G, seed=42)

    plt.figure()

    nx.draw(
        G,
        pos,
        with_labels=True,
        node_color="lightgray",
        node_size=1000,
        font_size=12,
        edge_color="gray"
    )

    plt.title("Grafo (sin coloración)")
    plt.show()
if __name__ == "__main__":

    colors = welsh_powell_coloring(mapa)

    print("Colores asignados:")
    for node, c in colors.items():
        print(f"{node} -> Color {c}")

    draw_graph(mapa, colors)