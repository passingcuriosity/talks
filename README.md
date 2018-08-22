
# Homology notes

## Simplical homology

We'll deal mostly with abstract simplical homology here.

A n-simplex is an n-triangle of n+1 vertices:

- 0-simplex is a point
- 1-simplex is a line
- 2-simplex is a triangle
- 3-simplex is a tetrahedron
- ...

In general these constructions are oriented, so we can consider an
n-simplex to be an ordered set of n+1 vertices.

Geometrically the faces of an n-triangle are the (n-1)-triangles that
intersect it. The faces of $\triangle A B C $ are the lines $AB$,
$BC$, and $CA$. We have a $faces$ operation which gives the set of
faces of an n-simplex.

$$faces([v_1, v_2, \ldots, v_{n+1}) = $$

## Cubical homology

An elementary interval $I$ is either

1. "degenerate": $I=[l,l]$ for some $l \in \mathbb{Z}$; or
2. "non-degenerate" : $I=[l,l+1]$ for some $l \in \mathbb{Z}$.
