Homology is a way of assigning a sequence of algebraic objects to other mathematical
structures. In this document we're interested in homology theories that can help us
analyse and understand topological spaces using techniques which can be implemented
in computer programs. We'll examine two examples of such theories:

\begin{itemize}
\item {\it Simplical homology} is founded on simplices -- triangles generalised
to arbitrary dimensions.
\item {\it Cubical homology} is founded on elementary cubes -- cubes of
arbitrary dimensions restricted to vertices at integer coordinates and edges
of unit length.
\end{itemize}

\subsection{Complexes}

We'll begin our journey by finding a way to represent our topological object
conveniently in our programs by approximating it with a combination of easily
processed pieces. Such a combination is a {\it complex}.

\begin{quote}
[The decomposition of a topological spce into simple pieces] qualified to be
called a complex if the pieces are topologically simple and their common
intersections are lower dimensional pieces of the same kind. \cite{edelsbrunner2010}
\end{quote}

\subsection{Cycles and boundaries}

\subsection{Chain complex}

\subsection{Algebra}

\subsection{Functors}

The various types of homology theory (including the two we'll see below) each
arises from a functor into the category of chain complexes. Once the objects are
mapped to chain complexes, the same mechanism (another functor) takes them to
homology groups in the same way.