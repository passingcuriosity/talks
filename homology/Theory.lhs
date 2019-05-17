
\subsection{Outline}

\begin{itemize}
\item $X$ is an object (topological space, etc.)
\item $\partial_{n}$ 
\item $H_{n}$ maps $X$ to $n$-dimensional homology group
\end{itemize}

The homology of a space $X$ is (represented by) it's homology groups:

$H_{0}(X), H_{1}(X), ... $

Where $H_{k}(X)$ is a group describing the $k$-dimensional holes in $X$.

\begin{itemize}
\item $H_{0}(X)$ describes the gaps between disconnected components of $X$
\item $H_{1}(X)$ describes the 
\end{itemize}



\subsection{Complexes}

We'll begin our journey by finding a way to represent our topological object
conveniently in programs by approximating it with a combination of easily
processed pieces. Such a combination is a {\it complex}.

\begin{quote}
[The decomposition of a topological space into simple pieces] qualifies to be
called a complex if the pieces are topologically simple and their common
intersections are lower dimensional pieces of the same kind.
\cite{edelsbrunner2010}
\end{quote}

\subsection{Cycles}

\subsection{Boundaries}

\subsection{Chain complexes}

From a space $X$ define:

- a boundary operator $\partial_{n} : C_{n} \rightarrow C_{n-1}$

- a {\it chain complex} $C(X)$ -- a sequence of Abelian groups or modules $C_{n}$

- $C_i \equiv 0$ where $i \le 0$

\begin{code}
\end{code}


\subsection{Algebra}

\subsection{Functors}

The various types of homology theory (including the two we'll see below) each
arises from a functor into the category of chain complexes. Once the objects are
mapped to chain complexes, the same mechanism (another functor) takes them to
homology groups in the same way.

