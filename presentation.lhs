\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{Zygohistomorphic prepromorphisms: An introduction to recursion schemes}
\author{Thomas Sutton}

\begin{document}
\maketitle

\begin{abstract}
Recursions schemes are useful ways of thinking implementing recursive operations, but they can also lead to
``ha ha only serious'' jokes like {\it zygohistomorphic prepromorphisms}. In this talk I'll try to explain
what this unwieldy name means and a few related (and, hopefully, more useful) recursion schemes.
\end{abstract}

\tableofcontents

\section{Recursion combinators}

\subsection{Maps, folds, scans}

\section{Categorising recursion schemes}

We can see that there are broad categories that recursion schemes fall into:

\begin{itemize}
\item We can tear a structure down.
\item We can build a structure up.
\end{itemize}

\section{Qualifying recursion schemes}

\section*{Questions}

\nocite{*}
\printbibliography[title=Bibliography]

\end{document}