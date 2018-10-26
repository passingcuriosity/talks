\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{amsmath}
\usepackage{bbold}
\usepackage{color}
\usepackage{fancyvrb}
\usepackage{graphicx}
\usepackage{microtype}
\usepackage{parskip}
\usepackage{tikz-cd}
\usepackage{upquote}
\usepackage{url}

\usepackage[backend=biber,style=trad-abbrv]{biblatex}
\usepackage[hidelinks,pdftex,unicode]{hyperref}

\urlstyle{same}
\UseMicrotypeSet[protrusion]{basicmath}

\addbibresource{article.bib}

\defbibenvironment{midbib}
  {\list
     {}
     {\setlength{\leftmargin}{\bibhang}%
      \setlength{\itemindent}{-\leftmargin}%
      \setlength{\itemsep}{\bibitemsep}%
      \setlength{\parsep}{\bibparsep}}}
  {\endlist}
  {\item}

\title{Berkeley Packet Filters}
\author{Thomas Sutton}
\date{October 31, 2018}

\hypersetup{
  pdftitle = {Berkeley Packet Filters},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
Many software systems require a way for users to specify -- at run time -- more
or less arbitrary logic to, e.g., select packets for filtering, assign quality
of service labels, permit or deny system calls, etc. Often these systems also
require that this logic not adversely effect the correctness, timeliness,
availability, etc. of processing. In this talk I'll describe the Berkeley Packet
Filters facility widely used to address requirements like these in the modern
Linux kernel.
\end{abstract}

\section{Introduction}

\section{The Problems}

\section{The Design}

\section{Using BPF}

\section*{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}