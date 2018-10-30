\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{amsmath}
\usepackage{bbold}
\usepackage{color}
\usepackage{fancyvrb}
\usepackage{graphicx}
\usepackage{microtype}
\usepackage{minted}
\usepackage{parskip}
\usepackage{tikz-cd}
\usepackage{upquote}
\usepackage{url}

\usepackage[backend=biber,style=trad-abbrv]{biblatex}
\usepackage[hidelinks,pdftex,unicode]{hyperref}

\urlstyle{same}
\UseMicrotypeSet[protrusion]{basicmath}

\addbibresource{article.bib}

\defbibenvironment{bare}
  {\list
     {}
     {\setlength{\leftmargin}{\bibhang}%
      \setlength{\itemindent}{-\leftmargin}%
      \setlength{\itemsep}{\bibitemsep}%
      \setlength{\parsep}{2\bibparsep}}}
  {\endlist}
  {\item}

\newcommand{\register}[1]{{\ensuremath{\lvert #1 \rvert}}}
\newcommand{\operation}[1]{\ensuremath{ \textsc{bpf\!\_\MakeLowercase{#1}} }}
\newcommand{\constant}[1]{\ensuremath{ \textsc{#1} }}
\newcommand{\structure}[1]{\ensuremath{#1}}
\newcommand{\literal}[1]{#1}

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
In this talk I'll give a high-level introduction to the Berkeley Packet Filter
facility for efficient and secure execution of arbitrary user-supplied logic in
performance-sensitive privileged contexts. While BPF is supported by a number of
UNIX-like systems, many of the examples we'll see are Linux specific.
\end{abstract}

\section{Introduction}

The {\it Berkeley Packet Filter} (or, originally, {\it BSD Packet Filter}) was
developed to support high performance filtering of raw network traffic for
network analysis and debugging. It was faster and simpler than other solutions
to the same problem at the time and has since been adopted in other systems and
to address other, similar, problems.

\section{The Problem}

Monitoring and diagnosing network problems can be difficult without access to
raw network traffic to analyse. Any sensible network operating system provides
some way to get access to that raw network traffic. Generally speaking there are
three parts to the problem:

\begin{enumerate}
\item getting the traffic
\item finding the interesting bits
\item analysing them
\end{enumerate}

The first step is generally required to be in the operating system kernel as it
requires access to the internals of the operating system networking
infrastructure.

The last step is generally required to be in an user-space application as it
the particular analysis is likely to change very frequently, involve some sort
of user interaction, etc.

The middle step is where BPF comes into play. Every network packet that is given
to the analysis program must be copied. If we give every single network packet,
interesting or not, to the analysis program then we're likely to waste a lot of
allocations -- modern networks are unimaginably noisy places. If we can filter
the packets {\it before} they are copied from the kernel to the analysis program
then we can avoid all that unnecessary allocation.

(This idea might be familiar to some of us a predicate pushdown with technologies
like Hive and Parquet -- there we're trying to avoid reading data from disk.)

So the problem is this:

We want to allow user-space programs to create filters and supply them to the
kernel. The kernel will evaluate those filters against each network packet to
determine whether or not that packet should be copied to user-space for
processing. We need to do it quickly, efficiently, and securely.

And, while we're deciding whether or not to copy a packet to user-space, we
might also like to decide {\it how much} of the packet to copy. There's no point
copying a whole 64 kB byte IP packet if you just care about the source and
desintation addresses.

\section{The Design}

One solution to this problem is BPF: a small virtual machine design for
efficient implementation on modern hardware and an instruction set chosen for
the specific task -- parsing a contiguously allocated data structure and
determining what action should be performed on it.

The programs for this virtual machine have a number of properties that make
them very well suited to the task at hand without introducing behaviours which
might be problematic when run in the kernel:

\begin{itemize}

\item All branches must branch forward. Without backward jumps we can't write
programs that loop forever, etc. DOSing the kernel is bad.

\item The machine is register based. This reduces memory usage and memory
bandwidth which improves performance and, on Linux, makes JIT compilation of
filter programs relatively straightforward.

\end{itemize}

\subsection{Virtual Machine}

\begin{itemize}
\item \register{A} - a 32-bit accumulator
\item \register{X} - a 32-bit register
\item \register{M} - a 32-bit * 16-register file of scratch memory
\end{itemize}

\subsection{Instructions}

The programs
 for this virtual machine is structured as a sequence of 4-tuples:

\begin{minted}{c}
struct sock_filter {	/* Filter block */
	__u16	code;	/* Actual filter code */
	__u8	jt;	  /* Jump true */
	__u8	jf;	  /* Jump false */
	__u32	k;	  /* Generic multiuse field */
};

struct sock_fprog {			/* Required for SO_ATTACH_FILTER. */
	unsigned short		   len;	/* Number of filter blocks */
	struct sock_filter __user *filter;
};
\end{minted}


\subsection{Operations}

The operations of the virtual machine fall into several classes:

\begin{enumerate}

\item[\literal{0x00}] \operation{LD}

\operation{LDX} \literal{0x01}

\operation{ST} \literal{0x02}

\operation{STX} \literal{0x03}

\operation{ALU} \literal{0x04}

\operation{JMP} \literal{0x05}

\operation{RET} \literal{0x06}

\operation{MISC} \literal{0x07}

\end{enumerate}


\begin{itemize}
\item load values into \register{A} or \register{X}
\item store into \register{M}
\item jump conditionally on \register{A} or unconditionally
\item various arithmetic operations on \register{A}
\item exchanging values between \register{A} and \register{X}
\item returning
\end{itemize}

Instruction codes can be decoded into:

\begin{itemize}
\item An instruction class (load, store, arithmetic, jump, etc.)
\item A size (byte, half-word, word)
\item A mode (immediate, absolute, indirect, memory, etc.)
\item An operation (operations and predicates for arithmetic and jumps)
\item 
\end{itemize}

There are eleven addressing modes (in Linux) allowing operations to support
multiple sources/targets depending on the need. The modes include:

\begin{enumerate}
\item[0] The value in \register{X}.
\item[1] The byte at an absolute offset in the packet.
\item[2] The byte at a relative offset (w.r.t. \register{X}) in the packet.
\item[3] The word at an index in \register{M}.
\item[4] The value parameter in the instruction.
\item[5] The lower nibble * 4 at an offset in the packet.
\item[6] A jump label
\item[7] A jump label for ``true'', for ``false'', and a literal.
\item[8] A jump label for ``true'' and a literal.
\item[9] The value in \register{A}
\item[10] One of a number of Linux-specific extensions
\end{enumerate}

\subsection{JIT}

The kernel includes a built-in JIT compiler to translate BPF virtual machine
code into native machine code. This is supported on a many 

\section{Using BPF}

\subsection{Socket filters}

As mentioned above, the original usecase was in filtering packets delivered to
sockets for tools like `tcpdump`.

\subsection{SECure COMPuting}

The prevalence of multi-tenant mixed workload systems since the early days of
the web has driven a range of access control mechanisms. BPF provides a flexible
implementation mechanism for controlling access to system resources: instead of
filtering network packets we can filter {\it system calls}. This has a number of
benefits:

\begin{itemize}
\item Filters are applied during syscall dispatch. There's no need to for 
implement support in each syscall, or wait for the maintainers to get around to
namespacing each particular resource.

\item Filters can evaluate arbitrary syscall parameters. There's no need to modify the
resources being controlled (e.g. labels, namespaces, etc.)

\end{itemize}

\begin{minted}{c}
struct seccomp_data {
    int   nr;                   /* System call number */
    __u32 arch;                 /* AUDIT_ARCH_* value (see <linux/audit.h>) */
    __u64 instruction_pointer;  /* CPU instruction pointer */
    __u64 args[6];              /* Up to 6 system call arguments */
};
\end{minted}

The \structure{seccomp\_data} structure is initialised by the kernel to describe the
invoked a system call and the current state of the thread which called it.

Filter programs for seccomp have more than just "accept" and "reject" actions to
return. Here the 32-bit return values are split into two parts. The high 16-bits
are an action (listed below) and the low 16-bits are instruction data which will
be used by some actions.

\begin{itemize}
\item \constant{SECCOMP\_RET\_KILL\_PROCESS} will abort the call and terminate
the calling program.
\item \constant{SECCOMP\_RET\_KILL\_THREAD} will abort the call and terminate the
calling thread.
\item \constant{SECCOMP\_RET\_TRAP} will send a signal to the invoking thread
with the instruction data in the signal structure.
\item \constant{SECCOMP\_RET\_ERRNO} will abort the call and set errno in the
invoking thread to the instruction data.
\item \constant{SECCOMP\_RET\_TRACE} will notify the ptrace kernel debugger
before proceeding, including the instruction data. If there is no debugger,
it will abort the call and set errno.
\item \constant{SECCOMP\_RET\_LOG} will log the call and then proceed with the
call.
\item \constant{SECCOMP\_RET\_ALLOW} will proceed with the call.
\end{itemize}

\section{Caveats}

Like most things on Linux, the true situation in a recent kernel is a bit
different and varies between architectures.

The BPF language described above is the public interface for user-space programs
to supply filters to the kernel. On 64-bit architectures these programs will be
translated into a different "enhanced BPF" language which is designed to be more
directly translated to modern instruction sets.

There are 10 registers instead of 2. These registers are mapped to hardware
registers so that function calls can be JITed directly on 64-bit architectures
(where function parameters are passed in registers).

The registers are 64-bit (not 32-bit) with 32-bit sub-registers to maintain the
semantics of arithmetic operations, etc.

The two-target conditional jumps are replaced with single target conditional
jumps with fall through. ("jne ja nein 0" might be replaced with two
instructions "jne ja 0; jmp nein").

The internal instruction format is quite a bit different: \\
op:8, dst\_reg:4, src\_reg:4, off:16, imm:32 instead of op:16, jt:8, jf:8, k:32



\section*{Bibliography}

\nocite{*}

\printbibliography[env=bare,heading=none]

\end{document}