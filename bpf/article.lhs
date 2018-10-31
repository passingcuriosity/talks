\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{amsmath}
\usepackage{bbold}
\usepackage{bytefield}
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

\newcommand\drawbits[1]{%
    \tikz[x=1.5ex,y=1.5ex,every path/.style={draw=black,semithick}]{%
        \foreach \y [count=\i] in {#1} {
            \expandafter\ifx\y0
                \draw (\i,0) rectangle (\i+1,1);
            \else
                \draw (\i,0) rectangle (\i+1,1);
                \filldraw[fill=black] (\i+0.2,0.2) rectangle (\i*1+0.8,0.8);
            \fi
        }%
    }%
}

\newcommand{\register}[1]{{\ensuremath{\lvert #1 \rvert}}}
\newcommand{\operation}[1]{\ensuremath{ \textsc{bpf\!\_\MakeLowercase{#1}} }}
\newcommand{\constant}[1]{\ensuremath{ \textsc{#1} }}
\newcommand{\structure}[1]{\ensuremath{#1}}
\newcommand{\literal}[1]{#1}
\newcommand{\field}[1]{#1}

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

\begin{center}
\includegraphics[width=0.75\textwidth]{filtering}
\end{center}

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

\subsection{Programs}

Like most similar machines, a program for the filter machine is a sequence of
instructions. In the BPF filter machine, instructions are comprised of four
fields:

\begin{itemize}
\item \field{code} - a 16-bit operation
\item \field{jt} - an 8-bit address for conditional jumps that are true
\item \field{jf} - an 8-bit address for conditional jumps that are false
\item \field{k} - a 32-bit generic field for use by instructions (literal values, etc.)
a literal value.
\end{itemize}

The C representation of a program looks like this:

\begin{minted}{c}
struct sock_filter {  /* Filter block */
    __u16 code;       /* Actual filter code */
    __u8  jt;         /* Jump true */
    __u8  jf;         /* Jump false */
    __u32 k;          /* Generic multiuse field */
};

struct sock_fprog {       /* Required for SO_ATTACH_FILTER. */
    unsigned short  len;  /* Number of filter blocks */
    struct sock_filter __user *filter;
};
\end{minted}

Once we have a program, we can submit it to the kernel to be attached to a 
socket:

\begin{minted}{c}
/* Generated by tcpdump -i en1 arp -d */
struct sock_filter code[] = {
  { 0x28, 0, 0, 0x0000000c }, /* (000) ldh      [12]                       */
  { 0x15, 0, 1, 0x00000806 }, /* (001) jeq      #0x806           jt 2 jf 3 */
  { 0x6, 0, 0, 0x00040000 },  /* (002) ret      #262144                    */
  { 0x6, 0, 0, 0x00000000 }   /* (003) ret      #0                         */
};

struct sock_fprog bpf = {
	.len = ARRAY_SIZE(code),
	.filter = code,
};

sock = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
if (sock < 0)
	/* ... bail out ... */

ret = setsockopt(sock, SOL_SOCKET, SO_ATTACH_FILTER, &bpf, sizeof(bpf));
if (ret < 0)
	/* ... bail out ... */
\end{minted}

\subsection{Instructions}

Each 16-bit opcode is decoded to determine the operation to be performed and any
parameters it may have.

The first step is to determine the operation class by inspecting the three least
significant bits. The particular class found will then determine how to decode
the rest of the instruction.

\drawbits{0,0,0,0,0,1,1,1}

\begin{itemize}
\item[\literal{0x00}] \operation{LD} - load values into register \register{A}

\item[\literal{0x01}] \operation{LDX} - load values into register \register{X}

\item[\literal{0x02}] \operation{ST} - store values from register \register{A}

\item[\literal{0x03}] \operation{STX} - store values from register \register{X}

\item[\literal{0x04}] \operation{ALU} - perform arithmeric on register \register{A}

\item[\literal{0x05}] \operation{JMP} - conditional and unconditional jumps

\item[\literal{0x06}] \operation{RET} - terminate execution, returning a value

\item[\literal{0x07}] \operation{MISC} - miscellaneous operations, extensions, etc.
\end{itemize}


\subsubsection{Load and Store}

Load and store instructions encode another two fields which determine where the
instruction will read/write from and the size of the read/write.

\begin{itemize}
\item \drawbits{0,0,0,1,1,0,0,0} determines the size of the read/write: 1, 2, 4,
or (in eBPF) 8 bytes.

\item \drawbits{1,1,1,0,0,0,0,0} determines how the location read/writen will
be determined. Immediate  absolute, indirect, memory, etc)
\end{itemize}

\subsubsection{Arithmetic and Jump}

Arithmetic and jump instructions modify or scrutinise register \register{A}.

ALU and JMP opcodes encode another two fields which determine which arithmetic
or comparison operation will be performed and where (for binary operations)
they will get the other operand.

\begin{itemize}
\item \drawbits{0,0,0,0,1,0,0,0} determines whether the operand will be taken
from the immediate \field{k} value in the instruction or from register
\register{X}.

\item \drawbits{1,1,1,1,0,0,0,0} determine the operation to be performed.

For ALU opcodes these are things like basic arithmetic and bit-banging operations.

For JMP instructions the operations are comparisons (eq, gt, gte) and some useful
optimisations (bit masking, etc.)
\end{itemize}

\subsubsection{Return}

The return opcodes encode a single futher field (and wastes the remining four
bits): which value to return.

\begin{itemize}
\item \drawbits{0,0,0,0,1,0,0,0} determins whether to return the immediate
\field{k} value in the instruction or the value from register \register{A}.
\end{itemize}

\subsubsection{Misc}

The miscellaneous class allows for more or less arbitrary extra functionality to
be added to the filter machine implementation.

\drawbits{1,1,1,1,1,0,0,0} The five remaining bits identify
specific miscellaneous operations.

There are only two specified in classic BPF:

\begin{itemize}
\item Copy the value from register \register{A} into register \register{X}
\item Copy the value from register \register{X} into register \register{A}
\end{itemize}


\subsection{JIT}

Linux has a second similar facility: extended BPF. This machine is based on BPF
but has been modified to more closely mimic modern CPUs. This makes it more
general (it can call kernel functions) and easier for a JIT compiler to
translate into machine code.

On 64-bit architectures BPF programs are translated into eBPF for execution
and/or for JIT compilation in the kernel, but 32-bit architectures still use the
classic BPF format.

In addition to the changes to the opcodes, the eBPF machine also has:

\begin{itemize}
\item ten registers (rather than two) which map one-to-one with hardware
registers on 64-bit architectures. This means that eBPF calling conventions
are just the native calling conventions on these platforms; there's no
marshalling at all to do when calling into or returning from kernel code from
an eBPF program.

\item registers are 64-bit (rather than 32-bit) with 32-bit sub-registers to
maintain semantics of classic programs.

\item two-way branches (with true and false locations on every jump) are
replaced with conditional jump and fall through. This simplifies the structure
over some code (programs which check a whitelist often compile as long chains of
``if good: OK else ...'').

\item as mentioned above, eBPF can call kernel functions.

\item operation encoding has been simplified. The 16-bit opcode field is split
into an 8-bit opcode field and two 4-bit source/destination register addresses
(to make use of all the extra registers). The two jump target fields are merge
into a single offset field.

Instructions which previously had flags to choose which register was operated on,
or whether to use a register or the immediate value instead have a flag to use
the source/destination register address or the immediate value.
\end{itemize}

\subsection{Verification}

The kernel needs to verify these programs before executing them. This
verification is a two-step process:

\begin{enumerate}
\item Control flow graph analysis to detect and reject loops, unreachable
instructions, etc.

\item Path analysis to determine the effects of the program on register and
stack values. This does simple type analysis (scalar values, various sorts of
pointers, undefined, etc.) and verifies that reads and writes repsect invariants
regarding these types (register typing, alignment, etc). It also track spill/fill
of registers, whitelist of callable kernel functions
\end{enumerate}

Of course, even with this restricted programming model and short program length
it's not feasible to analyse every path. The verifier does pruning 

\section{Using BPF}

\subsection{Socket filters}

As mentioned above, the original usecase was in filtering packets delivered to
sockets for tools like `tcpdump'.

\begin{verb}
tcpdump -i en1 arp
\end{verb}

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