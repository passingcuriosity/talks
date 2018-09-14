\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{amsmath}
\usepackage{bbold}
\usepackage{graphicx}
\usepackage{tikz-cd}
\usepackage{url}
\usepackage{upquote}
\usepackage{microtype}
\usepackage{parskip}
\usepackage[pdftex,unicode]{hyperref}
\usepackage{color}
\usepackage{fancyvrb}

\usepackage{amsthm}
\theoremstyle{definition}
\newtheorem{example}{Example}[section]

\UseMicrotypeSet[protrusion]{basicmath}
\urlstyle{same}

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{Some notes on queueing theory}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {Some notes on queueing theory},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle
\begin{abstract}
Queueing theory provides mathematical tools to analyse the behaviour of systems
of agents which make and service requests. This document provides an introduction
to the analysis of some simple queueing systems.
\end{abstract}

\tableofcontents

\clearpage

\section{Introduction}

{\it Queueing theory}\footnote{The spelling is traditional} is a field of
mathematics that analyses the behaviour of systems that involve {\it queueing}.
The results of such analyses are generally probabalistic statistics which can
help guide the optimisation of such systems.

\section{Models of queueing}

Queueing systems can be characterised more or less completely by specifying:

\begin{itemize}
\item The {\it arrival} process, which adds new work items to the queue.
\item The {\it service} distribution.
\item The number of {\it channels} which process work items.
\item The {\it capacity} of the system, including in-progress and queued work
items.
\item The size of the {\it calling population}.
\item The {\it discipline} which determines the order in which work items are
processed.
\end{itemize}

These factors are often written as a formula in {\it Kendall's notation}.

Single server vs multi server.

Probability distibution of arrivals.

Probability distribution of service times.

\section{Single-channel queue}

The sindle-channel queue models situations where customers pass through only
one channel to recieve a service. For example, a single queue in a post office,
or a dsingle phone line in a small business. A mathematical model for the queue
can be used to answer such questions as:

\begin{itemize}
\item What is the probability that the queue is empty?
\item What is the average number of people waiting in line?
\end{itemize}

This information can be used to help optimise a system involving such a queue.

\subsection{Probability distribution}

For many waiting line situations the arrivals occur randomly and independently
of other arrivals. In this situation, the Poisson distribution has been found
to give a good approximation of the arrival pattern.

For the Poisson distribution, the probability of $x$ arrivals in a specific time is given by

$$P(x) = \frac{\lambda^{x}e^{-\lambda}}{x!}, x=0,1,2,\ldots$$

with $\lambda$ the mean number of arrivals per time period.

\begin{example}
Company A has phone calls with a mean arrival rate of 10 phone calls per hour.
For a 12 minute period the mean arrival rate would be

$$\frac{10}{60} \times 12 = 2 calls$$

Then the Poisson probability function for $x$ phone calls during a 12 minute 
period would be:

$$P(x) = \frac{\lambda^{x}e^{-\lambda}}{x!} = \frac{2^{x}e^{-2}}{x!}$$

We could then determine the probabilities of 0, 1, 2, 3, and 4 phone calls
occurring during a 12 minute period as follows:

$$P(0) = \frac{2^{0}e^{-2}}{0!} \approx 0.1353$$

$$P(1) = \frac{2^{1}e^{-2}}{1!} \approx 0.2707$$

$$P(2) = \frac{2^{2}e^{-2}}{2!} \approx 0.2707$$

$$P(3) = \frac{2^{3}e^{-2}}{3!} \approx 0.1804$$

$$P(4) = \frac{2^{4}e^{-2}}{4!} \approx 0.0902$$

\end{example}

\subsection{Distribution of service time}

The service time is the time a customer spends being served once the service
has begun. For the previous example the service time would be the length of
a phone call once the company sales- person answers the phone.

In general it has been found that the service time in most cases can be
approximated by an exponential probability distribution. Using the exponential
probability distribution, the probability that the service time will be less
than or equal to a length of time $t$ is

$$P(service time \leq t) = 1 - e^{-\mu t}$$

where $\mu$ is the mean number of units that can be served per time period.

\begin{example}
Company A has found that their company sales-person can answer on average
about 15 calls per hour. For a 12 minute period we would expect this to
give:

$$\mu = \frac{15}{60} \times 12 = 3 calls$$

Then we could determine the probability of the service time being less than
or equal to 0.5, 1, 2, 3, and 4 minutes as follows:

$$P(time \leq 0.5) = 1 - e^{-0.5\mu} = 1 - e^{-0.5 \times 3} = 1 - e^{-1.5} \approx 0.7769$$

$$P(time \leq 1) = 1 - e^{-1\mu} = 1 - e^{-1 \times 3} = 1 - e^{-3} \approx 0.9502$$

$$P(time \leq 2) = 1 - e^{-2\mu} = 1 - e^{-2 \times 3} = 1 - e^{-6} \approx 0.9975$$

$$P(time \leq 3) = 1 - e^{-3\mu} = 1 - e^{-3 \times 3} = 1 - e^{-9} \approx 0.99988$$

$$P(time \leq 4) = 1 - e^{-4\mu} = 1 - e^{-4 \times 3} = 1 - e^{-12} \approx 0.999994$$

Note that as we approach 4 and progress beyond the probability approaches 1.
\end{example}

\subsection{Interarrival time}


One important characteristic of the arrival process is the interarrival time. The interarrival time is the amount of time between two successive arrivals. Using the exponential distribution, the probability that the next customer arrives within t units of the previous customer is given as

$$P(interarrival time \leq t) = 1 – e^{-\lambda t}$$

where $\lambda$ is the mean number of arrivals per time period.

\begin{example}
For Company A with a mean arrival rate of 10 phone calls per hour, determine the probability that a second call comes within 4 minutes of the previous call.

This is the same as asking for the probability of the interarrival time being less than 4 minutes. For a continuous function less than will give the same result as less than or equal to. So we evaluate

$$P(interarrival time \leq 4) = 1 – e^{-\lambda x4} = 1 – e^{-4 \lambda}$$

We require $\lambda$ in arrivals per minute.

$\lambda = 10$ per hour or 60 mins.

So $\lambda = \frac{10}{6}$ calls per minute $= \frac{1}{6}$.

$$P(interarrival time \leq 4) = 1 – e^{-4\times\frac{1}{6}} = 1 – e^{-\frac{2}{3}} \approx 􏰁 0.4866$$
\end{example}

\subsection{Steady state}

The beginning period where operations may still be a little unsettled is usually called the transient period. Once operations have settled down after a period of time we say that the system has reached a normal or steady-state operation. The waiting line models under consideration here are mainly for the steady state period of a business operation.

\section{Single-channel queue with Poisson arrival and exponential service}

The steady state operating characteristics of the waiting line model help describe the operation of the waiting line. These characteristics can be very helpful when decisions need to be made about a business waiting line.

For a single-channel waiting line with Poisson arrivals and exponential service times we have

$\lambda$ = mean number of arrivals per time period

$\mu$ = mean number of services per time period

The steady state operating characteristics are:

\begin{align}
P_{0} = & 1 - \frac{\lambda}{ \mu } & \text{the probability that no units are in the system} \\
L_q = & \frac{\lambda^2}{\mu(\mu-\lambda)} & \text{the average number of units in the waiting line} \\
L = & L_{q} + \frac{\lambda}{\mu} & \text{the average number of units in the system} \\
W_{q} = & \frac{L_{q}}{\lambda} & \text{the average time a unit spends in the waiting line} \\
W = & W_{q} + \frac{1}{\mu} & \text{the average time a unit spends in the system} \\
P_{w} = & \frac{\lambda}{\mu} & \text{the probability that an arriving unit has to wait for service} \\
% P_{n} = & \(\frac{\lambda}{\mu}\) & \text{whot}
\end{align}

\section{Single-channel queue with Poisson arrival and arbitrary service}

\appendix

\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}