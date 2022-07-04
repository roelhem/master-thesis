\documentclass{mscthesis}

%include polycode.fmt
%include cs-format.fmt

\usepackage{thhaskell}
\usepackage{thnotation}
\usepackage{thdiagrams}

\title{Optics Thesis - Work in Progress}

\begin{document}

\maketitle

%let typeDefs=False

\chapter{Reference}

In this chapter, I put some reference tables that I made for myself during the implementation of optics in Haskell and the writing of the examples in this provisional report. I intent to use a simular style of notation for my final thesis.

\input{figures/table_notation}
\input{figures/table_optics-IE-rules}
\input{figures/table_optics-actions}

\chapter{Lattice}

In figure \ref{fig:optic-type-category}, you will find the start of the lattice that I needed for the implementation in Haskell. Note that it is actually not a lattice! I just imagined that it was, so that the examples in chapter \ref{chap:examples} did at least type-check.

We can view $\Optic : \Category \to \Category$ as a functor that sends a category to all the optics on that category. You could therefore view the diagram below as a part of $[\Category,\Category]$ where the arrows are natural transformations.

\input{figures/fig_optic-type-category}

\chapter{Examples}\label{chap:examples}

Here I give some examples of optics that I defined this week in Haskell. For these, I did my best to write them down such that they are easy to read. I've made a lot more examples in the implementation. For those, I refer to the git-repository at \texttt{https://github.com/roelhem/master-thesis} (Folders: \texttt{impl/haskell/src/OptTh/Examples} and \texttt{impl/haskell/src/OptTh/Simple}.)

%include OptTh/Simple/Standard.lhs

%include OptTh/Examples/Trees/SimpleLib.lhs

\chapter{Questions}

I have the following questions:

\section{Can you do $\lambda C$-like things in Haskell?}

The main struggle I had wasn't nessecaraly with the implementation of the lenses itself, but with the type-system of Haskell. I couldn't find an obvious way to implement the types of the optics so that they could still be composed. Instead, I tried to use a language extention called TemplateHaskell, but I found it hard to understand, as it is documented in a very annoying way. Therefore, I just created a stupidly large type family (see: \texttt{impl/haskell/src/OptTh/Simple/Types.hs}) that can calculate the type of the meet of two optics.

Now this implementation would be a lot simpler if I just had the full calculus of constructions ($\lambda C$) at my disposal. As a mathematician, I understand that system way better. If I had that, I could just calculate the types I need in a normal function. The main point is to avoid these annoyingly big and error-prone type families without learning TemplateHaskell.

Is there a way to do these $\lambda C$-like things in Haskell?

\section{How should I call the $\hask{Unit}$-typeclass?}

To give a propper description of an \emph{AffineTraversal}, which is one of the most common optics I encounter in the wild, I need the name of some property of functors. In essence, this is like \texttt{Applicative} with only the \texttt{pure} function. I called it \texttt{Unit} for now (see: \texttt{impl/haskell/src/OptTh/Types/Extra.hs}), but I think there is a better name for it.

In normal term, I would describe this property as follows: An endofunctor $F: \cat{C} \to \cat{C}$ is ??? iff there is a natural transformation $\mu : I \natto F$. Here, $I$ is the identity functor on $\cat{C}$.

How should I call the typeclass for functors that have this property?

\end{document}