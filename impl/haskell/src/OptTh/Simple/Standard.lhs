\section{Optics for standard data-types.}

%if False
\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module OptTh.Simple.Standard where

import OptTh.Simple.Types
import OptTh.Simple.Helpers
import Data.Semigroup.Foldable (Foldable1 (..))
import Data.Semigroup.Traversable (Traversable1 (..))
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix)
import Data.Tuple (swap)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (Foldable(..))
import qualified Data.Map as M
import Data.Bool (bool)
\end{code}
%endif

Lot of optics have simular properties as typeclasses. In this module, we will define the connection between optics and some commonly used Haskell typeclasses.

\subsection{Products}

%if typeDefs
\begin{code}
_1 :: Lens (a,y) (b,y) a b 
_2 :: Lens (x,a) (x,b) a b
_swap :: Iso (a,b) (c,d) (b,a) (d,c)
_pairGrate :: Grate (a, a) (b, b) a b
\end{code}
%endif

The most obvious lenses on products are $\opt{\pi}_1: \oobj{A \times Y}{B \times Y} \optto \oobj{A}{B}$ and $\opt{\pi}_2: \oobj{X \times A}{X \times B} \optto \oobj{A}{B}$ with focusses on the left and right side of the product respectively.

\begin{code}
_1 = Lens fst (flip (,) . snd)
_2 = Lens snd ((,) . fst)
\end{code}

We can also define the useful optic $\optic{swap}_\times \in \Iso(\oobj{A \times B}{C \times D}, \oobj{B \times A}{D \times C})$ that swaps the left and right side of the product.

\begin{code}
_swap = Iso swap swap
\end{code}

Finally, we have an optic $\optic{pairGrate} \in \Grate(\obj{A \times A}{B \times B}, \oobj{A}{B})$ for pairs that have the same value at both sides.

\begin{code}
_pairGrate = Grate (\f -> (f fst, f snd))
\end{code}

\subsection{Coproducts}

%if typeDefs
\begin{code}
_Left :: Prism (Either a x) (Either b x) a b
_Right :: Prism (Either x a) (Either x b) a b
_swapEither :: Iso (Either a b) (Either c d) (Either b a) (Either d c)
_tau1, _tau2 :: AchromaticLens (Either a a) (Either b b) a b
\end{code}
%endif

Like $\opt{\pi}_1$ and $\opt{\pi}_2$ were the most obvious \emph{lenses} on a product, we have two \emph{prisms} $\opt{\sigma}_1: \oobj{A + Y}{B + Y} \optto \oobj{A}{B}$ and $\opt{\sigma}_2: \oobj{X + A}{X + B} \optto \oobj{A}{B}$ with their focusses on the left and right side of the co-product.

\begin{code}
_Left  =  Prism (either  Right          (Left . Right)  ) Left
_Right =  Prism (either  (Left . Left)  Right           ) Right
\end{code}

And of course an isomorphism $\optic{swap}_+ \in \Iso\left(\oobj{A + B}{C + D}, \oobj{B + A}{D + C}\right)$ that swaps the prisms.

\begin{code}
_swapEither = Iso (either Right Left) (either Right Left)
\end{code}

Note that these optics are exactly the dual optics of their product counterparts. That is, if $\opt{\pi}_i$ is an optic for $A,B,C,D \in \cat{C}$, then $\opt{\sigma}_i$ is an optic for $A,B,C,D \in \op{\cat{C}}$.

\medskip

Now I would like to have a dual counterpart of the $\optic{pairGrate}$ lens, but I do not know yet what that would be or if it actually exists. Instead, we will define two optics $\opt{\tau}_1, \opt{\tau}_2 \in \AchromaticLens(\oobj{A + A}{B + B}, \oobj{A}{B})$ that have a simular behaviour. 

\begin{code}
_tau1 = AlgLens (either id id) $ \case
          Nothing  ->  Left
          Just s   ->  \b -> case s of
            Left    _  -> Left b
            Right   _  -> Right b
_tau2 = AlgLens (either id id) $ \case
          Nothing  ->  Right
          Just s   ->  \b -> case s of
            Left    _  -> Left b
            Right   _  -> Right b
\end{code}

Here, $\opt{\tau}_1, \opt{\tau}_2$ have the same results for $\Elim\Put$ and $\Elim\Get$ when used as a lens. They only differ for $\Elim\Put_\hask{Maybe}$, in which case $\opt{\tau}_1$ defaults to $\hask{Left}\;b$ and $\opt{\tau}_2$ defaults to $\hask{Right}\;b$.

\subsection{Exponentials}

%if typeDefs
\begin{code}
_coDom :: Grate (k -> a) (k -> b) a b
_evAt :: Eq k => k -> Lens (k -> a) (k -> Either b a) a b
_evAt' :: Eq k => k -> Lens' (k -> a) a
_img :: (Enum k, Bounded k) => Traversal1 (k -> a) (k -> b) a b
_single :: Iso (() -> a) (() -> b) a b
_pair :: Iso (Bool -> a) (Bool -> b) (a,a) (b,b)
_distr :: Iso (Either k k' -> a) (Either k k' -> b) (k -> a, k' -> a) (k -> b, k' -> b)
\end{code}
%endif

Every exponential type $A^K$ has an optic $\optic{coDom} \in \Grate\left(\oobj{A^K}{B^K},\oobj{A}{B}\right)$ that focusses on the codomain $A$. It can be defined as follows:

\begin{code}
_coDom = Grate (\f s -> f ($s))
\end{code}

In a programmers perpective, a \emph{grate} represent things that are ``like functions''. In a simular way, we have that \emph{glasses} are a bit like ``methods'' (as used in object oriented programming language). Now methods aren't a thing in Haskell, so we do not bother with those in this module.

\subsubsection{Special values of $K$}

We can make a few more optics if we put some extra constraints onto the domain of the function. Firstly, we can define $\optic{ev}_k \in \Lens\left(\oobj{A^K}{(B+A)^K}, \oobj{A}{B}\right)$ and $\optic{ev}_k' \in \Lens(K^A, A)$ for every value $k \in K$ if the values of $K$ are comparable (i.e. are in the \texttt{Eq}-typeclass).

\begin{code}
_evAt k = Lens (\s -> s k) (\s b k' -> if k' == k then Left b else Right (s k'))
_evAt' k = Lens (\s -> s k) (\s b k' -> if k' == k then b else s k')
\end{code}

We can even traverse over the whole image of $A^K$ if we can enumerate the values of the domain (i.e. $K$ is in the \texttt{Enum}- and \texttt{Bounded}-typeclasses). This gives us the optic $\optic{img} \in \TraversalOne\left(\oobj{A^K}{B^K}, \oobj{A}{B}\right)$.

%{
%format NE.!! = "\mathbin{\mathbf{!!}}"
%format NE.fromList = "\;"
%format .. = ",\ldots,"
\begin{code}
_img = Traversal1 (\f s -> (\bs k -> bs NE.!! ix k) <$> each f s)
  where 
    each f s  = sequence1 (f . s <$> NE.fromList [minBound .. maxBound])
    ix   k'   = fromEnum k' - fromEnum (minBound `asTypeOf` k')
\end{code}
%}

Lastly, we will define the $\Iso$s that correspond to the isomorphisms $A^{\mathbbm{1}} \isomorph A$, $A^{\Bool} \isomorph A \times A$ and $A^{K + K'} \isomorph A^K \times A^{K'}$.

\begin{code}
_single  = Iso (\f ->  f ()                    )  const
_pair    = Iso (\f ->  (f False,   f True)     )  (uncurry bool)
_distr   = Iso (\f ->  (f . Left,  f . Right)  )  (uncurry either)
\end{code}

\subsection{Haskell Typeclasses}

\subsubsection{Functors}

The fmap function of the \hask{Functor}-typeclass is enough to describe a Setter.

\begin{code}
_fmap :: Functor f => Setter (f a) (f b) a b
_fmap = Setter fmap
\end{code}

In the case of a Bifunctor, we have two setters.

\begin{code}
_first :: Bifunctor f => Setter (f a x) (f b x) a b
_first = Setter first

_second :: Bifunctor f => Setter (f x a) (f x b) a b
_second = Setter second
\end{code}

If the functor is applicative, we can make a Kaleidoscope.

\begin{code}
_seq :: Applicative f => Kaleidoscope (f a) (f b) a b
_seq = Kaleidoscope (\f ss -> f <$> sequenceA ss)
\end{code}

And of it is traversable, we can make a Traversal.

\begin{code}
_traverse :: Traversable f => Traversal (f a) (f b) a b
_traverse = Traversal traverse

_traverse1 :: Traversable1 f => Traversal1 (f a) (f b) a b
_traverse1 = Traversal1 traverse1
\end{code}

\subsubsection{Foldable}

For a foldable, we have a Fold.

\begin{code}
_foldMap :: Foldable f => Fold (f a) b a t
_foldMap = Fold foldMap
\end{code}

We get simular folds with a Foldable1

\begin{code}
_foldMap1 :: Foldable1 f => Fold1 (f a) b a t
_foldMap1 = Fold1 foldMap1
\end{code}