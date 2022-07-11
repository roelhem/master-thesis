%if False
\begin{code}

module OptTh.Examples.Trees.SimpleLib where

import OptTh.Prelude
import OptTh.Simple.Types
import OptTh.Simple.Helpers
import OptTh.Simple.Standard
import Data.Functor.Apply ((<.*>), (<*.>), MaybeApply (..))
import OptTh.Common.Categories (type (~>) (oTo))
\end{code}
%endif

%{
%format _tree = "\optic{tree}"


\section{Trees}

We will now use the standard optics as defined above and interpret them in the case of a tree. We will represent trees in Haskell using the following reccursive type 
\[
  \Conid{Tree}(K,V, L) \coloneqq \Pi X : *. (L \to X) \to ((V \to X) \to V \to X) \to X.
\]
Here, we will call $K$ the \emph{index} of the \emph{edges}, $V$ the \emph{node-labels}. We will define the \hask{Tree}-type in Haskell with the following data definition (simular as the one above):

\begin{code}
data Tree k v l where
  Leaf :: l -> Tree k v l
  Node :: (k -> Tree k v l) -> v -> Tree k v l
\end{code}

Here, we have that a tree can be a $\Conid{Leaf}$ with some label $l \in L$ or a $\Conid{Node}$, which is the product of a \emph{label} $v \in V$ and some edges $e \in \Conid{Tree}(K,V,L)^K$. Thus we have that
\[
       \Conid{Tree}(K,V,L) \isomorph L + \left(\Conid{Tree}(K,V,L)^K \times V\right)
\]

We can capture this isomorphism using an \Iso-optic with the following definition:

\begin{code}
type TreeNode k v l = (k -> Tree k v l, v)

_tree :: Iso (Tree k v l) (Tree k' v' l') (Either l (TreeNode k v l)) (Either l' (TreeNode k' v' l'))
_tree = Iso f g
  where
    f (Leaf l)        =  Left   l
    f (Node e v)      =  Right  (e,v)
    g (Left l)        =  Leaf   l
    g (Right (e, v))  =  Node   e  v
\end{code}

We can use this \optic{tree}-optic and compose them with the standard optics from the previous section to define different types of optics that are incredably useful for working with those trees.

\subsection{Basic Optics}

\subsubsection{Optics for the general case}

We will first define the most obvious optics for \hask{Tree} that always exist. We will start with the $\Prism$s for the \hask{Leaf} and \hask{Node}.

\begin{code}
node :: Prism (Tree k v l) (Tree k' v' l) (TreeNode k v l) (TreeNode k' v' l)
node = _tree % _Right

leaf :: Prism' (Tree k v l) l
leaf = _tree % _Left
\end{code}

Note that we cannot change the type of a leaf, as nodes can also contain leafs in the structure. We have to change all leaves in a tree to change the type of those leaves.

The optics that focus on the \emph{edges} and \emph{labels} of the root node can be made by precomposing $\opt{\pi}_1$ and $\opt{\pi}_2$ lenses with the $node$-prism. The result of these compositions  will be \emph{Affine Traversals}.

\begin{code}
edges :: AffineTraversal (Tree k v l) (Tree k' v l) (k -> Tree k v l) (k' -> Tree k' v l)
edges = node % _1

label :: AffineTraversal' (Tree k v l) v
label = node % _2
\end{code}

We will also make an optic that focusses on the children which is a \emph{Glass}. Note that this optic is defined on the tree-nodes instead of the tree itself. The reason for this is that I do not yet know what $\Glass \lor \Prism$ is appart from $\Setter$, which is too general for this example. We can get the optics on the tree by precomposing it with \emph{node}.

\begin{code}
children :: Glass (k -> Tree k w l, v) (k -> Tree k' w' l', v) (Tree k w l) (Tree k' w' l')
children = _1 % _coDom
\end{code}

We now have enough to define the \emph{Setter} on the leafs that can change the leaf-type, as well as a setter that changes all \emph{labels} of the tree.
 
\begin{code}
leafs :: Setter (Tree k v l) (Tree k v l') l l'
leafs = Setter $ \f -> \case 
       Leaf l -> Leaf (f l)
       Node e v -> Node (over (_coDom % leafs) f e) v
\end{code}

And a simmular setter for the node labels of the tree.

\begin{code}
labels :: Setter (Tree k v l) (Tree k v' l) v v'
labels = node % Setter (\f -> over _2 f . over (children % labels) f)
\end{code}

\subsubsection{Comparable keys}

We have a few more useful optics when the edge-keys are comparable. Firstly, we have a family of affine traversals that focusses on each of the immediate children of the tree.

\begin{code}
childAt :: Eq k => k -> AffineTraversal' (Tree k c v) (Tree k c v)
childAt k = edges % _evAt' k
\end{code}

And an affine traversal that has it's focus on some ancestor of the tree at some specified key-path.

\begin{code}
ancestorAt :: (Eq k, Foldable f) => f k -> AffineTraversal' (Tree k v l) (Tree k v l)
ancestorAt = foldr (\k o -> childAt k % o) (oTo Equality)
\end{code}

We can also define a traversal that walks over each node-label of a key-path. Note that we always have at least one element in such a path if we start at a node, being the label of the node itself. Therefore, this path on the node will become an \stdcat{Traversal1}. However, if we start on a \hask{Tree}, it can be empty, as a tree could also have a leaf without a node-label.

\begin{code}
path1 :: (Eq k) => [k] -> Traversal1' (TreeNode k v l) v
path1 []   = oTo _2
path1 (k:ks) = Traversal1 (\f s -> 
                put (_1 % _evAt' k) 
                <$> traversing1 _2 f s
                <.*> travMaybe (path ks) f (get (_1 % _evAt' k) s)
              )

path :: (Eq k) => [k] -> Traversal' (Tree k v l) v
path ks = node % path1 ks
\end{code}

\subsubsection{Leaves with monoid values.}

We can make another optic if the tree has monoidal label values. This is because we can create a node from the value alone, having only leaves as its children with value $\mempty$ (\texttt{mempty}). This describes an \emph{AchromaticLens}.

\begin{code}
aLabel :: Monoid l => AchromaticLens' (TreeNode k v l) v
aLabel = AlgLens (get _2) $ \case
              Nothing -> (review _coDom (Leaf mempty),)
              Just n  -> put _2 n
\end{code}

\subsubsection{Enumerable edges.}

We will now look at the optics that can be defined on a tree with edges that are enumerable. This allows us to order the leaf values. We can make a \hask{Traversal1} that traverses all the leafs of the tree in this order.

\begin{code}
travLeafs :: (Bounded k, Enum k) => Traversal1 (Tree k v l) (Tree k v l') l l'
travLeafs = Traversal1 $ \f -> \case
              Leaf l    ->  Leaf <$> f l
              Node e v  ->  (`Node` v) <$> traversing1 (_img % travLeafs) f e
\end{code}

Note that this is a \hask{Traversal1} as a finite tree always has at least one leaf. You could argue that an infinite tree has no leafs, but in that case, it would nerver terminate.

We can also traverse the node-labels of the trees in \emph{pre-order} and \emph{post-order}. These will both be normal \hask{Traversal}s on the tree, but \hask{Traversal1}s on the tree-nodes.

\begin{code}
preOrder :: (Bounded k, Enum k) => Traversal (Tree k v l) (Tree k v' l) v v'
preOrder  = node % preOrder1

preOrder1 :: (Bounded k, Enum k) => Traversal1 (TreeNode k v l) (TreeNode k v' l) v v'
preOrder1 = Traversal1 $ \f x -> flip (,)
                      <$> f (get _2 x)
                      <.*> travMaybe (_img % preOrder) f (get _1  x)

postOrder :: (Bounded k, Enum k) => Traversal (Tree k v l) (Tree k v' l) v v'
postOrder = node % postOrder1

postOrder1 :: (Bounded k, Enum k) => Traversal1 (TreeNode k v l) (TreeNode k v' l) v v'
postOrder1 = Traversal1 $ \f x -> (,) 
                      <$> travMaybe (_img % preOrder) f (get _1  x)
                      <*.> f         (get _2 x)
\end{code}


\subsubsection{Binary Trees.}

We have a lot more interesting optics if we look at binary trees. We can represent binary trees by choosing boolean values ($\mathcal{B}$) as the keys of the edges. We can define two affine traversals for the left and right child.

\begin{code}
leftChild, rightChild :: AffineTraversal' (Tree Bool v l) (Tree Bool v l)
leftChild   = edges % _pair % _1
rightChild  = edges % _pair % _2
\end{code}

On these binary trees, we have the \emph{in-order} traversal.

\begin{code}
inOrder  :: Traversal   (Tree      Bool v l) (Tree      Bool v' l) v v'
inOrder   = node % inOrder1

inOrder1 :: Traversal1  (TreeNode  Bool v l) (TreeNode  Bool v' l) v v'
inOrder1 = Traversal1 $ \f x -> 
                      (\ l c r -> (review _pair (l, r), c)) 
                      <$> travMaybe inOrder f (get (_1 % _evAt' True)  x)
                      <*.> f (get _2 x)
                      <.*> travMaybe inOrder f (get (_1 % _evAt' False) x)
\end{code}

%}