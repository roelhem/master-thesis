%if False
\begin{code}
module OptTh.Examples.Trees.OpticsLib where

import OptTh.Examples.Trees.Types
import OptTh.Simple.Types
import OptTh.Simple.Helpers
import OptTh.Simple.Standard
import Data.Functor.Apply ((<.*>), (<*.>), Apply, MaybeApply (MaybeApply))
\end{code}
%endif

\section{Optics for Trees}

We will now use the standard optics as defined above and interpret them in the case of a tree. We will represent trees in Haskell using the following reccursive type \[
       \Conid{Tree}(K,V,L) \coloneqq \Pi X : *. (L \to X) \to ((K \to X) \to V \to X) \to X
\]

Here, we have that a tree can be a $\Conid{Leaf}$ with some value $l \in L$ or a $\Conid{Node}$, which is the product of a \emph{label} $v \in V$ and some edges $e \in K^{\Conid{Tree}(K,V,L)}$. Thus we have that
\[
       \Conid{Tree}(K,V,L) \simeq L + \left(V \times K^{\Conid{Tree}(K,V,L)}\right)
\]
We will call $K$ the \emph{index} of the $\Conid{Edges}$.

\subsection{General Case}

\subsubsection{Edges}

For the edges, we can only define a grate.

\begin{code}
vertices :: Grate (Edges k c) (Edges k c') c c'
vertices = Grate (\f k -> f ($ k))
\end{code}

However, if the edge indices are comparable, we can have a lens that for each vertex.

\begin{code}
vertexAt :: Eq k => k -> Lens' (Edges k v) v
vertexAt = _evAt'
\end{code}

\subsubsection{Nodes}

For the nodes, we will define two lenses: one that focuses on the label and one that focusses on the edges.

\begin{code}
label = _2 :: Lens (Node k c v) (Node k c v') v v'
edges = _1 :: Lens (Node k c v) (Node k' c' v ) (Edges k c) (Edges k' c')
\end{code}

We can compose the vertices Grate with the edges Lens to get a Glass that focusses on the children of the Node.

\begin{code}
children = edges >.> _coDom :: Glass (Node k c v) (Node k c' v) c c'

childAt :: Eq k => k -> Lens' (Node k c v) c
childAt k = edges >.> _evAt' k
\end{code}

\subsubsection{Tree}

For the tree, we firstly have two prisms. One that focuses on Nodes and one on the Leafs of the Tree.

\begin{code}
node :: Prism (Tree k v l) (Tree k' v' l) (TreeNode k v l) (TreeNode k' v' l)
node = Prism (\case 
              Leaf l -> Left (Leaf l)
              Node n -> Right n
       ) Node

leaf :: Prism' (Tree k v l) l
leaf = Prism (\case 
              Leaf l -> Right l
              Node n -> Left (Node n)
       ) Leaf
\end{code}

Note that we cannot change the type of a leaf, as nodes can also contain leafs in the structure. If have to change all leaves in a tree to change the type of those leaves. For this, we define the following setter optic.

\begin{code}
leaves :: Setter (Tree k v l) (Tree k v l') l l'
leaves = Setter $ \f -> \case 
       Leaf l -> Leaf (f l)
       Node n -> Node (over (children >.> leaves) f n)
\end{code}

We can define a simmular setter for the node labels of the tree. However, it is more useful to define it on the TreeNode instead of the Tree. We can get the setter on the Tree by precomposing the node-prism.

\begin{code}
labels :: Setter (TreeNode k v l) (TreeNode k v' l) v v'
labels = Setter (\f (e, v) -> (over (vertices >.> node >.> labels) f e, f v))
\end{code}

Finally, we can make a collection of lenses for each key path of a tree.

\begin{code}
nodeAt :: (Eq k, Foldable f) => f k -> AffineTraversal' (Tree k v l) (TreeNode k v l)
nodeAt = foldr (\k o -> node >.> childAt k >.> o) (og node)
\end{code}

Simmulary, we have a Traversal over a path. When we start at a TreeNode, we even know for sure that it has a label. Therefore, we have a path1 that starts at the TreeNode.

\begin{code}
path1 :: (Eq k) => [k] -> Traversal1' (TreeNode k v l) v
path1 []   = og label
path1 (k:ks) = Traversal1 (\f s -> 
                put (childAt k) 
                <$> traversing1 label f s
                <.*> travMaybe (path ks) f (get (childAt k) s)
              )

path :: (Eq k) => [k] -> Traversal' (Tree k v l) v
path ks = node >.> path1 ks
\end{code}

\subsection{Leaves with monoid values.}

We can make another optic if the tree has monoidal label values. This is because we can create a node from the value alone, having only leaves as its children with value \texttt{mempty}. This describes an AchromaticLens.

\begin{code}
aLabel :: Monoid l => AchromaticLens' (TreeNode k v l) v
aLabel = AlgLens (get label) (\case
              Nothing -> (review vertices (Leaf mempty),)
              Just n  -> put label n
       )
\end{code}

\subsection{Binary Trees.}

We have a lot more interesting optics if we look at binary trees.

First note that the edges of a binary tree are isomorphic to a product.

\begin{code}
vertexPair :: Iso (BinEdges c) (BinEdges c') (c, c) (c', c')
vertexPair = Iso (\e -> (e False, e True)) $ \(l, r) -> \case
                  False -> l
                  True  -> r
\end{code}

\begin{code}
leftVertex, rightVertex :: Lens' (BinEdges c) c
leftVertex  = vertexPair >.> _1
rightVertex = vertexPair >.> _2
\end{code}

\begin{code}
leftChild, rightChild :: Lens' (BinTreeNode v l) (BinTree v l)
leftChild  = childAt False
rightChild = childAt True
\end{code}

\begin{code}
inOrder  :: Traversal  (BinTree v l)     (BinTree v' l)     v v'
inOrder   = node >.> inOrder1

inOrder1 :: Traversal1 (BinTreeNode v l) (BinTreeNode v' l) v v'
inOrder1 = Traversal1 $ \f x -> 
                      (\ l c r -> (review vertexPair (l, r), c)) 
                      <$> travMaybe inOrder f (get leftChild  x)
                      <*.> f         (get label      x)
                      <.*> travMaybe inOrder f (get rightChild x)


preOrder :: (Bounded k, Enum k) => Traversal (Tree k v l) (Tree k v' l) v v'
preOrder  = node >.> preOrder1

preOrder1 :: (Bounded k, Enum k) => Traversal1 (TreeNode k v l) (TreeNode k v' l) v v'
preOrder1 = Traversal1 $ \f x -> 
                      (\ v e -> (e,v))
                      <$> f         (get label      x)
                      <.*> travMaybe (_img >.> preOrder) f (get edges  x)

postOrder :: (Bounded k, Enum k) => Traversal (Tree k v l) (Tree k v' l) v v'
postOrder = node >.> postOrder1

postOrder1 :: (Bounded k, Enum k) => Traversal1 (TreeNode k v l) (TreeNode k v' l) v v'
postOrder1 = Traversal1 $ \f x ->
                      (,) 
                      <$> travMaybe (_img >.> preOrder) f (get edges  x)
                      <*.> f         (get label      x)
\end{code}