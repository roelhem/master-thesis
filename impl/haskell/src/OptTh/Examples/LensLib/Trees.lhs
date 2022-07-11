\begin{code}
module OptTh.Examples.LensLib.Trees where

import Control.Lens (lens, prism, iso, _1, _2, view, bimapping, setting, over, set, review)
import Data.Bifunctor (first, second)
import Control.Applicative (liftA2)
import Data.Functor.Apply (liftF3, MaybeApply(..), (<.>), (<.*>), (<*.>) )
import Data.Traversable (fmapDefault, foldMapDefault)
import OptTh.Types.Extra (DT(..), dtExtract)
import Data.Distributive (Distributive (collect), distribute)
import OptTh.Types.VanLaarhoven ( Traversal
                                , Traversal1
                                , Lens
                                , Lens'
                                , Prism
                                , Iso
                                , AffineTraversal
                                , AffineTraversal'
                                , VLMap
                                , Grate
                                , Setter
                                , grate
                                )
import Data.Aeson.Encoding (value)
\end{code}

\section{Trees}

\subsection{Edges}

We start with the representation of the edges, which can be seen as a function from the child index type to the child type.

\begin{code}
type Edges k c = k -> c
\end{code}

It has a grate for the children (at the end of the edges)

\begin{code}
vertices :: Grate (Edges k c) (Edges k d) c d
vertices = grate rezip  -- Edges (\k -> f $ (`eChildAt` k) <$> x)
  where rezip f k = f ($ k)
\end{code}

\subsection{Node}

A node is then the product the edges with some label.

\begin{code}
type Node k c v = (Edges k c, v)
\end{code}

We can define two lenses: one for the label and one for the edges.

\begin{code}
label :: Lens (Node k c v) (Node k c v') v v'
edges :: Lens (Node k c v) (Node k' c' v ) (Edges k c) (Edges k' c')
label = lens snd (\(e, _) v' -> (e, v')) -- Same as _2
edges = lens fst (\(_, v) e' -> (e', v)) -- Same as _1
\end{code}

By combining the edges-lens with the vertices-lens, we can get a glass.

\begin{code}
-- children :: Glass (Node k c v) (Node k c' v) c c'
\end{code}

\subsection{Tree}

We can now define the inductive structure that represents a Tree.

\begin{code}
type TreeNode k v = Node k (Tree k v) v
data Tree     k v = Leaf | Root (TreeNode k v)
\end{code}

We then define a prism on the Tree to the TreeNode.

\begin{code}
root :: Prism (Tree k v) (Tree k v') (TreeNode k v) (TreeNode k v')
root = prism Root $ \case Leaf   -> Left Leaf
                          Root n -> Right n
\end{code}

We make a setter that can be used to change all the node-values in the Tree.

\begin{code}
labels :: Setter (TreeNode k v) (TreeNode k v') v v'
labels = setting (\f (es, v) -> (over (root.labels) f <$> es, f v))
\end{code}

\section{BinaryTree}

We can define a binary tree by keying the children with a Bool. We again start with the edges.

\subsection{Edges}

\begin{code}
type BinEdges c = Edges Bool c
\end{code}

Note that a binary node is isomorphic to a product of two children.

\begin{code}
vertexPair :: Iso (BinEdges c) (BinEdges c') (c, c) (c', c')
vertexPair = iso to from
  where to x        = (x False, x True)
        from (l, r) = \case False -> l
                            True  -> r
\end{code}

We also have two lenses on this node. One for the left child and one for the right child. We can use the Iso defined above and the predefined lenses of products to define these. However, note that we cannot change the type of the children, as we have no way to specify the other side of the node.

\begin{code}
leftVertex, rightVertex :: Lens' (BinEdges c) c
leftVertex  = vertexPair . _1
rightVertex = vertexPair . _2
\end{code}

\subsection{Node}

We make another alias for binary Nodes for convenience.

\begin{code}
type BinNode c v = Node Bool c v
\end{code}

This new binary node has two new lenses for the left and the right child. We can construct these using optic composition.

\begin{code}
leftChild, rightChild :: Lens' (BinNode c v) c
leftChild  = edges . leftVertex
rightChild = edges . rightVertex
\end{code}

\subsection{Tree}

For the binary tree, we define the following type aliases.

\begin{code}
type BinTreeNode v = TreeNode Bool v
type BinTree       = Tree Bool
\end{code}

We can represent traversals through the binary tree by a Traversal. These are the types of traversals that we want to define:

\begin{code}
inOrder1, preOrder1, postOrder1 :: Traversal  (BinTreeNode v) (BinTreeNode v') v v'
inOrder,  preOrder,  postOrder  :: Traversal  (BinTree v)     (BinTree v')     v v'
\end{code}

We will do this in two steps. Firstly, note that the join of a Prism and a Traversal1 is a Traversal. We can therefore define the traversals over the binary trees by precomposing the root-prism.

\begin{code}
inOrder   = root . inOrder1
preOrder  = root . preOrder1
postOrder = root . postOrder1
\end{code}

Secondly, we define the traversals over the nodes.

\begin{code}
inOrder1   f x = recc <$> inOrder f (view leftChild  x)
                      <*> f         (view label      x)
                      <*> inOrder f (view rightChild x)
  where recc l c r = (review vertexPair (l, r), c)

preOrder1  f x = recc <$> f         (view label      x)
                      <*> inOrder f (view leftChild  x)
                      <*> inOrder f (view rightChild x)
  where recc c l r = (review vertexPair (l, r), c)

postOrder1 f x = recc <$> inOrder f (view leftChild  x)
                      <*> inOrder f (view rightChild x)
                      <*> f         (view label      x)
  where recc l r c = (review vertexPair (l, r), c)
\end{code}

\section{Non-empty Binary Tree}