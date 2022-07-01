\begin{code}
module OptTh.Examples.Trees.Types ( Edges
                                  , Node
                                  , TreeNode
                                  , Tree(..)
                                  , TreeNode'
                                  , Tree'
                                  , BinEdges
                                  , BinNode
                                  , BinTreeNode
                                  , BinTree
                                  , BinTreeNode'
                                  , BinTree'
                                  ) where
\end{code}

\section{Trees}

We will represent trees in the examples using following types.

First, we start with the edges. We represent these as a function from some index (the edge index) to the values at the end vertices of the edge.

\begin{code}
type Edges k c = k -> c
\end{code}

We will then define nodes as as product of Edges with a label.

\begin{code}
type Node k c v = (Edges k c, v)
\end{code}

Finally, we will represent a tree using an inductive structure build from these nodes. We will also allow values at the leafs of the tree of a different type.

\begin{code}
type TreeNode k v l = Node k (Tree k v l) v
data Tree     k v l = Leaf l | Node (TreeNode k v l)
\end{code}

We will use the 1 type as a leaf value of we don't want the leafs to represent any value. For these cases, we define the following type aliases:

\begin{code}
type TreeNode' k v = TreeNode k v ()
type Tree'     k v = TreeNode k v ()
\end{code}

\subsection{Binary Trees}

Using the definitions above, we can define binary trees by choosing Bool as the edge index. For convenience, we will define the following type aliases:

\begin{code}
type BinEdges     c      = Edges     Bool c
type BinNode      c v    = Node      Bool c v
type BinTreeNode    v l  = TreeNode  Bool   v l
type BinTree        v l  = Tree      Bool   v l
type BinTreeNode'   v    = TreeNode' Bool   v
type BinTree'       v    = Tree'     Bool   v
\end{code}