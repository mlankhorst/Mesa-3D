
#include "nv50_ir_graph.h"

namespace nv50_ir {

Graph::Graph()
{
   root = NULL;
   size = 0;
   sequence = 0;
}

Graph::~Graph()
{
   Iterator *iter = this->safeIteratorDFS();

   for (; !iter->end(); iter->next())
      reinterpret_cast<Node *>(iter->get())->cut(false);

   putIterator(iter);
}

void Graph::insert(Node *node)
{
   if (!root) {
      root = node;
      size = 1;
      node->graph = this;
   } else {
      root->attach(node, Edge::TREE);
   }
}

void Graph::Edge::unlink()
{
   if (origin) {
      prev[0]->next[0] = next[0];
      next[0]->prev[0] = prev[0];
      if (origin->out == this)
         origin->out = (next[0] == this) ? NULL : next[0];

      --origin->outCount;
   }
   if (target) {
      prev[1]->next[1] = next[1];
      next[1]->prev[1] = prev[1];
      if (target->in == this)
         target->in = (next[1] == this) ? NULL : next[1];

      --target->inCount;
   }
}

const char *Graph::Edge::typeStr() const
{
   switch (type) {
   case TREE:    return "tree";
   case FORWARD: return "forward";
   case BACK:    return "back";
   case CROSS:   return "cross";
   case DUMMY:   return "dummy";
   case UNKNOWN:
   default:
      return "unk";
   }
}

Graph::Node::Node(void *priv) : data(priv),
                                in(0), out(0), graph(0),
                                visited(0),
                                inCount(0), outCount(0)
{
   // nothing to do
}

void Graph::Node::attach(Node *node, Edge::Type kind)
{
   Edge *edge = new Edge(this, node, kind);

   // insert head
   if (this->out) {
      edge->next[0] = this->out;
      edge->prev[0] = this->out->prev[0];
      edge->prev[0]->next[0] = edge;
      this->out->prev[0] = edge;
   }
   this->out = edge;

   if (node->in) {
      edge->next[1] = node->in;
      edge->prev[1] = node->in->prev[1];
      edge->prev[1]->next[1] = edge;
      node->in->prev[1] = edge;
   }
   node->in = edge;

   ++this->outCount;
   ++node->inCount;

   assert(this->graph);
   if (!node->graph) {
      node->graph = this->graph;
      ++node->graph->size;
   }

   if (kind == Edge::UNKNOWN)
      graph->classifyEdges();
}

bool Graph::Node::detach(Graph::Node *node)
{
   EdgeIterator ei = this->outgoing();
   for (; !ei.end(); ei.next())
      if (ei.getNode() == node)
         break;
   if (ei.end()) {
      ERROR("no such node attached\n");
      return false;
   }
   delete ei.getEdge();
   return true;
}

// Cut a node from the graph, deleting all attached edges and subsequently
// all nodes that become unreachable.
void Graph::Node::cut(const bool subtree)
{
   Iterator *itB = NULL, *itA = NULL;

   if (!graph || (!in && !out))
      return;

   if (subtree)
      itB = graph->safeIteratorDFS(); // unaffected by graph modification

   while (out)
      delete out;
   while (in)
      delete in;

   if (!subtree)
      return;
   itA = graph->iteratorDFS(); // all reachable nodes after cutting

   for (; !itB->end(); itB->next()) {
      Node *node = reinterpret_cast<Node *>(itB->get());
      if (node->visited != graph->sequence && node != this)
         delete node;
   }

   graph->putIterator(itA);
   graph->putIterator(itB);
}

Graph::Edge::Edge(Node *org, Node *tgt, Type kind)
{
   target = tgt;
   origin = org;
   type = kind;

   next[0] = next[1] = this;
   prev[0] = prev[1] = this;
}

bool
Graph::Node::reachableBy(Node *node, Node *term)
{
   Stack stack;
   Node *pos;
   const int seq = graph->nextSequence();

   stack.push(node);

   while (stack.getSize()) {
      pos = reinterpret_cast<Node *>(stack.pop().u.p);

      if (pos == this)
         return true;
      if (pos == term)
         continue;

      for (EdgeIterator ei = pos->outgoing(); !ei.end(); ei.next()) {
         if (ei.getType() == Edge::BACK || ei.getType() == Edge::DUMMY)
            continue;
         if (ei.getNode()->visit(seq))
            stack.push(ei.getNode());
      }
   }
   return pos == this;
}

class DFSIterator : public Iterator
{
public:
   DFSIterator(Graph *graph, const bool preorder)
   {
      unsigned int seq = graph->nextSequence();

      nodes = new Graph::Node * [graph->getSize() + 1];
      count = 0;
      pos = 0;
      nodes[graph->getSize()] = 0;

      if (graph->getRoot()) {
         graph->getRoot()->visit(seq);
         search(graph->getRoot(), preorder, seq);
      }
   }

   ~DFSIterator()
   {
      if (nodes)
         delete[] nodes;
   }

   void search(Graph::Node *node, const bool preorder, const int sequence)
   {
      if (preorder)
         nodes[count++] = node;

      for (Graph::EdgeIterator ei = node->outgoing(); !ei.end(); ei.next())
         if (ei.getNode()->visit(sequence))
            search(ei.getNode(), preorder, sequence);

      if (!preorder)
         nodes[count++] = node;
   }

   virtual bool end() const { return pos >= count; }
   virtual void next() { if (pos < count) ++pos; }
   virtual void *get() const { return nodes[pos]; }

   void reset() { pos = 0; }

protected:
   Graph::Node **nodes;
   int count;
   int pos;
};

Iterator *Graph::iteratorDFS(bool preorder)
{
   return new DFSIterator(this, preorder);
}

Iterator *Graph::safeIteratorDFS(bool preorder)
{
   return this->iteratorDFS(preorder);
}

class CFGIterator : public Iterator
{
public:
   CFGIterator(Graph *graph)
   {
      nodes = new Graph::Node * [graph->getSize() + 1];
      count = 0;
      pos = 0;
      nodes[graph->getSize()] = 0;

      // TODO: argh, use graph->sequence instead of tag and just raise it by > 1
      Iterator *iter;
      for (iter = graph->iteratorDFS(); !iter->end(); iter->next())
         reinterpret_cast<Graph::Node *>(iter->get())->tag = 0;
      graph->putIterator(iter);

      if (graph->getRoot())
         search(graph->getRoot(), graph->nextSequence());
   }

   virtual void *get() const { return nodes[pos]; }
   virtual bool end() const { return pos >= count; }
   virtual void next() { if (pos < count) ++pos; }

private:
   void search(Graph::Node *node, const int sequence)
   {
      Stack bb, cross;

      bb.push(node);

      while (bb.getSize()) {
         node = reinterpret_cast<Graph::Node *>(bb.pop().u.p);
         assert(node);
         if (!node->visit(sequence))
            continue;
         node->tag = 0;

         for (Graph::EdgeIterator ei = node->outgoing(); !ei.end(); ei.next()) {
            switch (ei.getType()) {
            case Graph::Edge::TREE:
            case Graph::Edge::FORWARD:
            case Graph::Edge::DUMMY:
               if (++(ei.getNode()->tag) == ei.getNode()->incidentCountFwd())
                  bb.push(ei.getNode());
               break;
            case Graph::Edge::BACK:
               continue;
            case Graph::Edge::CROSS:
               if (++(ei.getNode()->tag) == 1)
                  cross.push(ei.getNode());
               break;
            default:
               assert(!"unknown edge kind in CFG");
               break;
            }
         }
         nodes[count++] = node;

         if (bb.getSize() == 0)
            cross.moveTo(bb);
      }
   }

private:
   Graph::Node **nodes;
   int count;
   int pos;
};

Iterator *Graph::iteratorCFG()
{
   return new CFGIterator(this);
}

Iterator *Graph::safeIteratorCFG()
{
   return this->iteratorCFG();
}

void Graph::classifyEdges()
{
   DFSIterator *iter;
   int seq;

   for (iter = new DFSIterator(this, true); !iter->end(); iter->next()) {
      Node *node = reinterpret_cast<Node *>(iter->get());
      node->visit(0);
      node->tag = 0;
   }
   putIterator(iter);

   classifyDFS(root, (seq = 0));

   sequence = seq;
}

void Graph::classifyDFS(Node *curr, int& seq)
{
   Graph::Edge *edge;
   Graph::Node *node;

   curr->visit(++seq);
   curr->tag = 1;

   for (edge = curr->out; edge; edge = edge->next[0]) {
      node = edge->target;
      if (edge->type == Edge::DUMMY)
         continue;

      if (node->getSequence() == 0) {
         edge->type = Edge::TREE;
         classifyDFS(node, seq);
      } else
      if (node->getSequence() > curr->getSequence()) {
         edge->type = Edge::FORWARD;
      } else {
         edge->type = node->tag ? Edge::BACK : Edge::CROSS;
      }
   }

   for (edge = curr->in; edge; edge = edge->next[1]) {
      node = edge->origin;
      if (edge->type == Edge::DUMMY)
         continue;

      if (node->getSequence() == 0) {
         edge->type = Edge::TREE;
         classifyDFS(node, seq);
      } else
      if (node->getSequence() > curr->getSequence()) {
         edge->type = Edge::FORWARD;
      } else {
         edge->type = node->tag ? Edge::BACK : Edge::CROSS;
      }
   }

   curr->tag = 0;
}

} // namespace nv50_ir