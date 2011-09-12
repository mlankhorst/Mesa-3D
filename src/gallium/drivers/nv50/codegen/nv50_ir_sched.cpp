
class InstrScheduling : public Pass
{
public:
   InstrScheduling();
   ~InstrScheduling();

   virtual bool visit(Function *);
   virtual bool visit(BasicBlock *);

   inline bool isBarrier(const Instruction *) const;

private:
   struct Annotation
   {
      Graph::Node node; // node in dependency graph
      int id;           // id of instruction
      int distance;     // distance in cycles from start of BasicBlock
      int latency;
      int usesLeft;     // uses of generated values left in BB
   };
   Annotation *annot; // array of instruction annotation data
   int annotLimit;
   Graph deps;        // dependency graph
   Node root;
   BasicBlock list;
   int distance;

   Instruction *ld[DATA_FILE_COUNT];
   Instruction *st[DATA_FILE_COUNT];

   Instruction *checkRAW(const Instruction *);
   Instruction *checkWAR(const Instruction *);
   Instruction *checkWAW(const Instruction *);

   inline Annotation *getAnnot(const Instruction *);
};

InstrScheduling::InstrScheduling() : annot(NULL), annotLimit(0)
{
   // nothing to do
}

InstrScheduling::~InstrScheduling()
{
   if (annot)
      delete[] annot;
}

Instruction *
InstrScheduling::checkRAW(const Instruction *ld)
{
   Instruction *st;
   for (st = st[ld->src[0].getFile()]; st; st = st->next) {
      if ()
         break;
   }
   return st;
}

Instruction *
InstrScheduling::checkWAR(const Instruction *st)
{
   Instruction *ld;
   for (ld = ld[st->src[0].getFile()]; ld; ld = ld->next) {
      if ()
         break;
   }
   if (ld) {
      // old read can be removed, since they form a dependency chain
   }
}

Instruction *
InstrScheduling::checkWAW(const Instruction *wr)
{
   Instruction *st;
   for (st = st[ld->src[0].getFile()]; st; st = st->next) {
      if ()
         break;
   }
   if (st) {
      // old write can be removed since they form a dependency chain
   }
}

void
InstrScheduling::checkUses(Instruction *insn)
{

}

void
InstrScheduling::addDependency(Instruction *a, Instruction *b) // a before b
{

}

void
InstrScheduling::visit(Function *fn)
{
   if (annotLimit < fn->allInsns.getSize() ||
       annotLimit > fn->allInsns.getSize() * 2) {
      annotLimit = fn->allInsns.getSize();
      if (annot)
         delete[] annot;
      annot = new Annotation[annotLimit];
   }
   memset(annot, 0, fn->allInsns.getSize() * sizeof(Annotation));
}

bool InstrScheduling::isBarrier(const Instruction *insn) const
{
   if (insn->asFlow())
      return true;
   return insn->op == OP_EMIT || insn->op == OP_RESTART;
}

void
InstrScheduling::buildDependencyGraph(Instruction *insn)
{
   while (!isBarrier(insn)) {
      Annotation *ai = getAnnot(insn);
      ai->id = insn->id;
      ai->node.data = insn;

      if (insn->op == OP_LOAD || insn->op == OP_VFETCH) {
         checkRAW(insn);
      } else
      if (insn->op == OP_STORE || insn->op == OP_EXPORT) {
         checkWAW(insn);
         checkWAR(insn);
      }
      checkUses(insn);

      if (!ai->node.incidentCount())
         root.attach(&ai->node);

#if 0
      for (int s = 0; insn->srcExists(s); ++s) {
         Instruction *idep = insn->getSrc(s)->getInsn();
         Annotation *ad;
         if (idep->bb != insn->bb)
            continue;
         ad = getAnnot(idep);
         assert(ad->id == idep->id);
         ad->node.attach(&ai->node, Graph::Edge::FORWARD);
      }
#endif
   }
}

void
InstrScheduling::retire(Instruction *insn)
{
   Annotation *a = getAnnot(insn);
   Graph::Edge *e;

   distance += targ->getThroughput(insn);

   while ((e = a->node.outgoing())) {
      if (e->getTarget()->incidentCount() == 1) {
         ready = reinterpret_cast<Instruction *>(e->getTarget()->data);
         // target instruction becomes ready
         getAnnot(ready)->distance = distance;
         root.attach(&getAnnot(ready)->node);
      }
      delete e;
   }
}

Instruction *
InstrScheduling::selectNext()
{
   Instruction *best = NULL;
   for (Graph::EdgeIterator ei = root.outgoing(); !ei.end(); ei.next()) {
      Instruction *i = reinterpret_cast<Instruction *>(ei.getTarget()->data);
      Annoation *a = getAnnot(insn);

      // crit 1: distance to last dependency > latency of last dependency
      // crit 2: dependency satisfaction
      // crit 3: instruction that contains the last use of a value
      
      if (a->distance - a->latency <
          i->distance - i->latency)
         best = i;
      else
      if () {
      }
   }
   return best;
}

void
InstrScheduling::visit(BasicBlock *bb)
{
   buildDependencyGraph(bb);
   while (retire(selectNext()));
}
