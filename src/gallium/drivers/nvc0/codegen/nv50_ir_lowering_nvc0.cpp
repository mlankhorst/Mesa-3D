
#include "nv50/codegen/nv50_ir.h"
#include "nv50/codegen/nv50_ir_build_util.h"

#include "nv50_ir_target_nvc0.h"

namespace nv50_ir {

#define QOP_ADD  0
#define QOP_SUBR 1
#define QOP_SUB  2
#define QOP_MOV2 3

#define QUADOP(q, r, s, t)                      \
   ((QOP_##q << 0) | (QOP_##r << 2) |           \
    (QOP_##s << 4) | (QOP_##t << 6))

class NVC0LegalizePostRA : public Pass
{
private:
   virtual bool visit(Function *);
   virtual bool visit(BasicBlock *);

   void replaceZero(Instruction *);
   void split64BitOp(Instruction *);
   bool tryReplaceContWithBra(BasicBlock *);
   void propagateJoin(BasicBlock *);

   LValue *r63;
};

bool
NVC0LegalizePostRA::visit(Function *fn)
{
   r63 = New_LValue(fn, FILE_GPR);
   r63->reg.data.id = 63;
   return true;
}

void
NVC0LegalizePostRA::replaceZero(Instruction *i)
{
   for (int s = 0; i->srcExists(s); ++s) {
      ImmediateValue *imm = i->getSrc(s)->asImm();
      if (imm && imm->reg.data.u64 == 0)
         i->setSrc(s, r63);
   }
}

void
NVC0LegalizePostRA::split64BitOp(Instruction *i)
{
   if (i->dType == TYPE_F64) {
      if (i->op == OP_MAD)
         i->op = OP_FMA;
      if (i->op == OP_ADD || i->op == OP_MUL || i->op == OP_FMA ||
          i->op == OP_CVT || i->op == OP_MIN || i->op == OP_MAX ||
          i->op == OP_SET)
         return;
      i->dType = i->sType = TYPE_U32;

      i->bb->insertAfter(i, i->clone(true)); // deep cloning
   }
}

// replace CONT with BRA for single unconditional continue
bool
NVC0LegalizePostRA::tryReplaceContWithBra(BasicBlock *bb)
{
   if (bb->cfg.incidentCount() != 2 || bb->getEntry()->op != OP_PRECONT)
      return false;
   Graph::EdgeIterator ei = bb->cfg.incident();
   if (ei.getType() != Graph::Edge::BACK)
      ei.next();
   if (ei.getType() != Graph::Edge::BACK)
      return false;
   BasicBlock *contBB = BasicBlock::get(ei.getNode());

   if (!contBB->getExit() || contBB->getExit()->op != OP_CONT ||
       contBB->getExit()->getPredicate())
      return false;
   contBB->getExit()->op = OP_BRA;
   bb->remove(bb->getEntry()); // delete PRECONT

   ei.next();
   assert(ei.end() || ei.getType() != Graph::Edge::BACK);
   return true;
}

// replace branches to join blocks with join ops
void
NVC0LegalizePostRA::propagateJoin(BasicBlock *bb)
{
   if (bb->getEntry()->op != OP_JOIN || bb->getEntry()->asFlow()->limit)
      return;
   for (Graph::EdgeIterator ei = bb->cfg.incident(); !ei.end(); ei.next()) {
      BasicBlock *in = BasicBlock::get(ei.getNode());
      Instruction *exit = in->getExit();
      if (!exit) {
         in->insertTail(new FlowInstruction(func, OP_JOIN, bb));
         // there should always be a terminator instruction
         WARN("inserted missing terminator in BB:%i\n", in->getId());
      } else
      if (exit->op == OP_BRA) {
         exit->op = OP_JOIN;
         exit->asFlow()->limit = 1; // must-not-propagate marker
      }
   }
   bb->remove(bb->getEntry());
}

bool
NVC0LegalizePostRA::visit(BasicBlock *bb)
{
   Instruction *i, *next;

   // remove pseudo operations and non-fixed no-ops, split 64 bit operations
   for (i = bb->getFirst(); i; i = next) {
      next = i->next;
      if (i->isNop()) {
         bb->remove(i);
      } else {
         if (i->op != OP_MOV)
            replaceZero(i);
         if (typeSizeof(i->dType) == 8)
            split64BitOp(i);
      }
   }
   if (!bb->getEntry())
      return true;

   if (!tryReplaceContWithBra(bb))
      propagateJoin(bb);

   return true;
}

class NVC0LoweringPass : public Pass
{
public:
   NVC0LoweringPass(Program *);

private:
   virtual bool visit(BasicBlock *);
   virtual bool visit(Instruction *);

   bool handleRDSV(Instruction *);
   bool handleWRSV(Instruction *);
   bool handleDIV(Instruction *);
   bool handleMOD(Instruction *);
   bool handlePOW(Instruction *);
   bool handleSQRT(Instruction *);
   bool handleEXPORT(Instruction *);
   bool handleTEX(TexInstruction *);
   bool handleTXD(TexInstruction *);
   bool handleManualTXD(TexInstruction *);

   void checkPredicate(Instruction *);

private:
   const Target *const targ;

   BuildUtil bld;
};

NVC0LoweringPass::NVC0LoweringPass(Program *prog) : targ(prog->getTarget())
{
   bld.setProgram(prog);
}

bool
NVC0LoweringPass::visit(BasicBlock *bb)
{
   return true;
}

// move array source to first slot, convert to u16, add indirections
bool
NVC0LoweringPass::handleTEX(TexInstruction *i)
{
   const int dim = i->tex.target.getDim();
   const int arg = i->tex.target.getDim() + i->tex.target.isArray();

   // generate and move the tsc/tic/array source to the front
   if (dim != arg || i->tex.rIndirectSrc >= 0 || i->tex.sIndirectSrc >= 0) {
      LValue *src = New_LValue(func, FILE_GPR); // 0xssttaaaa

      Value *arrayIndex = i->tex.target.isArray() ? i->getSrc(dim) : NULL;
      for (int s = dim; s >= 1; --s)
         i->setSrc(s, i->getSrc(s - 1));
      i->setSrc(0, arrayIndex);

      Value *ticRel = i->getIndirectR();
      Value *tscRel = i->getIndirectS();

      if (arrayIndex)
         bld.mkCvt(OP_CVT, TYPE_U16, src, TYPE_F32, arrayIndex);
      else
         bld.loadImm(src, 0);

      if (ticRel) {
         i->setSrc(i->tex.rIndirectSrc, NULL);
         bld.mkOp3(OP_INSBF, TYPE_U32, src, ticRel, bld.mkImm(0x0917), src);
      }
      if (tscRel) {
         i->setSrc(i->tex.sIndirectSrc, NULL);
         bld.mkOp3(OP_INSBF, TYPE_U32, src, tscRel, bld.mkImm(0x0710), src);
      }

      i->setSrc(0, src);
   }

   // insert the offset source between coordinates and lod/bias, shadow
   if (i->tex.useOffsets) {
      int s = arg + i->tex.target.isShadow();
      if (i->op == OP_TXB || i->op == OP_TXL)
         ++s;
      for (; s > arg; --s)
         i->setSrc(s, i->getSrc(s - 1));

      i->setSrc(arg, bld.loadImm(NULL,
                                 ((i->tex.offset[0][0] & 0xf) << 0) |
                                 ((i->tex.offset[0][1] & 0xf) << 4) |
                                 ((i->tex.offset[0][2] & 0xf) << 8)));
   }

   return true;
}

bool
NVC0LoweringPass::handleManualTXD(TexInstruction *i)
{
   static const uint8_t qOps[4][2] =
   {
      { QUADOP(MOV2, ADD,  MOV2, ADD),  QUADOP(MOV2, MOV2, ADD,  ADD) }, // l0
      { QUADOP(SUBR, MOV2, SUBR, MOV2), QUADOP(MOV2, MOV2, ADD,  ADD) }, // l1
      { QUADOP(MOV2, ADD,  MOV2, ADD),  QUADOP(SUBR, SUBR, MOV2, MOV2) }, // l2
      { QUADOP(SUBR, MOV2, SUBR, MOV2), QUADOP(SUBR, SUBR, MOV2, MOV2) }, // l3
   };
   Value *def[4][4];
   Value *crd[3];
   Instruction *tex;
   Value *zero = bld.loadImm(bld.getSSA(), 0);
   int l, c;
   const int dim = i->tex.target.getDim();

   i->op = OP_TEX; // no need to clone dPdx/dPdy later

   for (c = 0; c < dim; ++c)
      crd[c] = bld.getScratch();

   bld.mkOp(OP_QUADON, TYPE_NONE, NULL);
   for (l = 0; l < 4; ++l) {
      // mov coordinates from lane l to all lanes
      for (c = 0; c < dim; ++c)
         bld.mkQuadop(0x00, crd[c], l, i->getSrc(c), zero);
      // add dPdx from lane l to lanes dx
      for (c = 0; c < dim; ++c)
         bld.mkQuadop(qOps[l][0], crd[c], l, i->dPdx[c].get(), crd[c]);
      // add dPdy from lane l to lanes dy
      for (c = 0; c < dim; ++c)
         bld.mkQuadop(qOps[l][1], crd[c], l, i->dPdy[c].get(), crd[c]);
      // texture
      bld.insert(tex = i->clone(true));
      for (c = 0; c < dim; ++c)
         tex->setSrc(c, crd[c]);
      // save results
      for (c = 0; i->defExists(c); ++c) {
         Instruction *mov;
         def[c][l] = bld.getSSA();
         mov = bld.mkMov(def[c][l], tex->getDef(c));
         mov->fixed = 1;
         mov->lanes = 1 << l;
      }
   }
   bld.mkOp(OP_QUADPOP, TYPE_NONE, NULL);

   for (c = 0; i->defExists(c); ++c) {
      Instruction *u = bld.mkOp(OP_UNION, TYPE_U32, i->getDef(c));
      for (l = 0; l < 4; ++l)
         u->setSrc(l, def[c][l]);
   }

   i->bb->remove(i);
   return true;
}

bool
NVC0LoweringPass::handleTXD(TexInstruction *txd)
{
   int dim = txd->tex.target.getDim();
   int arg = txd->tex.target.getDim() + txd->tex.target.isArray();

   handleTEX(txd);
   if (txd->src[arg].exists())
      ++arg;

   if (dim > 2 || txd->tex.target.isShadow())
      return handleManualTXD(txd);

   // at most s/t/array, x, y, offset
   assert(arg <= 4 && !txd->src[arg].exists());

   for (int c = 0; c < dim; ++c) {
      txd->src[arg + c * 2 + 0].set(txd->dPdx[c]);
      txd->src[arg + c * 2 + 1].set(txd->dPdy[c]);
      txd->dPdx[c] = NULL;
      txd->dPdy[c] = NULL;
   }
   return true;
}

bool
NVC0LoweringPass::handleWRSV(Instruction *i)
{
   Instruction *st;
   Symbol *sym;
   uint32_t addr;

   // must replace, $sreg are not writable
   addr = targ->getSVAddress(FILE_SHADER_OUTPUT, i->getSrc(0)->asSym());
   if (addr >= 0x400)
      return false;
   sym = bld.mkSymbol(FILE_SHADER_OUTPUT, 0, i->sType, addr);

   st = bld.mkStore(OP_EXPORT, i->dType,
                    sym, i->getIndirect(0), i->getSrc(1));

   st->perPatch = i->perPatch;

   bld.getBB()->remove(i);
   return true;
}

bool
NVC0LoweringPass::handleRDSV(Instruction *i)
{
   const SVSemantic sn = i->getSrc(0)->reg.data.sv.sv;
   const int si = i->getSrc(0)->reg.data.sv.index;
   Symbol *sym = i->getSrc(0)->asSym();
   Value *vtx = NULL;
   Instruction *ld;
   uint32_t addr = targ->getSVAddress(FILE_SHADER_INPUT, sym);

   if (addr >= 0x400) // mov $sreg
      return true;

   switch (sn) {
   case SV_POSITION:
      assert(prog->getType() == Program::TYPE_FRAGMENT);
      ld = New_Instruction(func, OP_LINTERP, TYPE_F32);
      ld->setDef(0, i->getDef(0));
      ld->setSrc(0, bld.mkSymbol(FILE_SHADER_INPUT, 0, TYPE_F32, addr));
      ld->setInterpolate(NV50_IR_INTERP_LINEAR);
      bld.getBB()->insertAfter(i, ld);
      break;
   case SV_TESS_COORD:
      assert(prog->getType() == Program::TYPE_TESSELLATION_EVAL);
      sym = bld.mkSysVal(SV_LANEID, 0);
      vtx = bld.mkOp1v(OP_RDSV, TYPE_U32, bld.getSSA(), sym);
      if (si == 2) {
         // using output file because we need the "O" specified on ld a[]
         sym = bld.mkSymbol(FILE_SHADER_OUTPUT, 0, TYPE_F32, 0x2f0);
         Value *x = bld.mkOp2v(OP_VFETCH, TYPE_F32, bld.getSSA(), sym, vtx);
         sym = bld.mkSymbol(FILE_SHADER_OUTPUT, 0, TYPE_F32, 0x2f4);
         Value *y = bld.mkOp2v(OP_VFETCH, TYPE_F32, bld.getSSA(), sym, vtx);

         bld.mkOp2(OP_SUB, TYPE_F32, i->getDef(0), bld.loadImm(NULL, 1.0f),
                   bld.mkOp2v(OP_ADD, TYPE_F32, bld.getSSA(), x, y));
      } else {
         sym = bld.mkSymbol(FILE_SHADER_OUTPUT, 0, TYPE_F32, addr);
         bld.mkOp2(OP_VFETCH, TYPE_F32, i->getDef(0), sym, vtx);
      }
      break;
   default:
      if (prog->getType() == Program::TYPE_TESSELLATION_EVAL)
         vtx = bld.mkOp1v(OP_PFETCH, TYPE_U32, bld.getSSA(), bld.mkImm(0));
      sym = bld.mkSymbol(FILE_SHADER_INPUT, 0, TYPE_NONE, addr);
      ld = bld.mkOp1(OP_VFETCH, i->dType, i->getDef(0), sym);
      ld->setSrc(1, vtx);
      if (i->src[0].isIndirect())
         ld->setIndirect(0, i->getIndirect(0));
      ld->perPatch = i->perPatch;
      if (1) // XXX: integer support
         bld.mkCvt(OP_CVT, TYPE_F32, i->getDef(0), TYPE_U32, i->getDef(0));
      break;
   }
   bld.getBB()->remove(i);
   return true;
}

bool
NVC0LoweringPass::handleDIV(Instruction *i)
{
   if (i->dType == TYPE_F32) {
      Instruction *rcp = bld.mkOp1(OP_RCP, TYPE_F32,
                                   bld.getSSA(), i->getSrc(1));
      i->op = OP_MUL;
      i->setSrc(1, rcp->getDef(0));
   } else
   if (i->dType == TYPE_U32) {
      bld.mkMovToReg(0, i->getSrc(0));
      bld.mkMovToReg(1, i->getSrc(1));
      bld.mkFlow(OP_CALL, NULL, CC_ALWAYS, NULL);
      bld.mkMovFromReg(i->getDef(0), 0);
   }

   return true;
}

bool
NVC0LoweringPass::handleMOD(Instruction *i)
{
   if (i->dType == TYPE_F32) {
      LValue *value = bld.getScratch();
      bld.mkOp1(OP_RCP, TYPE_F32, value, i->getSrc(1));
      bld.mkOp2(OP_MUL, TYPE_F32, value, i->getSrc(0), value);
      bld.mkOp1(OP_TRUNC, TYPE_F32, value, value);
      bld.mkOp2(OP_MUL, TYPE_F32, value, i->getSrc(1), value);
      i->op = OP_SUB;
      i->setSrc(1, value);
   } else
   if (i->dType == TYPE_U32) {
      bld.mkMovToReg(0, i->getSrc(0));
      bld.mkMovToReg(1, i->getSrc(1));
      bld.mkFlow(OP_CALL, NULL, CC_ALWAYS, NULL);
      bld.mkMovFromReg(i->getDef(0), 0);
   }

   return true;
}

bool
NVC0LoweringPass::handleSQRT(Instruction *i)
{
   Instruction *rsq = bld.mkOp1(OP_RSQ, TYPE_F32,
                                bld.getSSA(), i->getSrc(0));
   i->op = OP_MUL;
   i->setSrc(1, rsq->getDef(0));

   return true;
}

bool
NVC0LoweringPass::handlePOW(Instruction *i)
{
   LValue *val = bld.getScratch();

   bld.mkOp1(OP_LG2, TYPE_F32, val, i->getSrc(0));
   bld.mkOp2(OP_MUL, TYPE_F32, val, i->getSrc(1), val)->dnz = 1;
   bld.mkOp1(OP_PREEX2, TYPE_F32, val, val);

   i->op = OP_EX2;
   i->setSrc(0, val);
   i->setSrc(1, NULL);

   return true;
}

bool
NVC0LoweringPass::handleEXPORT(Instruction *i)
{
   if (prog->getType() == Program::TYPE_FRAGMENT) {
      int id = i->getSrc(0)->reg.data.offset / 4;

      if (i->src[0].isIndirect())
         return false; // TODO, ugly
      i->op = OP_MOV;
      i->src[0].set(i->src[1]);
      i->setSrc(1, NULL);
      i->setDef(0, New_LValue(func, FILE_GPR));
      i->getDef(0)->reg.data.id = id;

      prog->maxGPR = MAX2(prog->maxGPR, id);
   }
   return true;
}

// Generate a binary predicate if an instruction is predicated by
// e.g. an f32 value.
void
NVC0LoweringPass::checkPredicate(Instruction *insn)
{
   Value *pred = insn->getPredicate();
   Value *pdst;

   if (!pred || pred->reg.file == FILE_PREDICATE)
      return;
   pdst = New_LValue(func, FILE_PREDICATE);

   // CAUTION: don't use pdst->getInsn, the definition might not be unique,
   //  delay turning PSET(FSET(x,y),0) into PSET(x,y) to a later pass

   bld.mkCmp(OP_SET, CC_NEU, TYPE_U32, pdst, pred, bld.mkImm(0));

   insn->setPredicate(insn->cc, pdst);
}

//
// - add quadop dance for texturing
// - put FP outputs in GPRs
// - convert instruction sequences (POW)
//
bool
NVC0LoweringPass::visit(Instruction *i)
{
   if (i->prev)
      bld.setPosition(i->prev, true);
   else
   if (i->next)
      bld.setPosition(i->next, false);
   else
      bld.setPosition(i->bb, true);

   if (i->cc != CC_ALWAYS)
      checkPredicate(i);

   switch (i->op) {
   case OP_TEX:
   case OP_TXB:
   case OP_TXL:
   case OP_TXF:
   case OP_TXQ:
   case OP_TXG:
      return handleTEX(i->asTex());
   case OP_TXD:
      return handleTXD(i->asTex());
   case OP_EX2:
      bld.mkOp1(OP_PREEX2, TYPE_F32, i->getDef(0), i->getSrc(0));
      i->setSrc(0, i->getDef(0));
      break;
   case OP_POW:
      return handlePOW(i);
   case OP_DIV:
      return handleDIV(i);
   case OP_MOD:
      return handleMOD(i);
   case OP_SQRT:
      return handleSQRT(i);
   case OP_EXPORT:
      return handleEXPORT(i);
   case OP_RDSV:
      return handleRDSV(i);
   case OP_WRSV:
      return handleWRSV(i);
   case OP_LOAD:
      if (i->src[0].getFile() == FILE_SHADER_INPUT) {
         i->op = OP_VFETCH;
         assert(prog->getType() != Program::TYPE_FRAGMENT);
      }
      break;
   case OP_PINTERP:
      if (i->getSrc(0)->reg.data.offset >= 0x280 &&
          i->getSrc(0)->reg.data.offset <  0x2c0)
         i->setInterpolate(i->getSampleMode() | NV50_IR_INTERP_SC);
      break;
   case OP_LINTERP:
      if (i->getSrc(0)->reg.data.offset == 0x3fc) {
         Value *face = i->getDef(0);
         bld.setPosition(i, true);
         bld.mkOp2(OP_SHL, TYPE_U32, face, face, bld.mkImm(31));
         bld.mkOp2(OP_XOR, TYPE_U32, face, face, bld.mkImm(0xbf800000));
      }
      break;
   default:
      break;
   }   
   return true;
}

bool
TargetNVC0::runLegalizePass(Program *prog, CGStage stage) const
{
   if (stage == CG_STAGE_PRE_SSA) {
      NVC0LoweringPass pass(prog);
      return pass.run(prog, false, true);
   } else
   if (stage == CG_STAGE_POST_RA) {
      NVC0LegalizePostRA pass;
      return pass.run(prog, false, true);
   } else {
      return true;
   }
}

} // namespace nv50_ir
