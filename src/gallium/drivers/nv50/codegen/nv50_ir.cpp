
#include "nv50_ir.h"
#include "nv50_ir_target.h"
#include "nv50_ir_driver.h"

extern "C" {
#include "nv50/nv50_program.h"
#include "nv50/nv50_debug.h"
}

namespace nv50_ir {

Modifier::Modifier(operation op)
{
   switch (op) {
   case OP_NEG: bits = NV50_IR_MOD_NEG; break;
   case OP_ABS: bits = NV50_IR_MOD_ABS; break;
   case OP_SAT: bits = NV50_IR_MOD_SAT; break;
   case OP_NOP: bits = NV50_IR_MOD_NOT; break;
   default:
      bits = 0;
      break;
   }
}

Modifier Modifier::operator*(const Modifier m) const
{
   unsigned int a, b, c;

   b = m.bits;
   if (this->bits & NV50_IR_MOD_ABS)
      b &= ~NV50_IR_MOD_NEG;

   a = (this->bits ^ b)      & (NV50_IR_MOD_NOT | NV50_IR_MOD_NEG);
   c = (this->bits | m.bits) & (NV50_IR_MOD_ABS | NV50_IR_MOD_SAT);

   return Modifier(a | c);
}

ValueRef::ValueRef() : value(0), insn(0), next(this), prev(this)
{
   indirect = -1;
   usedAsPtr = false;
}

ValueRef::~ValueRef()
{
   this->set(NULL);
}

ImmediateValue *ValueRef::getImmediate() const
{
   Value *src = value;

   while (src) {
      if (src->reg.file == FILE_IMMEDIATE)
         return src->asImm();

      Instruction *insn = src->getUniqueInsn();

      src = (insn && insn->op == OP_MOV) ? insn->getSrc(0) : NULL;
   }
   return NULL;
}

ValueDef::ValueDef() : value(0), insn(0), next(this), prev(this)
{
   // nothing to do
}

ValueDef::~ValueDef()
{
   this->set(NULL);
}

void
ValueRef::set(const ValueRef &ref)
{
   this->set(ref.get());
   mod = ref.mod;
   indirect = ref.indirect;
}

void
ValueRef::set(Value *refVal)
{
   if (value == refVal)
      return;
   if (value) {
      if (value->uses == this)
         value->uses = (next == this) ? NULL : next;
      value->unref();
      DLLIST_DEL(this);
   }

   if (refVal) {
      if (refVal->uses)
         DLLIST_ADDTAIL(refVal->uses, this);
      else
         refVal->uses = this;
      refVal->ref();
   }
   value = refVal;
}

void
ValueDef::set(Value *defVal)
{
   assert(next != this || prev == this); // check that SSA hack isn't active

   if (value == defVal)
      return;
   if (value) {
      if (value->defs == this)
         value->defs = (next == this) ? NULL : next;
      DLLIST_DEL(this);
   }

   if (defVal) {
      if (defVal->defs)
         DLLIST_ADDTAIL(defVal->defs, this);
      else
         defVal->defs = this;
   }
   value = defVal;
}

// TODO: make me faster by using a safe iterator
void
ValueDef::replace(Value *repVal, bool doSet)
{
   ValueRef **refs = new ValueRef * [value->refCount()];
   int n = 0;

   if (!refs && value->refCount())
      OOM();

   for (ValueRef::Iterator iter = value->uses->iterator(); !iter.end();
        iter.next()) {
      assert(n < value->refCount());
      refs[n++] = iter.get();
   }
   while (n)
      refs[--n]->set(repVal);

   if (doSet)
      this->set(repVal);

   if (refs)
      delete[] refs;
}

void
ValueDef::mergeDefs(ValueDef *join)
{
   DLLIST_MERGE(this, join, ValueDef *);
}

Value::Value()
{
  refCnt = 0;
  uses = NULL;
  defs = NULL;
  join = this;

  memset(&reg, 0, sizeof(reg));
  reg.size = 4;
}

bool
Value::coalesce(Value *jval, bool force)
{
   Value *repr = this->join; // new representative
   Value *jrep = jval->join;

   if (reg.file != jval->reg.file || reg.size != jval->reg.size) {
      if (!force)
         return false;
      ERROR("forced coalescing of values of different sizes/files");
   }

   if (!force && (this->join->reg.data.id != jval->join->reg.data.id)) {
      if (this->join->reg.data.id >= 0 &&
          jval->join->reg.data.id >= 0)
            return false;
      if (jval->join->reg.data.id >= 0) {
         repr = jval->join;
         jval = this;
      }

      // need to check all fixed register values of the program for overlap
      Function *func = defs->getInsn()->bb->getFunction();

      // TODO: put values in by register-id bins per function
      ArrayList::Iterator iter = func->allLValues.iterator();
      for (; !iter.end(); iter.next()) {
         Value *fixed = reinterpret_cast<Value *>(iter.get());
         assert(fixed);
         if (fixed->reg.data.id == repr->reg.data.id)
            if (fixed->livei.overlaps(repr->livei))
               return false;
      }
   }
   if (repr->livei.overlaps(jrep->livei)) {
      if (!force)
         return false;
      // do we really want this ? if at all, only for constraint ops
      INFO("NOTE: forced coalescing with live range overlap\n");
   }

   NV50_DBGMSG(PROG_RA, "coalescing %%%i <- %%%i\n", repr->id, jval->id);

   ValueDef::Iterator iter = jrep->defs->iterator();
   for (; !iter.end(); iter.next())
      iter.get()->get()->join = repr;

   repr->defs->mergeDefs(jrep->defs);
   repr->livei.unify(jrep->livei);

#if NV50_DEBUG & NV50_DEBUG_PROG_RA
   debug_printf("join(%%%i) = {", repr->id);
   for (ValueDef::Iterator it = repr->defs->iterator(); !it.end(); it.next()) {
      debug_printf(" %%%i", it.get()->get()->id);
   }
   debug_printf(" }\n");
   if (repr->livei.isEmpty())
      debug_printf("livei(%%%i) is empty\n", repr->id);
   else {
      debug_printf("livei(%%%i) = ", repr->id);
      repr->livei.print();
   }
#endif

   assert(repr->join == repr && jval->join == repr);
   return true;
}

LValue::LValue(Function *fn, DataFile file)
{
   reg.file = file;
   reg.size = (file != FILE_PREDICATE) ? 4 : 1;
   reg.data.id = -1;

   affinity = -1;

   fn->add(this, this->id);
}

LValue::LValue(Function *fn, LValue *lval)
{
   assert(lval);

   reg.file = lval->reg.file;
   reg.size = lval->reg.size;
   reg.data.id = -1;

   affinity = -1;

   fn->add(this, this->id);
}

Value *LValue::clone(Function *func) const
{
   LValue *that = New_LValue(func, reg.file);

   that->reg.size = this->reg.size;
   that->reg.type = this->reg.type;
   that->reg.data = this->reg.data;

   return that;
}

Symbol::Symbol(Program *prog, DataFile f, ubyte fidx)
{
   baseSym = NULL;

   reg.file = f;
   reg.fileIndex = fidx;
   reg.data.offset = 0;

   prog->add(this, this->id);
}

Value *
Symbol::clone(Function *func) const
{
   Program *prog = func->getProgram();

   Symbol *that = New_Symbol(prog, reg.file, reg.fileIndex);

   that->reg.size = this->reg.size;
   that->reg.type = this->reg.type;
   that->reg.data = this->reg.data;

   that->baseSym = this->baseSym;

   return that;
}

ImmediateValue::ImmediateValue(Program *prog, uint32_t uval)
{
   memset(&reg, 0, sizeof(reg));

   reg.file = FILE_IMMEDIATE;
   reg.size = 4;
   reg.type = TYPE_U32;

   reg.data.u32 = uval;

   prog->add(this, this->id);
}

ImmediateValue::ImmediateValue(Program *prog, float fval)
{
   memset(&reg, 0, sizeof(reg));

   reg.file = FILE_IMMEDIATE;
   reg.size = 4;
   reg.type = TYPE_F32;

   reg.data.f32 = fval;

   prog->add(this, this->id);
}

ImmediateValue::ImmediateValue(Program *prog, double dval)
{
   memset(&reg, 0, sizeof(reg));

   reg.file = FILE_IMMEDIATE;
   reg.size = 8;
   reg.type = TYPE_F64;

   reg.data.f64 = dval;

   prog->add(this, this->id);
}

ImmediateValue::ImmediateValue(const ImmediateValue *proto, DataType ty)
{
   reg = proto->reg;

   reg.type = ty;
   reg.size = typeSizeof(ty);
}

bool
ImmediateValue::isInteger(const int i) const
{
   switch (reg.type) {
   case TYPE_S8:
      return reg.data.s8 == i;
   case TYPE_U8:
      return reg.data.u8 == i;
   case TYPE_S16:
      return reg.data.s16 == i;
   case TYPE_U16:
      return reg.data.u16 == i;
   case TYPE_S32:
   case TYPE_U32:
      return reg.data.s32 == i; // as if ...
   case TYPE_F32:
      return reg.data.f32 == static_cast<float>(i);
   case TYPE_F64:
      return reg.data.f64 == static_cast<double>(i);
   default:
      return false;
   }
}

bool
ImmediateValue::isNegative() const
{
   switch (reg.type) {
   case TYPE_S8:  return reg.data.s8 < 0;
   case TYPE_S16: return reg.data.s16 < 0;
   case TYPE_S32:
   case TYPE_U32: return reg.data.s32 < 0;
   case TYPE_F32: return reg.data.u32 & (1 << 31);
   case TYPE_F64: return reg.data.u64 & (1ULL << 63);
   default:
      return false;
   }
}

bool
ImmediateValue::isPow2() const
{
   switch (reg.type) {
   case TYPE_U8:
   case TYPE_U16:
   case TYPE_U32: return util_is_power_of_two(reg.data.u32);
   default:
      return false;
   }
}

void
ImmediateValue::applyLog2()
{
   switch (reg.type) {
   case TYPE_S8:
   case TYPE_S16:
   case TYPE_S32:
      assert(!this->isNegative());
      // fall through
   case TYPE_U8:
   case TYPE_U16:
   case TYPE_U32:
      reg.data.u32 = util_logbase2(reg.data.u32);
      break;
   case TYPE_F32:
      reg.data.f32 = log2f(reg.data.f32);
      break;
   case TYPE_F64:
      reg.data.f64 = log2(reg.data.f64);
      break;
   default:
      assert(0);
      break;
   }
}

bool
ImmediateValue::compare(CondCode cc, float fval) const
{
   if (reg.type != TYPE_F32)
      ERROR("immediate value is not of type f32");

   switch (static_cast<CondCode>(cc & 7)) {
   case CC_TR: return true;
   case CC_FL: return false;
   case CC_LT: return reg.data.f32 <  fval;
   case CC_LE: return reg.data.f32 <= fval;
   case CC_GT: return reg.data.f32 >  fval;
   case CC_GE: return reg.data.f32 >= fval;
   case CC_EQ: return reg.data.f32 == fval;
   case CC_NE: return reg.data.f32 != fval;
   default:
      assert(0);
      return false;
   }
}

bool
Value::interfers(const Value *that) const
{
   uint32_t idA, idB;

   if (that->reg.file != reg.file || that->reg.fileIndex != reg.fileIndex)
      return false;
   if (this->asImm())
      return false;

   if (this->asSym()) {
      idA = this->join->reg.data.offset;
      idB = that->join->reg.data.offset;
   } else {
      idA = this->join->reg.data.id * this->reg.size;
      idB = that->join->reg.data.id * that->reg.size;
   }

   if (idA < idB)
      return (idA + this->reg.size > idB);
   else
   if (idA > idB)
      return (idB + that->reg.size > idA);
   else
      return (idA == idB);
}

bool
Value::equals(const Value *that, bool strict) const
{
   that = that->join;

   if (strict)
      return this == that;

   if (that->reg.file != reg.file || that->reg.fileIndex != reg.fileIndex)
      return false;
   if (that->reg.size != this->reg.size)
      return false;

   if (that->reg.data.id != this->reg.data.id)
      return false;

   return true;
}

bool
ImmediateValue::equals(const Value *that, bool strict) const
{
   const ImmediateValue *imm = that->asImm();
   if (!imm)
      return false;
   return reg.data.u64 == imm->reg.data.u64;
}

bool
Symbol::equals(const Value *that, bool strict) const
{
   if (this->reg.file != that->reg.file)
      return false;
   assert(that->asSym());

   if (this->baseSym != that->asSym()->baseSym)
      return false;

   return this->reg.data.offset == that->reg.data.offset;
}

void Instruction::init()
{
   next = prev = 0;

   cc = CC_ALWAYS;
   rnd = ROUND_N;
   cache = CACHE_CA;
   subOp = 0;

   saturate = join = ftz = dnz = restart = atomic = perPatch = 0;
   fixed = terminator = encSize = ipa = 0;
   lanes = 0xf;

   postFactor = 0;

   for (int p = 0; p < NV50_IR_MAX_DEFS; ++p)
      def[p].setInsn(this);
   for (int p = 0; p < NV50_IR_MAX_SRCS; ++p)
      src[p].setInsn(this);

   predSrc = -1;
   flagsDef = -1;
   flagsSrc = -1;
}

Instruction::Instruction() : extended(0)
{
   init();

   op = OP_NOP;
   dType = sType = TYPE_F32;

   id = -1;
   bb = 0;
}

Instruction::Instruction(Function *fn, operation opr, DataType ty, int ext)
   : extended(ext)
{
   init();

   op = opr;
   dType = sType = ty;

   fn->add(this, this->id);
}

Instruction::~Instruction()
{
   if (bb)
      bb->remove(this);

   for (int s = 0; srcExists(s); ++s)
      setSrc(s, NULL);
   // must unlink defs too since the list pointers will get deallocated
   for (int d = 0; defExists(d); ++d)
      setDef(d, NULL);
}

void
Instruction::setSrc(int s, ValueRef& ref)
{
   setSrc(s, ref.get());
   src[s].mod = ref.mod;
}

void
Instruction::swapSources(int a, int b)
{
   Value *value = src[a].get();
   Modifier m = src[a].mod;

   setSrc(a, src[b]);

   src[b].set(value);
   src[b].mod = m;
}

Instruction *
Instruction::clone(bool deep) const
{
   Instruction *insn = New_Instruction(bb->getFunction(), op, dType);
   assert(!asCmp() && !asFlow());
   cloneBase(insn, deep);
   return insn;
}

void
Instruction::cloneBase(Instruction *insn, bool deep) const
{
   insn->sType = this->sType;

   insn->cc = this->cc;
   insn->rnd = this->rnd;

   insn->saturate = this->saturate;
   insn->join = this->saturate;
   insn->atomic = this->saturate;
   insn->ftz = this->saturate;
   insn->restart = this->saturate;

   insn->postFactor = this->postFactor;

   if (deep) {
      if (!bb)
         return;
      Function *fn = bb->getFunction();
      for (int d = 0; this->defExists(d); ++d)
         insn->setDef(d, this->getDef(d)->clone(fn));
   } else {
      for (int d = 0; this->defExists(d); ++d)
         insn->setDef(d, this->getDef(d));
   }

   for (int s = 0; this->srcExists(s); ++s)
      insn->src[s].set(this->src[s]);

   insn->predSrc = this->predSrc;
   insn->flagsDef = this->flagsDef;
   insn->flagsSrc = this->flagsSrc;
}

unsigned int
Instruction::defCount(unsigned int mask) const
{
   unsigned int i, n;

   for (n = 0, i = 0; this->defExists(i); ++i, mask >>= 1)
      n += mask & 1;
   return n;
}

unsigned int
Instruction::srcCount(unsigned int mask) const
{
   unsigned int i, n;

   for (n = 0, i = 0; this->srcExists(i); ++i, mask >>= 1)
      n += mask & 1;
   return n;
}

bool
Instruction::setIndirect(int s, Value *value)
{
   int p;

   assert(this->srcExists(s));
   for (p = s + 1; this->srcExists(p); ++p);

   assert(p < NV50_IR_MAX_SRCS);
   src[p] = value;
   src[p].usedAsPtr = (value != 0);
   src[s].indirect = p;
   return true;
}

bool
Instruction::setPredicate(CondCode ccode, Value *value)
{
   cc = ccode;

   if (!value) {
      if (predSrc >= 0) {
         src[predSrc] = 0;
         predSrc = -1;
      }
      return true;
   }

   if (predSrc < 0) {
      int s;
      for (s = 0; s < NV50_IR_MAX_SRCS; ++s)
         if (!src[s].exists())
            break;
      assert(s < NV50_IR_MAX_SRCS);
      predSrc = s;
   }
   src[predSrc] = value;
   return true;
}

bool
Instruction::writesPredicate() const
{
   for (int d = 0; d < 2 && def[d].exists(); ++d)
      if (def[d].exists() &&
          (getDef(d)->inFile(FILE_PREDICATE) || getDef(d)->inFile(FILE_FLAGS)))
         return true;
   return false;
}

static bool
insnCheckCommutation(const Instruction *a, const Instruction *b)
{
   for (int d = 0; a->defExists(d); ++d)
      for (int s = 0; b->srcExists(s); ++s)
         if (a->getDef(d)->interfers(b->getSrc(s)))
            return false;
   return true;
}

bool
Instruction::isCommutationLegal(const Instruction *i) const
{
   bool ret = true;
   ret = ret && insnCheckCommutation(this, i);
   ret = ret && insnCheckCommutation(i, this);
   return ret;
}

TexInstruction::TexInstruction(Function *fn, operation op)
   : Instruction(fn, op, TYPE_F32, 1)
{
   memset(&tex, 0, sizeof(tex));

   tex.rIndirectSrc = -1;
   tex.sIndirectSrc = -1;
}

Instruction *
TexInstruction::clone(bool deep) const
{
   TexInstruction *tex = new TexInstruction(bb->getFunction(), op);
   cloneBase(tex, deep);

   tex->tex = this->tex;

   if (op == OP_TXD) {
      for (unsigned int c = 0; c < tex->tex.target.getDim(); ++c) {
         tex->dPdx[c].set(dPdx[c]);
         tex->dPdy[c].set(dPdy[c]);
      }
   }

   return tex;
}

const struct TexInstruction::Target::Desc TexInstruction::Target::descTable[] =
{
   { "1D",                1, 1, false, false, false },
   { "2D",                2, 2, false, false, false },
   { "2D_MS",             2, 2, false, false, false },
   { "3D",                3, 3, false, false, false },
   { "CUBE",              2, 3, false, true,  false },
   { "1D_SHADOW",         1, 1, false, false, true  },
   { "2D_SHADOW",         2, 2, false, false, true  },
   { "CUBE_SHADOW",       2, 3, false, true,  true  },
   { "1D_ARRAY",          1, 2, true,  false, false },
   { "2D_ARRAY",          2, 3, true,  false, false },
   { "2D_MS_ARRAY",       2, 3, true,  false, false },
   { "CUBE_ARRAY",        2, 3, true,  true,  false },
   { "1D_ARRAY_SHADOW",   1, 2, true,  false, true  },
   { "2D_ARRAY_SHADOW",   2, 3, true,  false, true  },
   { "RECT",              2, 2, false, false, false },
   { "RECT_SHADOW",       2, 2, false, false, true  },
   { "CUBE_ARRAY_SHADOW", 2, 4, true,  true,  true  },
   { "BUFFER",            1, 1, false, false, false },
};

CmpInstruction::CmpInstruction(Function *fn, operation op)
   : Instruction(fn, op, TYPE_F32, 1)
{
   setCond = CC_ALWAYS;
}

FlowInstruction::FlowInstruction(Function *fn, operation op,
                                 BasicBlock *targ)
   : Instruction(fn, op, TYPE_NONE, 1)
{
   target.bb = targ;

   if (op == OP_BRA || op == OP_CONT || op == OP_BREAK ||
       (op == OP_JOIN && targ))
      terminator = 1;

   allWarp = absolute = limit = 0;
}

Program::Program(Type type, Target *arch)
   : progType(type),
     target(arch),
     InstructionPool(sizeof(Instruction), 6),
     LValuePool(sizeof(LValue), 8)
{
   code = NULL;
   binSize = 0;

   maxGPR = -1;

   main = new Function(this, "MAIN");
}

Program::~Program()
{
   // free memory (if any)
}

void Program::releaseInstruction(Instruction *insn)
{
   if (insn->extended) {
      delete insn;
   } else {
      insn->~Instruction();
      InstructionPool.release(insn);
   }
}

void Program::releaseValue(Value *value)
{
   LValue *lval = value->asLValue();

   if (lval) {
      lval->~LValue();
      LValuePool.release(lval);
   } else {
      delete value;
   }
}


} // namespace nv50_ir

extern "C" {

static void
nv50_ir_init_prog_info(struct nv50_ir_prog_info *info)
{
   info->io.clipDistance = 0xff;
   info->io.pointSize = 0xff;
   info->io.edgeFlagIn = 0xff;
   info->io.edgeFlagOut = 0xff;
   info->io.fragDepth = 0xff;
   info->io.sampleMask = 0xff;
   info->io.backFaceColor[0] = info->io.backFaceColor[1] = 0xff;
}
   
int
nv50_ir_generate_code(struct nv50_ir_prog_info *info)
{
   int ret = 0;

   nv50_ir::Program::Type type;

   nv50_ir_init_prog_info(info);

#define PROG_TYPE_CASE(a, b)                                      \
   case PIPE_SHADER_##a: type = nv50_ir::Program::TYPE_##b; break

   switch (info->type) {
   PROG_TYPE_CASE(VERTEX, VERTEX);
// PROG_TYPE_CASE(HULL, TESSELLATION_CONTROL);
// PROG_TYPE_CASE(DOMAIN, TESSELLATION_EVAL);
   PROG_TYPE_CASE(GEOMETRY, GEOMETRY);
   PROG_TYPE_CASE(FRAGMENT, FRAGMENT);
   default:
      type = nv50_ir::Program::TYPE_COMPUTE;
      break;
   }

   nv50_ir::Target *targ = nv50_ir::Target::create(info->targetArch);
   if (!targ)
      return -1;

   nv50_ir::Program *prog = new nv50_ir::Program(type, targ);

   switch (info->bin.sourceFmt) {
#if 0
   case PIPE_IR_LLVM:
   case PIPE_IR_GLSL:
      return -1;
   case PIPE_IR_SM4:
      ret = prog->makeFromSM4(info) ? 0 : -1;
      break;
   case PIPE_IR_TGSI:
#endif
   default:
      ret = prog->makeFromTGSI(info) ? 0 : -1;
      break;
   }
   if (ret < 0)
      goto out;

   prog->getTarget()->runLegalizePass(prog, nv50_ir::CG_STAGE_PRE_SSA);
   prog->print();

   prog->convertToSSA();
   prog->getTarget()->runLegalizePass(prog, nv50_ir::CG_STAGE_SSA);
   prog->print();
   prog->optimizeSSA(1);

   if (!prog->registerAllocation()) {
      ret = -4;
      goto out;
   }
   prog->getTarget()->runLegalizePass(prog, nv50_ir::CG_STAGE_POST_RA);

   prog->optimizePostRA(1);

   if (!prog->emitBinary(info)) {
      ret = -5;
      goto out;
   }

out:
   debug_printf("IR_GENERATE_CODE: %i, cleaning up ...\n\n", ret);

   info->bin.maxGPR = prog->maxGPR;
   info->bin.code = prog->code;
   info->bin.codeSize = prog->binSize;

   delete prog;
   delete targ;

   return ret;
}

} // extern "C"
