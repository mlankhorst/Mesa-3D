
#ifndef __NV50_IR_INLINES_H__
#define __NV50_IR_INLINES_H__

static inline CondCode reverseCondCode(CondCode cc)
{
   static const uint8_t ccRev[8] = { 0, 4, 2, 6, 1, 5, 3, 7 };

   return static_cast<CondCode>(ccRev[cc & 7] | (cc & ~7));
}

static inline CondCode inverseCondCode(CondCode cc)
{
   return static_cast<CondCode>(cc ^ 7);
}

static inline bool isMemoryFile(DataFile f)
{
   return (f >= FILE_MEMORY_CONST && f <= FILE_MEMORY_LOCAL);
}

static inline bool isTextureOp(operation op)
{
   return (op >= OP_TEX && op <= OP_TEXCSAA);
}

static inline unsigned int typeSizeof(DataType ty)
{
   switch (ty) {
   case TYPE_U8:
   case TYPE_S8:
      return 1;
   case TYPE_F16:
   case TYPE_U16:
   case TYPE_S16:
      return 2;
   case TYPE_F32:
   case TYPE_U32:
   case TYPE_S32:
      return 4;
   case TYPE_F64:
   case TYPE_U64:
   case TYPE_S64:
      return 8;
   case TYPE_B96:
      return 12;
   case TYPE_B128:
      return 16;
   default:
      return 0;
   }
}

static inline DataType typeOfSize(unsigned int size,
                                  bool flt = false, bool sgn = false)
{
   switch (size) {
   case 1: return sgn ? TYPE_S8 : TYPE_U8;
   case 2: return flt ? TYPE_F16 : (sgn ? TYPE_S16 : TYPE_U16);
   case 8: return flt ? TYPE_F64 : (sgn ? TYPE_S64 : TYPE_U64);
   case 12: return TYPE_B96;
   case 16: return TYPE_B128;
   case 4:
   default:
      return flt ? TYPE_F32 : (sgn ? TYPE_S32 : TYPE_U32);
   }
}

static inline bool isFloatType(DataType ty)
{
   return (ty >= TYPE_F16 && ty <= TYPE_F64);
}

static inline bool isSignedIntType(DataType ty)
{
   return (ty == TYPE_S8 || ty == TYPE_S16 || ty == TYPE_S32);
}

static inline bool isSignedType(DataType ty)
{
   switch (ty) {
   case TYPE_NONE:
   case TYPE_U8:
   case TYPE_U16:
   case TYPE_U32:
   case TYPE_B96:
   case TYPE_B128:
      return false;
   default:
      return true;
   }
}

const ValueRef *ValueRef::getIndirect(int dim) const
{
   return isIndirect(dim) ? &insn->src[indirect[dim]] : NULL;
}

DataFile ValueRef::getFile() const
{
   return value ? value->reg.file : FILE_NULL;
}

unsigned int ValueRef::getSize() const
{
   return value ? value->reg.size : 0;
}

Value *ValueRef::rep() const
{
   assert(value);
   return value->join;
}

Value *ValueDef::rep() const
{
   assert(value);
   return value->join;
}

DataFile ValueDef::getFile() const
{
   return value ? value->reg.file : FILE_NULL;
}

unsigned int ValueDef::getSize() const
{
   return value ? value->reg.size : 0;
}

void ValueDef::setSSA(LValue *lval)
{
   Value *save = value;

   this->set(NULL);
   prev = reinterpret_cast<ValueDef *>(save);
   value = lval;
   lval->defs = this;
}

void ValueDef::restoreDefList()
{
   if (next == this)
      prev = this;
}

const LValue *ValueDef::preSSA() const
{
   return reinterpret_cast<LValue *>(prev);
}

Instruction *Value::getInsn() const
{
   assert(!defs || getUniqueInsn());
   return defs ? defs->getInsn() : NULL;
}

Instruction *Value::getUniqueInsn() const
{
   if (defs) {
      if (join != this) {
         ValueDef::Iterator it = defs->iterator();
         while (!it.end() && it.get()->get() != this)
            it.next();
         assert(it.get()->get() == this);
         return it.get()->getInsn();
      }

      // after regalloc, the definitions of coalesced values are linked
      if (reg.data.id < 0) {
         ValueDef::Iterator it = defs->iterator();
         int nDef;
         for (nDef = 0; !it.end() && nDef < 2; it.next())
            if (it.get()->get() == this) // don't count joined values
               ++nDef;
         if (nDef > 1)
            WARN("value %%%i not uniquely defined\n", id); // return NULL ?
      }

      assert(defs->get() == this);
      return defs->getInsn();
   }
   return NULL;
}

Value *Instruction::getIndirect(int s, int dim) const
{
   return src[s].isIndirect(dim) ? getSrc(src[s].indirect[dim]) : NULL;
}

Value *Instruction::getPredicate() const
{
   return (predSrc >= 0) ? getSrc(predSrc) : NULL;
}

Value *TexInstruction::getIndirectR() const
{
   return tex.rIndirectSrc >= 0 ? getSrc(tex.rIndirectSrc) : NULL;
}

Value *TexInstruction::getIndirectS() const
{
   return tex.rIndirectSrc >= 0 ? getSrc(tex.rIndirectSrc) : NULL;
}

CmpInstruction *Instruction::asCmp()
{
   if (op >= OP_SET_AND && op <= OP_SLCT && op != OP_SELP)
      return static_cast<CmpInstruction *>(this);
   return NULL;
}

const CmpInstruction *Instruction::asCmp() const
{
   if (op >= OP_SET_AND && op <= OP_SLCT && op != OP_SELP)
      return static_cast<const CmpInstruction *>(this);
   return NULL;
}

FlowInstruction *Instruction::asFlow()
{
   if (op >= OP_BRA && op <= OP_JOIN)
      return static_cast<FlowInstruction *>(this);
   return NULL;
}

const FlowInstruction *Instruction::asFlow() const
{
   if (op >= OP_BRA && op <= OP_JOINAT)
      return static_cast<const FlowInstruction *>(this);
   return NULL;
}

TexInstruction *Instruction::asTex()
{
   if (op >= OP_TEX && op <= OP_TEXCSAA)
      return static_cast<TexInstruction *>(this);
   return NULL;
}

const TexInstruction *Instruction::asTex() const
{
   if (op >= OP_TEX && op <= OP_TEXCSAA)
      return static_cast<const TexInstruction *>(this);
   return NULL;
}

// XXX: use a virtual function so we're really really safe ?
LValue *Value::asLValue()
{
   if (reg.file >= FILE_GPR && reg.file <= FILE_ADDRESS)
      return static_cast<LValue *>(this);
   return NULL;
}

Symbol *Value::asSym()
{
   if (reg.file >= FILE_MEMORY_CONST)
      return static_cast<Symbol *>(this);
   return NULL;
}

const Symbol *Value::asSym() const
{
   if (reg.file >= FILE_MEMORY_CONST)
      return static_cast<const Symbol *>(this);
   return NULL;
}

void Symbol::setOffset(int32_t offset)
{
   reg.data.offset = offset;
}

void Symbol::setAddress(Symbol *base, int32_t offset)
{
   baseSym = base;
   reg.data.offset = offset;
}

void Symbol::setSV(SVSemantic sv, uint32_t index)
{
   reg.data.sv.sv = sv;
   reg.data.sv.index = index;
}

ImmediateValue *Value::asImm()
{
   if (reg.file == FILE_IMMEDIATE)
      return static_cast<ImmediateValue *>(this);
   return NULL;
}

const ImmediateValue *Value::asImm() const
{
   if (reg.file == FILE_IMMEDIATE)
      return static_cast<const ImmediateValue *>(this);
   return NULL;
}

Value *Value::get(Iterator &it)
{
   return reinterpret_cast<Value *>(it.get());
}

bool BasicBlock::reachableBy(BasicBlock *by, BasicBlock *term)
{
   return cfg.reachableBy(&by->cfg, &term->cfg);
}

BasicBlock *BasicBlock::get(Iterator &iter)
{
   return reinterpret_cast<BasicBlock *>(iter.get());
}

BasicBlock *BasicBlock::get(Graph::Node *node)
{
   assert(node);
   return reinterpret_cast<BasicBlock *>(node->data);
}

LValue *Function::getLValue(int id)
{
   assert((unsigned int)id < (unsigned int)allLValues.getSize());
   return reinterpret_cast<LValue *>(allLValues.get(id));
}

#endif // __NV50_IR_INLINES_H__