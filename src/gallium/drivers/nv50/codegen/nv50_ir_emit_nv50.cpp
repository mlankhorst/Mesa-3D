
#include "nv50_ir.h"
#include "nv50_ir_target.h"

namespace nv50_ir {

void
CodeEmitter::setCodeLocation(void *ptr, uint32_t size)
{
   code = reinterpret_cast<uint32_t *>(ptr);
   codeSize = 0;
   maxCodeSize = size;
}

void
CodeEmitter::printBinary() const
{
   uint32_t *bin = code - codeSize / 4;
   debug_printf("program binary (%u bytes)", codeSize);
   for (unsigned int pos = 0; pos < codeSize / 4; ++pos) {
      if ((pos % 8) == 0)
         debug_printf("\n");
      debug_printf("%08x ", bin[pos]);
   }
   debug_printf("\n");
}

class CodeEmitterNV50 : public CodeEmitter
{
public:
   CodeEmitterNV50(const Target *);

   virtual bool emitInstruction(Instruction *);
   virtual uint32_t getMinEncodingSize(const Instruction *) const;

   inline void setProgramType(Program::Type pType) { progType = pType; }

private:
   const Target *targ;

   Program::Type progType;

private:
   void emitForm_A(const Instruction *, uint64_t);
   void emitForm_B(const Instruction *, uint64_t);
   void emitForm_S(const Instruction *, uint32_t, bool pred);

   void emitPredicate(const Instruction *);

   void setAddress16(const ValueRef&);
   void setImmediate(const Instruction *, const int s);
   void setImmediateS8(const ValueRef&);

   void emitCondCode(CondCode cc, int pos);
   void emitInterpMode(const Instruction *);
   void emitLoadStoreType(DataType ty);
   void emitCachingMode(CacheMode c);

   void emitShortSrc2(const ValueRef&);

   inline uint8_t getSRegEncoding(const ValueRef&);

   void roundMode_A(const Instruction *);
   void roundMode_C(const Instruction *);
   void roundMode_CS(const Instruction *);

   void emitNegAbs12(const Instruction *);

   void emitLOAD(const Instruction *);
   void emitSTORE(const Instruction *);
   void emitMOV(const Instruction *);

   void emitINTERP(const Instruction *);
   void emitPFETCH(const Instruction *);
   void emitVFETCH(const Instruction *);
   void emitOUT(const Instruction *);

   void emitUADD(const Instruction *);
   void emitFADD(const Instruction *);
   void emitUMUL(const Instruction *);
   void emitFMUL(const Instruction *);
   void emitFMAD(const Instruction *);

   void emitLogicOp(const Instruction *, uint8_t subOp);
   void emitShift(const Instruction *);

   void emitSFnOp(const Instruction *, uint8_t subOp);

   void emitCVT(Instruction *);
   void emitMINMAX(const Instruction *);
   void emitPreOp(const Instruction *);

   void emitSET(const CmpInstruction *);

   void emitTEX(const TexInstruction *);
   void emitTEXCSAA(const TexInstruction *);
   void emitTXQ(const TexInstruction *);

   void emitQUADOP(const QuadInstruction *);

   void emitFlow(const FlowInstruction *);

   inline void defId(const ValueDef&, const int pos);
   inline void srcId(const ValueRef&, const int pos);

   inline void srcAddr32(const ValueRef&, const int pos); // address / 4

   inline void srcId(const ValueRef *, const int pos);

   inline bool isLIMM(const ValueRef&);
};

// for better visibility
#define HEX64(h, l) 0x##h##l##ULL

#define SDATA(a) ((a).rep()->reg.data)
#define DDATA(a) ((a).rep()->reg.data)

void CodeEmitterNV50::srcId(const ValueRef& src, const int pos)
{
   assert(src.get());
   code[pos / 32] |= SDATA(src).id << (pos % 32);
}

void CodeEmitterNV50::srcId(const ValueRef *src, const int pos)
{
   assert(src->get());
   code[pos / 32] |= SDATA(*src).id << (pos % 32);
}

void CodeEmitterNV50::srcAddr32(const ValueRef& src, const int pos)
{
   assert(src.get());
   code[pos / 32] |= (SDATA(src).offset >> 2) << (pos % 32);
}

void CodeEmitterNV50::defId(const ValueDef& def, const int pos)
{
   assert(def.get());
   code[pos / 32] |= DDATA(def).id << (pos % 32);
}

void
CodeEmitterNV50::roundMode_MAD(const Instruction *insn)
{
   switch (insn->rnd) {
   case ROUND_M: code[1] |= 1 << 22; break;
   case ROUND_P: code[1] |= 2 << 22; break;
   case ROUND_Z: code[1] |= 3 << 22; break;
   default:
      assert(insn->rnd == ROUND_N);
      break;
   }
}

void
CodeEmitterNV50::emitMNeg12(const Instruction *i)
{
   code[1] |= i->src[0].mod.neg() << 26;
   code[1] |= i->src[1].mod.neg() << 27;
}

void CodeEmitterNV50::emitCondCode(CondCode cc, int pos)
{
   uint8_t val;

   assert(pos >= 32 || pos <= 27);

   switch (cc) {
   case CC_LT:  val = 0x1; break;
   case CC_LTU: val = 0x9; break;
   case CC_EQ:  val = 0x2; break;
   case CC_EQU: val = 0xa; break;
   case CC_LE:  val = 0x3; break;
   case CC_LEU: val = 0xb; break;
   case CC_GT:  val = 0x4; break;
   case CC_GTU: val = 0xc; break;
   case CC_NE:  val = 0x5; break;
   case CC_NEU: val = 0xd; break;
   case CC_GE:  val = 0x6; break;
   case CC_GEU: val = 0xe; break;
   case CC_TR:  val = 0xf; break;
   case CC_FL:  val = 0x0; break;

   case CC_O:  val = 0x10; break;
   case CC_C:  val = 0x11; break;
   case CC_A:  val = 0x12; break;
   case CC_S:  val = 0x13; break;
   case CC_NS: val = 0x1c; break;
   case CC_NA: val = 0x1d; break;
   case CC_NC: val = 0x1e; break;
   case CC_NO: val = 0x1f; break;

   default:
      val = 0;
      assert(!"invalid condition code");
      break;
   }
   code[pos / 32] |= val << (pos % 32);
}

void
CodeEmitterNV50::emitPredicate(const Instruction *i)
{
   assert(!(code[1] & 0x00003f80));
   
   if (i->flagsSrc >= 0) {
      assert(i->getSrc(i->flagsSrc)->reg.file == FILE_FLAGS);
      emitCondCode(i->cc, 32 + 7);
      srcId(i->src[i->predSrc], 32 + 12);
   } else {
      code[1] |= 0x0780;
   }
}

void
CodeEmitterNV50::emitFlagsWr(const Instruction *i)
{
   assert(!(code[1] & 0x70));

   if (i->flagsDef >= 0)
      code[1] |= (DDATA(i->def[i->flagsDef]).id << 4) | 0x40;
}

void
CodeEmitterNV50::setAReg16(const Instruction *i)
{
   if (1) { // FIXME
      const uint id = SDATA(i->src[0]).id + 1;

      assert(id <= 4);
      code[0] |= (id & 3) << 26;
      code[1] |= id & 4;
   }
}

void
CodeEmitterNV50::setImmediate(const Instruction *i, const int s)
{
   const ImmediateValue *imm = i->src[s].get()->asImm();

   assert(imm);

   code[1] |= 3;
   code[0] |= (imm->reg.data.u32 & 0x3f) << 16;
   code[1] |= (imm->reg.data.u32 >> 6) << 2;
}

void
CodeEmitterNV50::setDst(const Value *dst)
{
   const Storage *reg = &dst->join->reg;

   assert(reg->file != FILE_ADDRESS);

   if (reg->data.id < 0) {
      code[0] |= (127 << 2) | 1;
      code[1] |= 8;
   } else {
      if (reg.file == FILE_SHADER_OUTPUT)
         code[1] |= 8;
      code[0] |= reg->data.id << 2;
   }
}

void
CodeEmitterNV50::setDst(const Instruction *i, int d)
{
   if (i->defExists(d)) {
      setDst(i->def[d].get());
   } else {
      code[0] |= 0x01fc;
      code[1] |= 0x0008;
   }
}

void
CodeEmitterNV50::setSrc0(const ValueRef &ref)
{
   const Storage *reg = &ref.rep()->reg;

   if (reg->file == FILE_SHADER_INPUT)
      code[1] |= 0x00200000;
   else
   if (ref.getUniqueInsn()->op == OP_PFETCH)
      code[1] |= 0x01800000;
   else
   if (reg->file != FILE_GPR)
      ERROR("invalid src0 register file: %d\n", reg->file);

   assert(reg->data.id < 128);
   code[0] |= reg->data.id << 9;
}

void
CodeEmitterNV50::setSrc1(const ValueRef &ref)
{
   const Storage *reg = &ref.rep()->reg;

   if (reg->file == FILE_MEMORY_CONST) {
      assert(!(code[1] & 0x01800000));
      code[0] |= 1 << 23;
      code[1] |= reg->fileIndex << 22;
   } else
   if (reg->file != FILE_GPR) {
      ERROR("invalid src1 register file: %d\n", reg->file);
   }

   assert(reg->data.id < 128);
   code[0] |= reg->data.id << 16;
}

void
CodeEmitterNV50::setSrc2(const ValueRef &ref)
{
   const Storage *reg = &ref.rep()->reg;

   if (reg->file == FILE_MEMORY_CONST) {
      assert(!(code[1] & 0x01800000));
      code[0] |= 1 << 24;
      code[1] |= reg->fileIndex << 22;
   } else
   if (reg->file != FILE_GPR) {
      ERROR("invalid src1 register file: %d\n", reg->file);
   }

   assert(reg->data.id < 128);
   code[1] |= reg->data.id << 14;
}

// the default form:
//  - long instruction
//  - 1 to 3 sources in slots 0, 1, 2
//  - address & flags
void
CodeEmitterNV50::emitForm_MAD(const Instruction *i, uint64_t opc)
{
   code[0] |= 1;

   emitPredicate(i);
   emitFlagsWr(i);

   setDst(i, 0);

   if (i->srcExists(0))
      setSrc0(i->src[0]);

   if (i->srcExists(1))
      setSrc1(i->src[1]);

   if (i->srcExists(2))
      setSrc2(i->src[2]);

   setAReg16(i);
}



void
CodeEmitterNV50::emitShortSrc2(const ValueRef &src)
{
   if (src.getFile() == FILE_MEMORY_CONST) {
      switch (src.get()->reg.fileIndex) {
      case 0:  code[0] |= 0x100; break;
      case 1:  code[0] |= 0x200; break;
      case 16: code[0] |= 0x300; break;
      default:
         assert(!"unsupported file index for short op");
         break;
      }
      srcAddr32(src, 20);
   } else {
      srcId(src, 20);
      assert(src.getFile() == FILE_GPR);
   }
}

void
CodeEmitterNV50::emitFMAD(const Instruction *i)
{
   bool neg1 = (i->src[0].mod ^ i->src[1].mod).neg();

   if (i->encSize == 8) {
      if (isLIMM(i->src[1])) {
         emitForm_A(i, HEX64(20000000, 00000002));
      } else {
         emitForm_A(i, HEX64(30000000, 00000000));

         if (i->src[2].mod.neg())
            code[0] |= 1 << 8;
      }
      roundMode_A(i);

      if (neg1)
         code[0] |= 1 << 9;

      if (i->saturate)
         code[0] |= 1 << 5;
      if (i->ftz)
         code[0] |= 1 << 6;
   } else {
      assert(!i->saturate && !i->src[2].mod.neg());
      emitForm_S(i, (i->src[2].getFile() == FILE_MEMORY_CONST) ? 0x2e : 0x0e,
                 false);
      if (neg1)
         code[0] |= 1 << 4;
   }
}

void
CodeEmitterNV50::emitFMUL(const Instruction *i)
{
   bool neg = (i->src[0].mod ^ i->src[1].mod).neg();

   assert(i->postFactor >= -3 && i->postFactor <= 3);

   if (i->encSize == 8) {
      if (isLIMM(i->src[1])) {
         assert(i->postFactor == 0); // constant folded, hopefully
         emitForm_A(i, HEX64(30000000, 00000002));
      } else {
         emitForm_A(i, HEX64(58000000, 00000000));
         roundMode_A(i);

         code[1] |=
            ((i->postFactor >= 0) ? i->postFactor : (3 - i->postFactor)) << 17;
      }
      if (neg)
         code[1] ^= 1 << 25; // aliases with LIMM sign bit

      if (i->saturate)
         code[0] |= 1 << 5;
      if (i->ftz)
         code[0] |= 1 << 6;
   } else {
      assert(!neg && !i->saturate && !i->ftz && !i->postFactor);
      emitForm_S(i, 0xa8, true);
   }
}

void
CodeEmitterNV50::emitUMUL(const Instruction *i)
{
   if (i->encSize == 8) {
      if (i->src[1].getFile() == FILE_IMMEDIATE) {
         emitForm_A(i, HEX64(10000000, 00000002));
      } else {
         emitForm_A(i, HEX64(50000000, 00000003));
      }
      if (i->sType == TYPE_S32)
         code[0] |= 1 << 5;
      if (i->dType == TYPE_S32)
         code[0] |= 1 << 7;
   } else {
      emitForm_S(i, i->src[1].getFile() == FILE_IMMEDIATE ? 0xaa : 0x2a, true);

      if (i->sType == TYPE_S32)
         code[0] |= 1 << 6;
   }
}

void
CodeEmitterNV50::emitFADD(const Instruction *i)
{
   if (i->encSize == 8) {
      if (isLIMM(i->src[1])) {
         emitForm_A(i, HEX64(28000000, 00000002));

         assert(!i->src[1].mod.neg() && !i->src[1].mod.abs() && !i->saturate);
      } else {
         emitForm_A(i, HEX64(50000000, 00000000));

         roundMode_A(i);
         if (i->saturate)
            code[1] |= 1 << 17;
      }
      emitNegAbs12(i);

      if (i->ftz)
         code[0] |= 1 << 5;
   } else {
      assert(!i->saturate &&
             !i->src[0].mod.abs() &&
             !i->src[1].mod.neg() && !i->src[1].mod.abs());

      emitForm_S(i, 0x49, true);

      if (i->src[0].mod.neg())
         code[0] |= 1 << 7;
   }
}

void
CodeEmitterNV50::emitUADD(const Instruction *i)
{
   uint32_t addOp = 0;

   assert(!i->src[0].mod.abs() && !i->src[1].mod.abs());

   if (i->src[0].mod.neg())
      addOp |= 0x200;
   if (i->src[1].mod.neg())
      addOp |= 0x100;

   if (i->encSize == 8) {
      if (isLIMM(i->src[1])) {
         emitForm_A(i, HEX64(08000000, 00000002));
         if (i->def[1].exists())
            code[1] |= 1 << 26; // write carry
      } else {
         emitForm_A(i, HEX64(48000000, 00000003));
         if (i->def[1].exists())
            code[1] |= 1 << 16; // write carry
      }
      code[0] |= addOp;

      if (i->saturate)
         code[0] |= 1 << 5;
      if (i->src[2].exists()) // add carry
         code[0] |= 1 << 6;
   } else {
      assert(!(addOp & 0x100));
      emitForm_S(i, (addOp >> 3) |
                 ((i->src[1].getFile() == FILE_IMMEDIATE) ? 0xac : 0x2c), true);
   }
}

void
CodeEmitterNV50::emitLogicOp(const Instruction *i, uint8_t subOp)
{
   if (i->encSize == 8) {
      if (isLIMM(i->src[1])) {
         emitForm_A(i, HEX64(38000000, 00000002));

         if (i->src[2].exists())
            code[1] |= 1 << 26;
      } else {
         emitForm_A(i, HEX64(68000000, 00000003));

         if (i->src[2].exists())
            code[1] |= 1 << 16;
      }
      code[0] |= subOp << 6;

      if (i->src[2].exists()) // carry
         code[0] |= 1 << 5;

      if (i->src[0].mod & Modifier(NV50_IR_MOD_NOT)) code[0] |= 1 << 9;
      if (i->src[1].mod & Modifier(NV50_IR_MOD_NOT)) code[0] |= 1 << 8;
   } else {
      emitForm_S(i, (subOp << 5) |
                 ((i->src[1].getFile() == FILE_IMMEDIATE) ? 0x1d : 0x8d), true);
   }
}

void
CodeEmitterNV50::emitPOPC(const Instruction *i)
{
   emitForm_A(i, HEX64(54000000, 00000004));

   if (i->src[0].mod & Modifier(NV50_IR_MOD_NOT)) code[0] |= 1 << 9;
   if (i->src[1].mod & Modifier(NV50_IR_MOD_NOT)) code[0] |= 1 << 8;
}

void
CodeEmitterNV50::emitINSBF(const Instruction *i)
{
   emitForm_A(i, HEX64(28000000, 30000000));
}

void
CodeEmitterNV50::emitShift(const Instruction *i)
{
   if (i->op == OP_SHR) {
      emitForm_A(i, HEX64(58000000, 00000003)
                 | (isSignedType(i->dType) ? 0x20 : 0x00));
   } else {
      emitForm_A(i, HEX64(60000000, 00000003));
   }

   if (0)
      code[0] |= 1 << 9; // clamp shift amount
}

void
CodeEmitterNV50::emitPreOp(const Instruction *i)
{
   if (i->encSize == 8) {
      emitForm_A(i, HEX64(60000000, 00000000));

      if (i->op == OP_PREEX2)
         code[0] |= 0x20;

      if (i->src[0].mod.abs()) code[0] |= 1 << 6;
      if (i->src[0].mod.neg()) code[0] |= 1 << 8;
   } else {
      emitForm_S(i, i->op == OP_PREEX2 ? 0x74000008 : 0x70000008, true);
   }
}

void
CodeEmitterNV50::emitSFnOp(const Instruction *i, uint8_t subOp)
{
   if (i->encSize == 8) {
      code[0] = 0x00000000 | (subOp << 26);
      code[1] = 0xc8000000;

      emitPredicate(i);

      defId(i->def[0], 14);
      srcId(i->src[0], 20);

      assert(i->src[0].getFile() == FILE_GPR);

      if (i->saturate) code[0] |= 1 << 5;

      if (i->src[0].mod.abs()) code[0] |= 1 << 7;
      if (i->src[0].mod.neg()) code[0] |= 1 << 9;
   } else {
      emitForm_S(i, 0x80000008 | (subOp << 26), true);

      assert(!i->src[0].mod.neg());
      if (i->src[0].mod.abs()) code[0] |= 1 << 30;
   }
}

void
CodeEmitterNV50::emitMINMAX(const Instruction *i)
{
   uint64_t op;

   assert(i->encSize == 8);

   op = (i->op == OP_MIN) ? 0x080e000000000000ULL : 0x081e000000000000ULL;

   emitForm_A(i, op);

   if (i->ftz)
      code[0] |= 1 << 5;
   else
   if (!isFloatType(i->dType))
      code[0] |= isSignedType(i->dType) ? 0x23 : 0x03;

   emitNegAbs12(i);
}

void
CodeEmitterNV50::roundMode_C(const Instruction *i)
{
   switch (i->rnd) {
   case ROUND_M:  code[1] |= 1 << 17; break;
   case ROUND_P:  code[1] |= 2 << 17; break;
   case ROUND_Z:  code[1] |= 3 << 17; break;
   case ROUND_NI: code[0] |= 1 << 7; break;
   case ROUND_MI: code[0] |= 1 << 7; code[1] |= 1 << 17; break;
   case ROUND_PI: code[0] |= 1 << 7; code[1] |= 2 << 17; break;
   case ROUND_ZI: code[0] |= 1 << 7; code[1] |= 3 << 17; break;
   default:
      assert(!"invalid round mode");
      break;
   }
}

void
CodeEmitterNV50::roundMode_CS(const Instruction *i)
{
   switch (i->rnd) {
   case ROUND_M:
   case ROUND_MI: code[0] |= 1 << 16; break;
   case ROUND_P:
   case ROUND_PI: code[0] |= 2 << 16; break;
   case ROUND_Z:
   case ROUND_ZI: code[0] |= 3 << 16; break;
   default:
      break;
   }
}

void
CodeEmitterNV50::emitCVT(Instruction *i)
{
   const bool f2f = isFloatType(i->dType) && isFloatType(i->sType);

   switch (i->op) {
   case OP_CEIL:  i->rnd = f2f ? ROUND_PI : ROUND_P; break;
   case OP_FLOOR: i->rnd = f2f ? ROUND_MI : ROUND_M; break;
   case OP_TRUNC: i->rnd = f2f ? ROUND_ZI : ROUND_Z; break;
   default:
      break;
   }

   const bool sat = (i->op == OP_SAT) || i->saturate;
   const bool abs = (i->op == OP_ABS) || i->src[0].mod.abs();
   const bool neg = (i->op == OP_NEG) || i->src[0].mod.neg();

   if (i->encSize == 8) {
      emitForm_B(i, HEX64(10000000, 00000004));

      roundMode_C(i);

      code[0] |= util_logbase2(i->def[0].getSize()) << 20;
      code[0] |= util_logbase2(i->src[0].getSize()) << 23;

      if (sat)
         code[0] |= 0x20;
      if (abs)
         code[0] |= 1 << 6;
      if (neg && i->op != OP_ABS)
         code[0] |= 1 << 8;

      if (i->ftz)
         code[1] |= 1 << 23;

      if (isSignedIntType(i->dType))
         code[0] |= 0x080;
      if (isSignedIntType(i->sType))
         code[0] |= 0x200;

      if (isFloatType(i->dType)) {
         if (!isFloatType(i->sType))
            code[1] |= 0x08000000;
      } else {
         if (isFloatType(i->sType))
            code[1] |= 0x04000000;
         else
            code[1] |= 0x0c000000;
      }
   } else {
      if (i->op == OP_CEIL || i->op == OP_FLOOR || i->op == OP_TRUNC) {
         code[0] = 0x298;
      } else
      if (isFloatType(i->dType)) {
         if (isFloatType(i->sType))
            code[0] = 0x098;
         else
            code[0] = 0x088 | (isSignedType(i->sType) ? (1 << 8) : 0);
      } else {
         assert(isFloatType(i->sType));

         code[0] = 0x288 | (isSignedType(i->sType) ? (1 << 8) : 0);
      }

      if (neg) code[0] |= 1 << 16;
      if (sat) code[0] |= 1 << 18;
      if (abs) code[0] |= 1 << 19;

      roundMode_CS(i);
   }
}

void
CodeEmitterNV50::emitSET(const CmpInstruction *i)
{
   uint32_t opc;

   switch (i->op) {
   case OP_SET_AND: opc = 0x10000000; break;
   case OP_SET_OR:  opc = 0x10200000; break;
   case OP_SET_XOR: opc = 0x10400000; break;
   default:
      opc = 0x100e0000;
      break;
   }
   emitForm_A(i, static_cast<uint64_t>(opc) << 32);

   if (isFloatType(i->dType) || isSignedIntType(i->sType))
      code[0] |= 0x20;

   if (i->def[0].getFile() == FILE_PREDICATE)
      code[1] += 0x08000000;

   emitCondCode(i->setCond, 32 + 23);
   emitNegAbs12(i);
}

void
CodeEmitterNV50::emitSLCT(const CmpInstruction *i)
{
   emitForm_A(i, HEX64(30000000, 00000000));

   switch (i->dType) {
   case TYPE_S32:
      code[0] |= 0x23;
      break;
   case TYPE_U32:
      code[0] |= 0x03;
      break;
   case TYPE_F32:
      code[1] |= 0x08000000;
      break;
   default:
      assert(!"invalid type for SLCT");
      break;
   }
   CondCode cc = i->setCond;

   if (i->src[2].mod.neg())
      cc = reverseCondCode(cc);

   emitCondCode(cc, 32 + 23);

   if (i->ftz)
      code[0] |= 1 << 5;
}

void CodeEmitterNV50::emitSELP(const Instruction *i)
{
   emitForm_A(i, HEX64(20000000, 00000004));

   if (i->cc == CC_NOT_P || i->src[2].mod & Modifier(NV50_IR_MOD_NOT))
      code[1] |= 1 << 20;
}

void CodeEmitterNV50::emitTEXCSAA(const TexInstruction *i)
{
   code[0] = 0x00000086;
   code[1] = 0xd0000000;

   code[1] |= i->texR;
   code[1] |= i->texS << 8;

   if (i->liveOnly)
      code[0] |= 1 << 9;

   defId(i->def[0], 14);
   srcId(i->src[0], 20);
}

void
CodeEmitterNV50::emitTEX(const TexInstruction *i)
{
   code[0] = 0x00000006;

   if (1)
      code[0] |= 0x80; // normal/t/p mode = t, XXX: what is this ?

   if (i->liveOnly)
      code[0] |= 1 << 9;

   switch (i->op) {
   case OP_TEX: code[1] = 0x80000000; break;
   case OP_TXB: code[1] = 0x84000000; break;
   case OP_TXL: code[1] = 0x86000000; break;
   case OP_TXF: code[1] = 0x92000000; break;
   case OP_TXG: code[1] = 0xa0000000; break;
   case OP_TXD: code[1] = 0xe0000000; break;
   default:
      assert(!"invalid texture op");
      break;
   }
   defId(i->def[0], 14);
   srcId(i->src[0], 20);

   emitPredicate(i);

   if (i->op == OP_TXG) code[0] |= i->txgComp << 5;

   code[1] |= i->texMask << 14;

   code[1] |= i->texR;
   code[1] |= i->texS << 8;
   if (i->indrTexR >= 0 || i->indrTexS >= 0)
      code[1] |= 1 << 18; // in 1st source (with array index)

   // texture target:
   code[1] |= (i->texTarget.getDim() - 1) << 20;
   if (i->texTarget.isCube())
      code[1] += 1 << 20;
   if (i->texTarget.isArray())
      code[1] |= 1 << 19;
   if (i->texTarget.isShadow())
      code[1] |= 1 << 24;

   int src1 = i->texTarget.getArgCount() - (i->texTarget.isShadow() ? 1 : 0);

   if (i->src[src1].getFile() == FILE_IMMEDIATE) { // lzero
      if (i->op == OP_TXL)
         code[1] &= ~(1 << 26);
      else
      if (i->op == OP_TXF)
         code[1] &= ~(1 << 25);
   }
   if (i->texTarget == TEX_TARGET_2D_MS ||
       i->texTarget == TEX_TARGET_2D_MS_ARRAY)
      code[1] |= 1 << 23;

   if (i->texOffsetEn) // in vecSrc0.w
      code[1] |= 1 << 22;

   srcId(i->src[src1], 26);
}

void
CodeEmitterNV50::emitTXQ(const TexInstruction *i)
{
   code[0] = 0x00000086;
   code[1] = 0xc0000000;

   switch (i->texQuery) {
   case TXQ_DIMS:            code[1] |= 0 << 22; break;
   case TXQ_TYPE:            code[1] |= 1 << 22; break;
   case TXQ_SAMPLE_POSITION: code[1] |= 2 << 22; break;
   case TXQ_FILTER:          code[1] |= 3 << 22; break;
   case TXQ_LOD:             code[1] |= 4 << 22; break;
   case TXQ_BORDER_COLOUR:   code[1] |= 5 << 22; break;
   default:
      assert(!"invalid texture query");
      break;
   }

   code[1] |= i->texR;
   code[1] |= i->texS << 8;
   if (i->indrTexR || i->indrTexS)
      code[1] |= 1 << 18;

   defId(i->def[0], 14);
   srcId(i->src[0], 20);
   srcId(i->src[1], 26);
}

void
CodeEmitterNV50::emitQUADOP(const QuadInstruction *i)
{
   code[0] = 0x00000000 | (i->laneMask << 6);
   code[1] = 0x48000000 | i->quadop;

   defId(i->def[0], 14);
   srcId(i->src[0], 20);
   srcId(i->src[1].exists() ? i->src[1] : i->src[0], 26);

   emitPredicate(i);
}

void
CodeEmitterNV50::emitFlow(const FlowInstruction *i)
{
   code[0] = 0x00000007;

   switch (i->op) {
   case OP_BRA:
      code[1] = i->absolute ? 0x00000000 : 0x40000000;
      if (i->src[0].getFile() == FILE_MEMORY_CONST ||
          i->src[1].getFile() == FILE_MEMORY_CONST)
         code[1] |= 0x4000;
      break;
   case OP_CALL:
      code[1] = i->absolute ? 0x10000000 : 0x50000000;
      if (i->src[0].getFile() == FILE_MEMORY_CONST)
         code[1] |= 0x4000;
      break;

   case OP_EXIT:  code[1] = 0x80000000; break;
   case OP_RET:   code[1] = 0x90000000; break;
   case OP_KIL:   code[1] = 0x98000000; break;
   case OP_BREAK: code[1] = 0xa8000000; break;
   case OP_CONT:  code[1] = 0xb0000000; break;

   case OP_JOINAT:    code[1] = 0x60000000; break;
   case OP_BREAKADDR: code[1] = 0x68000000; break;
   case OP_CONTADDR:  code[1] = 0x70000000; break;

   case OP_QUADON:  code[1] = 0xc0000000; break;
   case OP_QUADPOP: code[1] = 0xc8000000; break;
   case OP_BRKPT:   code[1] = 0xd0000000; break;
   default:
      assert(!"invalid flow operation");
      return;
   }

   if (i->flagsSrc < 0)
      code[0] |= 0x1e0;

   emitPredicate(i);

   if (i->allWarp)
      code[0] |= 1 << 15;
   if (i->limit)
      code[0] |= 1 << 16;

   if (i->target) {
      int32_t pcRel = i->target->binPos - codeSize;

      assert(!i->absolute);

      code[0] |= (pcRel & 0x3f) << 26;
      code[1] |= (pcRel >> 6) & 0x3ffff;
   } else {
      // XXX: absolute branches
   }
}

void
CodeEmitterNV50::emitPFETCH(const Instruction *i)
{
   uint32_t prim = i->src[0].get()->reg.data.u32;

   code[0] = 0x00000006 | ((prim & 0x3f) << 20);
   code[1] = 0x00000000 | (prim >> 6);

   emitPredicate(i);

   defId(i->def[0], 14);
   srcId(i->src[1], 20);
}

void
CodeEmitterNV50::emitVFETCH(const Instruction *i)
{
   code[0] = 0x00000006;
   code[1] = 0x06000000 | i->src[0].get()->reg.data.offset;

   if (i->perPatch)
      code[0] |= 0x100;

   emitPredicate(i);

   code[0] |= (i->defCount(0xf) - 1) << 5;

   defId(i->def[0], 14);
   srcId(i->src[0].getIndirect(), 26);

   if (progType != Program::TYPE_VERTEX)
      srcId(i->src[1], 20); // secret vertex address
   else
      code[0] |= 0x3f << 20;
}

void
CodeEmitterNV50::emitEXPORT(const Instruction *i)
{
   code[0] = 0x00000006 | ((i->src[0].get()->reg.size / 4 - 1) << 5);
   code[1] = 0x0a000000 | i->src[0].get()->reg.data.offset;

   if (i->perPatch)
      code[0] |= 0x100;

   emitPredicate(i);

   assert(i->src[1].getFile() == FILE_GPR);

   srcId(i->src[0].getIndirect(), 20);
   srcId(i->src[1], 26);

   if (progType != Program::TYPE_VERTEX) // secret vertex address
      srcId(i->src[2], 32 + 17);
   else
      code[1] |= 0x3f << 17;
}

void
CodeEmitterNV50::emitOUT(const Instruction *i)
{
   code[0] = 0x00000006;
   code[1] = 0x1c000000;

   emitPredicate(i);

   defId(i->def[0], 14); // new secret address
   srcId(i->src[0], 20); // old secret address, should be 0 initially

   if (i->op == OP_EMIT)
      code[0] |= 1 << 5;
   if (i->op == OP_RESTART || i->restart)
      code[0] |= 1 << 6;

   // vertex stream
   if (i->src[1].getFile() == FILE_IMMEDIATE) {
      code[1] |= 0xc000;
      code[0] |= SDATA(i->src[0]).u32 << 26;
   } else {
      srcId(i->src[1], 26);
   }
}

void
CodeEmitterNV50::emitInterpMode(const Instruction *i)
{
   if (i->encSize == 8) {
      code[0] |= i->interpMode << 6;
   } else {
      if (i->getInterpMode() == NV50_IR_INTERP_SC)
         code[0] |= 0x80;
      assert(i->op == OP_PINTERP && i->getSampleMode() == 0);
   }
}

void
CodeEmitterNV50::emitINTERP(const Instruction *i)
{
   const uint32_t base = i->getSrc(0)->reg.data.offset;

   if (i->encSize == 8) {
      code[0] = 0x00000000;
      code[1] = 0xc0000000 | (base & 0xffff);

      if (i->saturate)
         code[0] |= 1 << 5;

      if (i->op == OP_PINTERP)
         srcId(i->src[1], 26);
      else
         code[0] |= 0x3f << 26;
   } else {
      assert(i->op == OP_PINTERP);
      code[0] = 0x00000009 | ((base & 0xc) << 6) | ((base >> 4) << 20);
      srcId(i->src[1], 20);
   }
   emitInterpMode(i);

   emitPredicate(i);

   defId(i->def[0], 14);
   srcId(i->src[0].getIndirect(), 20);

   if (i->getSampleMode() == NV50_IR_INTERP_OFFSET)
      srcId(i->src[i->op == OP_PINTERP ? 2 : 1], 17);
}

void
CodeEmitterNV50::emitLoadStoreType(DataType ty)
{
   uint8_t val;

   switch (ty) {
   case TYPE_U8:
      val = 0x00;
      break;
   case TYPE_S8:
      val = 0x20;
      break;
   case TYPE_F16:
   case TYPE_U16:
      val = 0x40;
      break;
   case TYPE_S16:
      val = 0x60;
      break;
   case TYPE_F32:
   case TYPE_U32:
   case TYPE_S32:
      val = 0x80;
      break;
   case TYPE_F64:
   case TYPE_U64:
   case TYPE_S64:
      val = 0xa0;
      break;
   case TYPE_B128:
      val = 0xc0;
      break;
   default:
      val = 0x80;
      assert(!"invalid type");
      break;
   }
   code[0] |= val;
}

void
CodeEmitterNV50::emitCachingMode(CacheMode c)
{
   uint32_t val;

   switch (c) {
   case CACHE_CA:
// case CACHE_WB:
      val = 0x000;
      break;
   case CACHE_CG:
      val = 0x100;
      break;
   case CACHE_CS:
      val = 0x200;
      break;
   case CACHE_CV:
// case CACHE_WT:
      val = 0x300;
      break;
   }
   code[0] |= val;
}

void
CodeEmitterNV50::emitSTORE(const Instruction *i)
{
   uint32_t opc;

   switch (i->src[0].getFile()) {
   case FILE_MEMORY_GLOBAL: opc = 0x90000000; break;
   case FILE_MEMORY_LOCAL:  opc = 0xc8000000; break;
   case FILE_MEMORY_SHARED: opc = 0xc9000000; break;
   default:
      assert(!"invalid memory file");
      opc = 0;
      break;
   }
   code[0] = 0x00000005;
   code[1] = opc;

   setAddress16(i->src[0]);
   srcId(i->src[0].getIndirect(), 20);

   emitPredicate(i);

   emitLoadStoreType(i->dType);
   emitCachingMode(i->cache);
}

void
CodeEmitterNV50::emitLOAD(const Instruction *i)
{
   uint32_t opc;

   code[0] = 0x00000005;

   switch (i->src[0].getFile()) {
   case FILE_MEMORY_GLOBAL: opc = 0x80000000; break;
   case FILE_MEMORY_LOCAL:  opc = 0xc0000000; break;
   case FILE_MEMORY_SHARED: opc = 0xc1000000; break;
   case FILE_MEMORY_CONST:
      opc = 0x14000000 | (i->src[0].get()->reg.fileIndex << 10);
      code[0] = 0x00000006;
      break;
   default:
      assert(!"invalid memory file");
      opc = 0;
      break;
   }
   code[1] = opc;

   setAddress16(i->src[0]);
   srcId(i->src[0].getIndirect(), 20);

   emitPredicate(i);

   emitLoadStoreType(i->dType);
   emitCachingMode(i->cache);
}

uint8_t
CodeEmitterNV50::getSRegEncoding(const ValueRef& ref)
{
   switch (SDATA(ref).id) {
   case SV_PHYSID:        return 0x03;
   case SV_VERTEX_COUNT:  return 0x10;
   case SV_INVOCATION_ID: return 0x11;
   case SV_YDIR:          return 0x12;
   case SV_TIDX:          return 0x21;
   case SV_TIDY:          return 0x22;
   case SV_TIDZ:          return 0x23;
   case SV_CTAIDX:        return 0x25;
   case SV_CTAIDY:        return 0x26;
   case SV_CTAIDZ:        return 0x27;
   case SV_NTIDX:         return 0x29;
   case SV_NTIDY:         return 0x2a;
   case SV_NTIDZ:         return 0x2b;
   case SV_NCTAIDX:       return 0x2d;
   case SV_NCTAIDY:       return 0x2e;
   case SV_NCTAIDZ:       return 0x2f;
   case SV_LBASE:         return 0x34;
   case SV_SBASE:         return 0x30;
   case SV_CLOCK:         return 0x50;
   case SV_CLOCK_HIGH:    return 0x51;
   default:
      assert(!"no sreg for system value");
      return 0;
   }
}

void
CodeEmitterNV50::emitMOV(const Instruction *i)
{
   if (i->src[0].getFile() == FILE_SYSTEM_VALUE) {
      uint8_t sr = getSRegEncoding(i->src[0]);

      if (i->encSize == 8) {
         code[0] = 0x00000004 | (sr << 26);
         code[1] = 0x2c000000;
      } else {
         code[0] = 0x40000008 | (sr << 20);
      }
      defId(i->def[0], 14);

      emitPredicate(i);
   } else
   if (i->encSize == 8) {
      uint64_t opc;

      if (i->src[0].getFile() == FILE_IMMEDIATE)
         opc = HEX64(18000000, 000001e2);
      else
      if (i->src[0].getFile() == FILE_PREDICATE)
         opc = HEX64(080e0000, 1c000004);
      else
         opc = HEX64(28000000, 00000004);

      opc |= i->lanes << 5;

      emitForm_B(i, opc);
   } else {
      uint32_t imm;

      if (i->src[0].getFile() == FILE_IMMEDIATE) {
         imm = SDATA(i->src[0]).u32;
         if (imm & 0xfff00000) {
            assert(!(imm & 0x000fffff));
            code[0] = 0x00000318 | imm;
         } else {
            code[0] = 0x00000118;
            setImmediateS8(i->src[0]);
         }
      } else {
         code[0] = 0x0028;
         emitShortSrc2(i->src[0]);
      }
      defId(i->def[0], 14);

      emitPredicate(i);
   }
}

bool
CodeEmitterNV50::emitInstruction(Instruction *insn)
{
   if (!insn->encSize) {
      ERROR("skipping unencodable instruction: "); insn->print();
      return false;
   } else
   if (codeSize + insn->encSize > maxCodeSize) {
      ERROR("code emitter output buffer too small\n");
      return false;
   }

   switch (insn->op) {
   case OP_MOV:
      emitMOV(insn);
      break;
   case OP_NOP:
      break;
   case OP_LOAD:
      emitLOAD(insn);
      break;
   case OP_STORE:
      emitSTORE(insn);
      break;
   case OP_LINTERP:
   case OP_PINTERP:
      emitINTERP(insn);
      break;
   case OP_VFETCH:
      emitVFETCH(insn);
      break;
   case OP_EXPORT:
      emitEXPORT(insn);
      break;
   case OP_PFETCH:
      emitPFETCH(insn);
      break;
   case OP_ADD:
      if (isFloatType(insn->dType))
         emitFADD(insn);
      else
         emitUADD(insn);
      break;
   case OP_MUL:
      if (isFloatType(insn->dType))
         emitFMUL(insn);
      else
         emitUMUL(insn);
      break;
   case OP_MAD:
   case OP_FMA:
      emitFMAD(insn);
      break;
      break;
   case OP_AND:
      emitLogicOp(insn, 0);
      break;
   case OP_OR:
      emitLogicOp(insn, 1);
      break;
   case OP_XOR:
      emitLogicOp(insn, 2);
      break;
   case OP_SELP:
      emitSELP(insn);
      break;
   case OP_SLCT:
      emitSLCT(insn->asCmp());
      break;
   case OP_MIN:
   case OP_MAX:
      emitMINMAX(insn);
      break;
   case OP_CEIL:
   case OP_FLOOR:
   case OP_TRUNC:
   case OP_CVT:
      emitCVT(insn);
      break;
   case OP_RSQ:
      emitSFnOp(insn, 5);
      break;
   case OP_RCP:
      emitSFnOp(insn, 4);
      break;
   case OP_LG2:
      emitSFnOp(insn, 3);
      break;
   case OP_EX2:
      emitSFnOp(insn, 2);
      break;
   case OP_SIN:
      emitSFnOp(insn, 1);
      break;
   case OP_COS:
      emitSFnOp(insn, 0);
      break;
   case OP_PRESIN:
   case OP_PREEX2:
      emitPreOp(insn);
      break;
   case OP_TEX:
   case OP_TXB:
   case OP_TXL:
   case OP_TXD:
      emitTEX(insn->asTex());
      break;
   case OP_BRA:
   case OP_CALL:
   case OP_RET:
   case OP_EXIT:
   case OP_CONT:
   case OP_BREAK:
   case OP_CONTADDR:
   case OP_BREAKADDR:
   case OP_JOINAT:
   case OP_BRKPT:
   case OP_QUADON:
   case OP_QUADPOP:
      emitFlow(insn->asFlow());
      break;
   case OP_QUADOP:
   case OP_DFDX:
   case OP_DFDY:
      emitQUADOP(insn->asQuadop());
      break;
   case OP_POPCNT:
      emitPOPC(insn);
      break;
   case OP_PHI:
   case OP_UNION:
   case OP_CONSTRAINT:
      ERROR("operation should have been eliminated");
      return false;
   case OP_SUB:
   case OP_EXP:
   case OP_LOG:
   case OP_SQRT:
   case OP_POW:
      ERROR("operation should have been lowered\n");
      return false;
   default:
      ERROR("unknow op\n");
      return false;
   }

   if (insn->join) {
      code[0] |= 0x10;
      assert(insn->encSize == 8);
   }

   code += insn->encSize / 4;
   codeSize += insn->encSize;
   return true;
}

uint32_t
CodeEmitterNV50::getMinEncodingSize(const Instruction *i) const
{
   const Target::OpInfo &info = targ->getInfo(i);

   if (info.minEncSize == 8)
      return 8;

   if (i->ftz || i->saturate || i->join)
      return 8;
   if (i->rnd != ROUND_N)
      return 8;
   if (i->predSrc >= 0 && i->op == OP_MAD)
      return 8;

   for (int s = 0; i->srcExists(s); ++s) {
      if (i->src[s].isIndirect())
         return 8;

      if (i->src[s].getFile() == FILE_MEMORY_CONST) {
         if (SDATA(i->src[s]).offset >= 0x100)
            return 8;
         if (i->getSrc(s)->reg.fileIndex > 1 &&
             i->getSrc(s)->reg.fileIndex != 16)
             return 8;
      } else
      if (i->src[s].getFile() == FILE_IMMEDIATE) {
         if (i->dType == TYPE_F32) {
            if (SDATA(i->src[s]).u32 >= 0x100)
               return 8;
         } else {
            if (SDATA(i->src[s]).u32 > 0xff)
               return 8;
         }
      }

      if (i->op == OP_CVT)
         continue;
      if (i->src[s].mod != Modifier(0)) {
         if (i->src[s].mod == Modifier(NV50_IR_MOD_ABS))
            if (i->op != OP_RSQ)
               return 8;
         if (i->src[s].mod == Modifier(NV50_IR_MOD_NEG))
            if (i->op != OP_ADD || s != 0)
               return 8;
      }
   }

   return 4;
}

CodeEmitterNV50::CodeEmitterNV50(const Target *target) : targ(target)
{
   code = NULL;
   codeSize = maxCodeSize = 0;
}

CodeEmitter *
Target::getCodeEmitter(Program::Type type)
{
   CodeEmitterNV50 *emt = new CodeEmitterNV50(this);
   emt->setProgramType(type);
   return emt;
}

bool
Program::emitBinary(struct nv50_tgsi_shader_info *info)
{
   CodeEmitter *emit = target->getCodeEmitter(progType);

   emit->prepareEmission(this);

   this->print();

   if (!binSize) {
      code = NULL;
      return false;
   }
   code = reinterpret_cast<uint32_t *>(MALLOC(binSize));
   if (!code)
      return false;
   emit->setCodeLocation(code, binSize);

   for (int b = 0; b < bbCount; ++b)
      for (Instruction *i = bbArray[b]->getEntry(); i; i = i->next)
         emit->emitInstruction(i);

   emit->printBinary();

   delete emit;
   return true;
}

} // namespace nv50_ir
