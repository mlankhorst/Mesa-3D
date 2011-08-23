
#include "nv50/codegen/nv50_ir_target.h"

namespace nv50_ir {

class TargetNVC0 : public Target
{
public:
   TargetNVC0(unsigned int chipset);

   virtual CodeEmitter *getCodeEmitter(Program::Type);

   virtual bool runLegalizePass(Program *, CGStage stage) const;

   virtual bool insnCanLoad(const Instruction *insn, int s,
                            const Instruction *ld) const;
   virtual bool isOpSupported(operation, DataType) const;
   virtual bool isModSupported(const Instruction *, int s, Modifier) const;
   virtual bool isSatSupported(const Instruction *) const;
   virtual bool mayPredicate(const Instruction *, const Value *) const;

   virtual unsigned int getFileSize(DataFile) const;
   virtual unsigned int getFileUnit(DataFile) const;

   virtual uint32_t getSVAddress(DataFile shaderFile, const Symbol *sv) const;

private:
   void initOpInfo();
};

Target *getTargetNVC0(unsigned int chipset)
{
   return new TargetNVC0(chipset);
}

TargetNVC0::TargetNVC0(unsigned int card)
{
   chipset = card;
   initOpInfo();
}

/* udiv(n, by)
 * {
 *  if (n < by)
 *    return 0, n
 *
 *  ln = bfind n + 1
 *  lb = bfind b + 1
 *
 *  l = ln - lb
 *
 *  by = shl by, l
 *
 *  q = 0
 *  while (l >= 0) {
 *    q = shl q, 1
 *    if (n >= by) {
 *      n -= by
 *      q |= 1
 *    }
 *    by = shr by, 1
 *    --l
 *  }
 *  rem = n
 * }
 */
// Slow unsigned integer division.
//
// INPUT:   $r0: dividend, $r1: divisor
// OUTPUT:  $r0: result, $r1: modulus
// CLOBBER: $r2 - $r3, $p0 - $p1
const uint32_t lib_nvc0_div_u32[] =
{
   0x0401dc03, 0x1b0e0000,
   0x00008003, 0x78000000,
   0x0400c003, 0x78000000,
   0x0c20c103, 0x48000000,
   0x0c108003, 0x60000000,
   0x00005c28,
   0x00001d18,
   0x0031c023, 0x1b0ec000,
   0xb000a1e7, 0x40000000,
   0x04000003, 0x6000c000,
   0x0813dc03, 0x1b000000,
   0x0420446c,
   0x040004bd,
   0x04208003, 0x5800c000,
   0x0430c103, 0x4800c000,
   0x0ffc5dff,
   0x90001dff
};

// Slow signed integer division.
//
// INPUT:   $r0: dividend, $r1: divisor
// OUTPUT:  $r0: result, $r1: modulus
// CLOBBER: $r2 - $r3, $p0 - $p3
const uint32_t lib_nvc0_div_s32[] =
{
   0xfc05dc23, 0x188e0000,
   0xfc17dc23, 0x18c40000,
   0x03301e18,
   0x07305e18,
   0x0401dc03, 0x1b0e0000,
   0x00008003, 0x78000000,
   0x0400c003, 0x78000000,
   0x0c20c103, 0x48000000,
   0x0c108003, 0x60000000,
   0x00005c28,
   0x00001d18,
   0x0031c023, 0x1b0ec000,
   0xb000a1e7, 0x40000000,
   0x04000003, 0x6000c000,
   0x0813dc03, 0x1b000000,
   0x0420446c,
   0x040004bd,
   0x04208003, 0x5800c000,
   0x0430c103, 0x4800c000,
   0x0ffc5dff,
   0x01700e18,
   0x05704a18,
   0x90001dff
};

// Newton Raphson reciprocal(x): r_{i+1} = r_i * (2.0 - x * r_i)
//
// INPUT:   $r0d (x)
// OUTPUT:  $r0d (rcp(x))
// CLOBBER: $r2 - $r7
const uint32_t lib_nvc0_rcp_f64[] =
{
   0x9810dc08,
   0x00009c28,
   0x4001df18,
   0x00019d18,
   0x08011e01, 0x200c0000,
   0x10209c01, 0x50000000,
   0x08011e01, 0x200c0000,
   0x10209c01, 0x50000000,
   0x08011e01, 0x200c0000,
   0x10201c01, 0x50000000,
   0x00001de7, 0x90000000,
};

// Newton Raphson rsqrt(x): r_{i+1} = r_i * (1.5 - 0.5 * x * r_i * r_i)
//
// INPUT:   $r0d (x)
// OUTPUT:  $r0d (rsqrt(x))
// CLOBBER: $r2 - $r7
const uint32_t lib_nvc0_rsqrt_f64[] =
{
   0x9c10dc08,
   0x00009c28,
   0x00019d18,
   0x3fe1df18,
   0x18001c01, 0x50000000,
   0x0001dde2, 0x18ffe000,
   0x08211c01, 0x50000000,
   0x10011e01, 0x200c0000,
   0x10209c01, 0x50000000,
   0x08211c01, 0x50000000,
   0x10011e01, 0x200c0000,
   0x10209c01, 0x50000000,
   0x08211c01, 0x50000000,
   0x10011e01, 0x200c0000,
   0x10201c01, 0x50000000,
   0x00001de7, 0x90000000,
};

static const uint8_t opInfo_srcNr[OP_LAST + 1] =
{
   0, 0, 0, 0, 0, 0,
   1, 1, 2,
   2, 2, 2, 2, 2, 3, 3, 3,
   1, 1, 1,
   2, 2, 2, 2, 2,
   2, 2, 1,
   1, 1, 1, 1,
   3, 3, 3, 2, 3, 3,
   1, 1, 1, 1, 1, 1,
   2, 2, 1, 1, 1, 2,
   0, 0, 0, 0, 0,
   0, 0, 0,
   0, 0, 0, 0, 0, 0,
   1, 1, 2, 1, 2,
   0, 0,
   1, 1, 1,
   1, 0, 1, 1, 1,
   1, 2,
   1, 1,
   1, 2, 2, 2, 0, 0,
   2, 3, 2,
   0
};

struct opProperties
{
   operation op;
   unsigned int mNeg   : 4;
   unsigned int mAbs   : 4;
   unsigned int mNot   : 4;
   unsigned int mSat   : 4;
   unsigned int fConst : 3;
   unsigned int fImmd  : 4; // last bit indicates if full immediate is suppoted
};

static const struct opProperties _initProps[] =
{
   //           neg  abs  not  sat  c[]  imm
   { OP_ADD,    0x3, 0x3, 0x0, 0x8, 0x2, 0x2 | 0x8 },
   { OP_SUB,    0x3, 0x3, 0x0, 0x0, 0x2, 0x2 | 0x8 },
   { OP_MUL,    0x3, 0x0, 0x0, 0x8, 0x2, 0x2 | 0x8 },
   { OP_MAX,    0x3, 0x3, 0x0, 0x0, 0x2, 0x2 },
   { OP_MIN,    0x3, 0x3, 0x0, 0x0, 0x2, 0x2 },
   { OP_MAD,    0x7, 0x0, 0x0, 0x8, 0x6, 0x2 | 0x8 }, // special c[] constraint
   { OP_ABS,    0x0, 0x0, 0x0, 0x0, 0x1, 0x0 },
   { OP_NEG,    0x0, 0x1, 0x0, 0x0, 0x1, 0x0 },
   { OP_CVT,    0x1, 0x1, 0x0, 0x8, 0x1, 0x0 },
   { OP_AND,    0x0, 0x0, 0x3, 0x0, 0x2, 0x2 | 0x8 },
   { OP_OR,     0x0, 0x0, 0x3, 0x0, 0x2, 0x2 | 0x8 },
   { OP_XOR,    0x0, 0x0, 0x3, 0x0, 0x2, 0x2 | 0x8 },
   { OP_SET,    0x3, 0x3, 0x0, 0x0, 0x2, 0x2 },
   { OP_SLCT,   0x0, 0x0, 0x0, 0x0, 0x6, 0x2 }, // special c[] constraint
   { OP_PREEX2, 0x1, 0x1, 0x0, 0x0, 0x1, 0x1 },
   { OP_PRESIN, 0x1, 0x1, 0x0, 0x0, 0x1, 0x1 },
   { OP_COS,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_SIN,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_EX2,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_LG2,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_RCP,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_RSQ,    0x1, 0x1, 0x0, 0x8, 0x0, 0x0 },
   { OP_DFDX,   0x1, 0x0, 0x0, 0x0, 0x0, 0x0 },
   { OP_DFDY,   0x1, 0x0, 0x0, 0x0, 0x0, 0x0 },
   { OP_CALL,   0x0, 0x0, 0x0, 0x0, 0x1, 0x0 },
   { OP_INSBF,  0x0, 0x0, 0x0, 0x0, 0x0, 0x4 },
   // saturate only:
   { OP_LINTERP, 0x0, 0x0, 0x0, 0x8, 0x0, 0x0 },
   { OP_PINTERP, 0x0, 0x0, 0x0, 0x8, 0x0, 0x0 },
};

void TargetNVC0::initOpInfo()
{
   unsigned int i, j;

   static const uint32_t commutative[(OP_LAST + 31) / 32] =
   {
      // ADD, MAD, MUL, AND, OR, XOR, MAX, MIN
      0x0670ca00, 0x0000003f, 0x00000000
   };

   static const uint32_t shortForm[(OP_LAST + 31) / 32] =
   {
      // ADD, MAD, MUL, AND, OR, XOR, PRESIN, PREEX2, SFN, CVT, PINTERP, MOV
      0x0670ca00, 0x00000000, 0x00000000
   };

   static const operation noDest[] =
   {
      OP_STORE, OP_WRSV, OP_EXPORT, OP_BRA, OP_CALL, OP_RET, OP_EXIT,
      OP_DISCARD, OP_CONT, OP_BREAK, OP_PRECONT, OP_PREBREAK, OP_PRERET,
      OP_JOIN, OP_JOINAT, OP_BRKPT, OP_MEMBAR, OP_EMIT, OP_RESTART,
      OP_QUADON, OP_QUADPOP
   };

   joinAnterior = false;

   for (i = 0; i < DATA_FILE_COUNT; ++i)
      nativeFileMap[i] = (DataFile)i;
   nativeFileMap[FILE_ADDRESS] = FILE_GPR;

   for (i = 0; i < OP_LAST; ++i) {
      opInfo[i].variants = NULL;
      opInfo[i].op = (operation)i;
      opInfo[i].srcTypes = 1 << (int)TYPE_F32;
      opInfo[i].dstTypes = 1 << (int)TYPE_F32;
      opInfo[i].immdBits = 0;
      opInfo[i].srcNr = opInfo_srcNr[i];

      for (j = 0; j < opInfo[i].srcNr; ++j) {
         opInfo[i].srcMods[j] = 0;
         opInfo[i].srcFiles[j] = 1 << (int)FILE_GPR;
      }
      opInfo[i].dstMods = 0;
      opInfo[i].dstFiles = 1 << (int)FILE_GPR;

      opInfo[i].hasDest = 1;
      opInfo[i].vector = (i >= OP_TEX && i <= OP_TEXCSAA);
      opInfo[i].commutative = (commutative[i / 32] >> (i % 32)) & 1;
      opInfo[i].pseudo = (i < OP_MOV);
      opInfo[i].predicate = !opInfo[i].pseudo;
      opInfo[i].flow = (i >= OP_BRA && i <= OP_JOIN);
      opInfo[i].minEncSize = (shortForm[i / 32] & (1 << (i % 32))) ? 4 : 8;
   }
   for (i = 0; i < sizeof(noDest) / sizeof(noDest[0]); ++i)
      opInfo[noDest[i]].hasDest = 0;

   for (i = 0; i < sizeof(_initProps) / sizeof(_initProps[0]); ++i) {
      const struct opProperties *prop = &_initProps[i];

      for (int s = 0; s < 3; ++s) {
         if (prop->mNeg & (1 << s))
            opInfo[prop->op].srcMods[s] |= NV50_IR_MOD_NEG;
         if (prop->mAbs & (1 << s))
            opInfo[prop->op].srcMods[s] |= NV50_IR_MOD_ABS;
         if (prop->mNot & (1 << s))
            opInfo[prop->op].srcMods[s] |= NV50_IR_MOD_NOT;
         if (prop->fConst & (1 << s))
            opInfo[prop->op].srcFiles[s] |= 1 << (int)FILE_MEMORY_CONST;
         if (prop->fImmd & (1 << s))
            opInfo[prop->op].srcFiles[s] |= 1 << (int)FILE_IMMEDIATE;
         if (prop->fImmd & 8)
            opInfo[prop->op].immdBits = 0xffffffff;
      }
      if (prop->mSat & 8)
         opInfo[prop->op].dstMods = NV50_IR_MOD_SAT;
   }
}

unsigned int
TargetNVC0::getFileSize(DataFile file) const
{
   switch (file) {
   case FILE_NULL:          return 0;
   case FILE_GPR:           return 63;
   case FILE_PREDICATE:     return 7;
   case FILE_FLAGS:         return 1;
   case FILE_ADDRESS:       return 0;
   case FILE_IMMEDIATE:     return 0;
   case FILE_MEMORY_CONST:  return 65536;
   case FILE_SHADER_INPUT:  return 0x400;
   case FILE_SHADER_OUTPUT: return 0x400;
   case FILE_MEMORY_GLOBAL: return 0xffffffff;
   case FILE_MEMORY_SHARED: return 16 << 10;
   case FILE_MEMORY_LOCAL:  return 48 << 10;
   case FILE_SYSTEM_VALUE:  return 32;
   default:
      assert(!"invalid file");
      return 0;
   }
}

unsigned int
TargetNVC0::getFileUnit(DataFile file) const
{
   if (file < FILE_MEMORY_CONST || file == FILE_SYSTEM_VALUE)
      return 2;
   return 0;
}

uint32_t
TargetNVC0::getSVAddress(DataFile shaderFile, const Symbol *sym) const
{
   const int idx = sym->reg.data.sv.index;
   const SVSemantic sv = sym->reg.data.sv.sv;

   const bool isInput = shaderFile == FILE_SHADER_INPUT;

   switch (sv) {
   case SV_POSITION:       return 0x070 + idx * 4;
   case SV_INSTANCE_ID:    return 0x2f8;
   case SV_VERTEX_ID:      return 0x2fc;
   case SV_PRIMITIVE_ID:   return isInput ? 0x060 : 0x040;
   case SV_LAYER:          return 0x064;
   case SV_VIEWPORT_INDEX: return 0x068;
   case SV_POINT_SIZE:     return 0x06c;
   case SV_CLIP_DISTANCE:  return 0x2c0 + idx * 4;
   case SV_POINT_COORD:    return 0x2e0 + idx * 4;
   case SV_FACE:           return 0x3fc;
   case SV_TESS_FACTOR:    return 0x000 + idx * 4;
   case SV_TESS_COORD:     return 0x2f0 + idx * 4;
   default:
      return 0xffffffff;
   }
}

bool
TargetNVC0::insnCanLoad(const Instruction *i, int s,
                        const Instruction *ld) const
{
   DataFile sf = ld->src[0].getFile();

   if (!(opInfo[i->op].srcFiles[s] & (1 << (int)sf)))
      return false;

   if (ld->src[0].isIndirect())
      return false;

   for (int k = 0; i->srcExists(k); ++k)
      if (i->src[k].getFile() != FILE_GPR &&
          i->src[k].getFile() != FILE_PREDICATE)
         return false;

   if (sf == FILE_IMMEDIATE) {
      Storage &reg = ld->getSrc(0)->asImm()->reg;

      if (reg.data.u64 == 0)
         return true;

      if (opInfo[i->op].immdBits != 0xffffffff) {
         if (i->sType == TYPE_F32) {
            if (reg.data.u32 & 0xfff)
               return false;
         } else
         if (i->sType == TYPE_S32 || i->sType == TYPE_U32) {
            // with u32, 0xfffff counts as 0xffffffff as well
            if (reg.data.s32 > 0x7ffff || reg.data.s32 < -0x80000)
               return false;
         }
      } else
      if ((i->op == OP_MAD || i->op == OP_FMA) && (i->dType == TYPE_F32)) {
         // requires src == dst, cannot decide before RA
         // (except if we implement more constraints)
         if (ld->getSrc(0)->asImm()->reg.data.u32 & 0xfff)
            return false;
      }
   }

   return true;
}

bool
TargetNVC0::isOpSupported(operation op, DataType ty) const
{
   if ((op == OP_MAD || op == OP_FMA) && (ty != TYPE_F32))
      return false;
   if (op == OP_SAD && ty != TYPE_S32)
      return false;
   if (op == OP_POW || op == OP_SQRT || op == OP_DIV || op == OP_MOD)
      return false;
   return true;
}

bool
TargetNVC0::isModSupported(const Instruction *insn, int s, Modifier mod) const
{
   return (mod & Modifier(opInfo[insn->op].srcMods[s])) == mod;
}

bool
TargetNVC0::mayPredicate(const Instruction *insn, const Value *pred) const
{
   if (insn->getPredicate())
      return false;
   return opInfo[insn->op].predicate;
}

bool
TargetNVC0::isSatSupported(const Instruction *insn) const
{
   if (insn->op == OP_CVT)
      return true;
   if (!(opInfo[insn->op].dstMods & NV50_IR_MOD_SAT))
      return false;

   if (insn->dType == TYPE_U32)
      return (insn->op == OP_ADD) || (insn->op == OP_MAD);

   return insn->dType == TYPE_F32;
}

} // namespace nv50_ir
