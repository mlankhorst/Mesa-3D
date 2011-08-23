
#ifndef __NV50_IR_TARGET_H__
#define __NV50_IR_TARGET_H__

#include "nv50_ir.h"

namespace nv50_ir {

class CodeEmitter
{
public:
   // returns whether the instruction was encodable and written
   virtual bool emitInstruction(Instruction *) = 0;

   virtual uint32_t getMinEncodingSize(const Instruction *) const = 0;

   void setCodeLocation(void *, uint32_t size);
   inline void *getCodeLocation() const { return code; }
   inline uint32_t getCodeSize() const { return codeSize; }

   void prepareEmission(Program *);
   void prepareEmission(Function *);
   virtual void prepareEmission(BasicBlock *);

   void printBinary() const;

protected:
   uint32_t *code;
   uint32_t codeSize;
   uint32_t maxCodeSize;
};

class Target
{
public:
   static Target *create(unsigned int chipset);

   inline unsigned int getChipset() const { return chipset; }

   virtual CodeEmitter *getCodeEmitter(Program::Type) = 0;

   virtual bool runLegalizePass(Program *, CGStage stage) const = 0;

public:
   struct OpInfo
   {
      OpInfo *variants;
      operation op;
      uint16_t srcTypes;
      uint16_t dstTypes;
      uint32_t immdBits;
      uint8_t srcNr;
      uint8_t srcMods[3];
      uint8_t dstMods;
      uint8_t srcFiles[3];
      uint8_t dstFiles;
      unsigned int minEncSize  : 4;
      unsigned int vector      : 1;
      unsigned int predicate   : 1;
      unsigned int commutative : 1;
      unsigned int pseudo      : 1;
      unsigned int flow        : 1;
      unsigned int hasDest     : 1;
   };

   inline const OpInfo& getOpInfo(const Instruction *) const;
   inline const OpInfo& getOpInfo(const operation) const;

   inline DataFile nativeFile(DataFile f) const;

   virtual bool insnCanLoad(const Instruction *insn, int s,
                            const Instruction *ld) const = 0;
   virtual bool isOpSupported(operation, DataType) const = 0;
   virtual bool isModSupported(const Instruction *, int s, Modifier) const = 0;
   virtual bool isSatSupported(const Instruction *) const = 0;
   virtual bool mayPredicate(const Instruction *, const Value *) const = 0;

   virtual unsigned int getFileSize(DataFile) const = 0;
   virtual unsigned int getFileUnit(DataFile) const = 0;

   virtual uint32_t getSVAddress(DataFile, const Symbol *) const = 0;

public:
   bool joinAnterior; // true if join is executed before the op

protected:
   unsigned int chipset;

   DataFile nativeFileMap[DATA_FILE_COUNT];

   OpInfo opInfo[OP_LAST + 1];
};

const Target::OpInfo& Target::getOpInfo(const Instruction *insn) const
{
   return opInfo[MIN2(insn->op, OP_LAST)];
}

const Target::OpInfo& Target::getOpInfo(const operation op) const
{
   return opInfo[op];
}

inline DataFile Target::nativeFile(DataFile f) const
{
   return nativeFileMap[f];
}

} // namespace nv50_ir

#endif // __NV50_IR_TARGET_H__
