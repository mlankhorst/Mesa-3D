
#ifndef __NV50_IR_TARGET_H__
#define __NV50_IR_TARGET_H__

#include "nv50_ir.h"

namespace nv50_ir {

class CodeEmitter
{
public:
   // @return: whether the instruction has been successfully written
   virtual bool emitInstruction(Instruction *) = 0;

   virtual uint32_t getMinEncodingSize(const Instruction *) const = 0;

   void setCodeLocation(void *, uint32_t size);
   void *getCodeLocation() const { return code; }
   uint32_t getCodeSize() const { return codeSize; }

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
   Target()
      : joinAnterior(false)
   {
      chipset = 0xc0;
      arch = chipset & 0xf0;

      initOpInfo();
   };

   unsigned int getArch() const { return arch; }
   unsigned int getChipset() const { return chipset; }

   virtual bool insnCanLoad(const Instruction *, int s, const Instruction *ld)
      const;
   virtual bool isOpSupported(operation, DataType) const;
   virtual bool isModSupported(const Instruction *, int s, Modifier) const;
   virtual bool isSatSupported(const Instruction *) const;
   virtual bool mayPredicate(const Instruction *, const Value *) const;

   bool shaderExportInPlace(Program::Type) const;

   virtual unsigned int getFileSize(DataFile) const;
   virtual unsigned int getFileUnit(DataFile) const;

   uint32_t getSVAddress(DataFile shaderFile, const Symbol *sv) const;

   virtual CodeEmitter *getCodeEmitter(Program::Type);
   inline  void         putCodeEmitter(CodeEmitter *);

   virtual bool runLegalizePass(Program *, CGStage stage) const;

   class OpInfo
   {
   public:
      OpInfo *variants;
      const char *name;
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

   static Target *create(unsigned int arch)
   {
      switch (arch & 0xf0) {
      case 0x50:
      case 0x80:
      case 0x90:
      case 0xa0:
      case 0xc0:
         return new Target();
      default:
         ERROR("unsupported target: %x\n", arch);
         return 0;
      }
   }

   inline const OpInfo& getInfo(const Instruction *insn) const
   {
      return opInfo[MIN2(insn->op, OP_LAST)];
   }

   inline const OpInfo& getOpInfo(const operation op) const
   {
      return opInfo[op];
   }

   inline DataFile nativeFile(DataFile f) const
   {
      return fileMap[f];
   }

public:
   const bool joinAnterior; // true if join is executed before the op

private:
   void initOpInfo();

private:
   unsigned int arch;
   unsigned int chipset;

   DataFile fileMap[DATA_FILE_COUNT];

   OpInfo opInfo[OP_LAST + 1];
};

} // namespace nv50_ir

#endif // __NV50_IR_TARGET_H__
