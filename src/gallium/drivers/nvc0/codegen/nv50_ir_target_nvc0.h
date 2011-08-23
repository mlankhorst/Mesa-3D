
#include "nv50/codegen/nv50_ir_target.h"

namespace nv50_ir {

class TargetNVC0 : public Target
{
public:
   TargetNVC0(unsigned int chipset);

   virtual CodeEmitter *getCodeEmitter(Program::Type);

   virtual bool insnCanLoad(const Instruction *insn, int s,
                            const Instruction *ld) const;
   virtual bool isOpSupported(operation, DataType) const;
   virtual bool isModSupported(const Instruction *, int s, Modifier) const;
   virtual bool isSatSupported(const Instruction *) const;
   virtual bool mayPredicate(const Instruction *, const Value *) const;

   virtual unsigned int getFileSize(DataFile) const;
   virtual unsigned int getFileUnit(DataFile) const;

   virtual uint32_t getSVAddress(DataFile shaderFile, const Symbol *sv) const;

   virtual bool runLegalizePass(Program *, CGStage stage) const;
};

} // namespace nv50_ir
