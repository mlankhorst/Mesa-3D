
#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"

using namespace nv50_ir;

class Converter : public llvm::ModulePass
{
public:
   Converter();

private:
   virtual bool runOnModule(llvm::Module& M);

   operation getOperation(const llvm::Instruction *);

   DataType inferSrcType(const llvm::Instruction *);
   DataType inferDstType(const llvm::Instruction *);

   bool handleFunction();
   bool handleBasicBlock();
   bool handleInstruction();

private:
   static char ID;

private:
   BuildUtil bld;
};

operation Converter::getOperation(const llvm::Instruction *insn)
{
   switch (insn->getOpcode()) {
   case llvm::Instruction::Ret:
      return OP_RET;
   case llvm::Instruction::Br:
   case llvm::Instruction::IndirectBr:
      return OP_BRA;
   case llvm::Instruction::Invoke: // ?
      return OP_CALL;
   case llvm::Instruction::Add:
   case llvm::Instruction::FAdd:
      return OP_ADD;
   case llvm::Instruction::Sub:
   case llvm::Instruction::FSub:
      return OP_SUB;
   case llvm::Instruction::Mul:
   case llvm::Instruction::FMul:
      return OP_MUL;
   case llvm::Instruction::UDiv:
   case llvm::Instruction::SDiv:
   case llvm::Instruction::FDiv:
      return OP_DIV;
   case llvm::Instruction::URem:
   case llvm::Instruction::SRem:
   case llvm::Instruction::FRem:
      return OP_MOD;
   case llvm::Instruction::Shl:
      return OP_SHL;
   case llvm::Instruction::LShr:
   case llvm::Instruction::AShr:
      return OP_SHR;
   case llvm::Instruction::And:
      return OP_AND;
   case llvm::Instruction::Or:
      return OP_OR;
   case llvm::Instruction::Xor:
      return OP_XOR;
   case llvm::Instruction::Load:
      return OP_LOAD;
   case llvm::Instruction::Store:
      return OP_STORE;
   case llvm::Instruction::Trunc:
   case llvm::Instruction::FPTrunc:
      return OP_TRUNC;
   case llvm::Instruction::ZExt:
   case llvm::Instruction::SExt:
   case llvm::Instruction::FPToUI:
   case llvm::Instruction::FPToSI:
   case llvm::Instruction::UIToFP:
   case llvm::Instruction::SIToFP:
   case llvm::Instruction::FPExt:
      return OP_CVT;
   case llvm::Instruction::PtrToInt:
   case llvm::Instruction::IntToPtr:
   case llvm::Instruction::BitCast:
      return OP_MOV;
   case llvm::Instruction::ICmp:
   case llvm::Instruction::FCmp:
      return OP_SET;
   case llvm::Instruction::Phi:
      return OP_PHI;
   case llvm::Instruction::Call:
      return OP_CALL;
   case llvm::Instruction::Select:
      return OP_SLCT;
   case llvm::Instruction::ExtractElement:
   case llvm::Instruction::ExtractValue:
      return OP_SPLIT;
   case llvm::Instruction::InsertElement:
   case llvm::Instruction::InsertValue:
      return OP_MERGE;
   case llvm::Instruction::Unwind:
   case llvm::Instruction::Unreachable:
   case llvm::Instruction::Alloca:
   case llvm::Instruction::GetElementPtr:
   case llvm::Instruction::ShuffleVector:
   default:
      return OP_NOP;
   }
}

DataType inferSrcType(llvm::Instruction *insn)
{
   switch (insn->getOpcode()) {
   case llvm::Instruction::Add:
   case llvm::Instruction::Sub:
   case llvm::Instruction::Mul:
   case llvm::Instruction::UDiv:
   case llvm::Instruction::URem:
   case llvm::Instruction::SRem:
   case llvm::Instruction::Shl:
   case llvm::Instruction::LShr:
   case llvm::Instruction::And:
   case llvm::Instruction::Or:
   case llvm::Instruction::Xor:
   case llvm::Instruction::Trunc:
   case llvm::Instruction::Load:
   case llvm::Instruction::Store:
   case llvm::Instruction::ZExt:
   case llvm::Instruction::UIToFP:
   case llvm::Instruction::PtrToInt:
   case llvm::Instruction::IntToPtr:
   case llvm::Instruction::BitCast:
      return TYPE_U32;
   case llvm::Instruction::SDiv:
   case llvm::Instruction::AShr:
   case llvm::Instruction::SExt:
   case llvm::Instruction::SIToFP:
   case llvm::Instruction::ICmp:
      return TYPE_S32;
   case llvm::Instruction::FAdd:
   case llvm::Instruction::FSub:
   case llvm::Instruction::FMul:
   case llvm::Instruction::FDiv:
   case llvm::Instruction::FRem:
   case llvm::Instruction::FPTrunc:
   case llvm::Instruction::FPToUI:
   case llvm::Instruction::FPToSI:
   case llvm::Instruction::FPExt:
   case llvm::Instruction::FCmp:
      return TYPE_F32;
   default:
      return TYPE_NONE;
}

DataType inferDstType(llvm::Instruction *insn)
{
   switch (insn->getOpcode()) {
   case llvm::Instruction::FPToUI:
      return TYPE_U32;
   case llvm::Instruction::FPToSI:
      return TYPE_S32;
   case llvm::Instruction::SIToFP:
   case llvm::Instruction::UIToFP:
      return TYPE_F32;
   default:
      return inferSrcType(insn);
   }
}

bool Converter::handleInstruction()
{
}

bool Converter::handleBasicBlock()
{
}

bool Converter::handleFunction()
{
}

bool Converter::runOnModule(llvm::Module& M)
{
   for (llvm::Module::iterator fni = M.begin(); fni != M.end(); ++fni) {
      if (fni->isDeclaration())
         continue;
      handleFunction(fni, new Function());
   }

   return false; // no modification
}

Converter::Converter()
   : ModulePass(ID)
{
   // ...
}
