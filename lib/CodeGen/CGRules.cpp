//===--- CGRules.cpp - Emit LLVM Code for declarations ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit blocks.
//
//===----------------------------------------------------------------------===//

#include "CGBlocks.h"
#include "CGDebugInfo.h"
#include "CGObjCRuntime.h"
#include "CodeGenFunction.h"
#include "CodeGenModule.h"
#include "clang/CodeGen/ConstantInitBuilder.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Module.h"
#include <algorithm>
#include <cstdio>

using namespace clang;
using namespace CodeGen;

/// Prepare and emit a block literal expression in the current function.
llvm::Value *CodeGenFunction::EmitRuleLiteral(const RuleExpr *blockExpr) {
  // Allocate the block info and place it at the head of the list.
  const BlockDecl *blockDecl = blockExpr->getBlockDecl(); 
  QualType longType = CGM.getContext().LongTy; // all captured data now stored as i64
  SourceLocation loc;

  QualType thisType = cast<CXXMethodDecl>(CurFuncDecl)->getThisType(CGM.getContext());
  FunctionArgList Args; 
  IdentifierInfo *IThis = &CGM.getContext().Idents.get("this"); 
  Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl), loc,
      loc, IThis, thisType, /*TInfo=*/nullptr, SC_None, nullptr));

  /// Compute the layout of the given block.  The header is basically:
  //     'struct { void *invoke; void *STy; ... data for captures ...}'.
  SmallVector<llvm::Type*, 8> elementTypes;
  elementTypes.push_back(CGM.VoidPtrTy); // void *invoke;
  elementTypes.push_back(CGM.Int64Ty);   // i64   STy;

  // Next, all the block captures.
  int pindex = elementTypes.size();
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable(); 
    QualType VT = variable->getType();
    if (CI.isByRef() || VT->getAsCXXRecordDecl() || VT->isObjCRetainableType()
     || CI.hasCopyExpr() || CI.isNested() || VT->isReferenceType()) {
printf("[%s:%d]ZZZZZ\n", __FUNCTION__, __LINE__); exit(-1);
    }
    elementTypes.push_back(CGM.getTypes().ConvertTypeForMem(longType));
    IdentifierInfo *II = &CGM.getContext().Idents.get(CI.getVariable()->getName()); 
    Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl), loc,
        loc, II, CI.getVariable()->getType(), /*TInfo=*/nullptr, SC_None, nullptr));
  }
  llvm::StructType *StructureType = llvm::StructType::get(CGM.getLLVMContext(), elementTypes, true);
printf("[%s:%d] STRUCTURETYPE \n", __FUNCTION__, __LINE__);
StructureType->dump();
  const llvm::StructLayout *layout = CGM.getContext().getTargetInfo().getDataLayout().getStructLayout(StructureType);

  const CGFunctionInfo &FnInfo = CGM.getTypes().arrangeBlockFunctionDeclaration(
        blockExpr->getFunctionType(), Args);
  llvm::Function *Fn = llvm::Function::Create(CGM.getTypes().GetFunctionType(FnInfo),
      llvm::GlobalValue::ExternalLinkage, //why is this not allowed?? llvm::GlobalValue::InternalLinkage
      "ruleTemplate", &CGM.getModule());

  // Make the allocation for the block.
  Address blockAddr = CreateTempAlloca(StructureType, CGM.getPointerAlign(), "block"); 

  // Initialize the block header.
  auto projectField = [&](unsigned index, CharUnits offset, const Twine &name) -> Address {
      return Builder.CreateStructGEP(blockAddr, index, offset, name);
    };
  auto storeField = [&](llvm::Value *value, unsigned index, CharUnits offset, const Twine &name) {
      Builder.CreateStore(value, projectField(index, offset, name));
    };
  storeField(llvm::ConstantExpr::getBitCast(Fn, VoidPtrTy),
      0, CharUnits(), "block.invoke");                     // Function *invoke;
  storeField(llvm::Constant::getIntegerValue(CGM.Int64Ty,  // Int64Ty STy;
    llvm::APInt(64, (uint64_t) StructureType)), 1, CharUnits(), "block.STy");

  // Finally, capture all the values into the block.
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable();
    QualType VT = variable->getType();
    // Fake up a new variable so that EmitScalarInit doesn't think
    // we're referring to the variable in its own initializer.
    DeclRefExpr declRef(const_cast<VarDecl *>(variable),
        /*RefersToEnclosingVariableOrCapture*/ false, VT, VK_LValue, SourceLocation()); 
    Expr *rval = ImplicitCastExpr::Create(CGM.getContext(), VT,
          CK_LValueToRValue, &declRef, nullptr, VK_RValue);
    if (VT != longType)
        rval = ImplicitCastExpr::Create(CGM.getContext(), longType,
              CK_IntegralCast, rval, nullptr, VK_RValue);
    ImplicitParamDecl BlockFieldPseudoVar(getContext(), longType, ImplicitParamDecl::Other); 
    EmitExprAsInit(rval, &BlockFieldPseudoVar, MakeAddrLValue(
        projectField(pindex, CharUnits::fromQuantity(layout->getElementOffset(pindex)), "block.captured"),
        longType, LValueBaseInfo(AlignmentSource::Decl, false)), /*captured by init*/ false);
    pindex++;
  } 

  // Now generate function itself
  CGBlockInfo &blockInfo = *new CGBlockInfo(blockDecl, "");
  blockInfo.NextBlockInfo = FirstBlockInfo;
  FirstBlockInfo = &blockInfo; 
  auto AI = Fn->arg_begin();
  AI++; // skip 'this' parameter
  for (const auto &CI : blockDecl->captures()) {
      blockInfo.paramMap[CI.getVariable()] = AI;
      AI++;
  }
  CodeGenFunction(CGM, true).GenerateRuleFunction(CurGD, blockInfo, FnInfo.getReturnType(),
         Fn, FnInfo, Args);

  // Cast to the converted block-pointer type, which happens (somewhat
  // unfortunately) to be a pointer to function type.
  return Builder.CreatePointerCast(blockAddr.getPointer(), ConvertType(blockExpr->getType()));
}

llvm::Value *CodeGenFunction::GetAddrOfBlockDeclRule(const VarDecl *variable) {
  return const_cast<CGBlockInfo *>(BlockInfo)->paramMap[variable];
}

llvm::Function *
CodeGenFunction::GenerateRuleFunction(GlobalDecl GD,
                                    const CGBlockInfo &blockInfo,
                                    QualType RetTy,
                                    llvm::Function *Fn,
                                    const CGFunctionInfo &FnInfo,
                                    const FunctionArgList &Args) {
printf("[%s:%d]\n", __FUNCTION__, __LINE__);
  BlockInfo = &blockInfo; // needed for GetAddrOfBlockDeclRule
  CurGD = GD; 
  const BlockDecl *FD = blockInfo.getBlockDecl(); 

  CXXThisValue = Fn->arg_begin();

  // Emit the standard function prologue.
  Stmt *Body = FD->getBody();
  StartFunction(FD, RetTy, Fn, FnInfo, Args, FD->getLocation(), Body->getLocStart()); 
  // Generate the body of the function.
  EmitStmt(Body);  // triggers callbacks to GetAddrOfBlockDeclRule for Captures items

  FinishFunction(CurEHLocation);
  return Fn;
}
