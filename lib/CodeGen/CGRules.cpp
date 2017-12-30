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
  CGBlockInfo &blockInfo = *new CGBlockInfo(blockDecl, "");
  blockInfo.NextBlockInfo = FirstBlockInfo;
  FirstBlockInfo = &blockInfo; 
  SourceLocation loc;

  QualType thisType = cast<CXXMethodDecl>(CurFuncDecl)->getThisType(CGM.getContext());
  const FunctionProtoType *FnType = blockExpr->getFunctionType();
  FunctionArgList Args; 
  IdentifierInfo *IThis = &CGM.getContext().Idents.get("this"); 
  Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl), loc,
      loc, IThis, thisType, /*TInfo=*/nullptr, SC_None, nullptr));

  /// Compute the layout of the given block.  The header is basically:
  //     'struct { void *invoke; void *STy; ... data for captures ...}'.
  SmallVector<llvm::Type*, 8> elementTypes;
  elementTypes.push_back(CGM.VoidPtrTy); // void *invoke;
  elementTypes.push_back(CGM.Int64Ty);   // i64   STy;
  QualType VTl = CGM.getContext().LongTy; // all captured data now stored as i64

  // Next, all the block captures.
  int pindex = elementTypes.size();
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable(); 
    QualType VT = variable->getType();
    if (CI.isByRef() || VT->getAsCXXRecordDecl() || VT->isObjCRetainableType()
     || CI.hasCopyExpr() || CI.isNested() || VT->isReferenceType()) {
printf("[%s:%d]ZZZZZ\n", __FUNCTION__, __LINE__); exit(-1);
    }
    elementTypes.push_back(CGM.getTypes().ConvertTypeForMem(VTl));
    IdentifierInfo *II = &CGM.getContext().Idents.get(CI.getVariable()->getName()); 
    Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl), loc,
        loc, II, CI.getVariable()->getType(), /*TInfo=*/nullptr, SC_None, nullptr));
  }
  const CGFunctionInfo &FnInfo = CGM.getTypes().arrangeBlockFunctionDeclaration(FnType, Args);
  llvm::Function *Fn = llvm::Function::Create(CGM.getTypes().GetFunctionType(FnInfo),
      llvm::GlobalValue::ExternalLinkage, //why is this not allowed?? llvm::GlobalValue::InternalLinkage
      "ruleTemplate", &CGM.getModule());

  auto AI = Fn->arg_begin();
  AI++; // skip 'this' parameter
  for (const auto &CI : blockDecl->captures()) {
      blockInfo.paramMap[CI.getVariable()] = AI;
      AI++;
  }

  llvm::Constant *blockFn = llvm::ConstantExpr::getBitCast(
      CodeGenFunction(CGM, true).GenerateRuleFunction(CurGD, blockInfo, FnType->getReturnType(),
         Fn, FnInfo, Args),
      VoidPtrTy);

  llvm::StructType *StructureType = llvm::StructType::get(CGM.getLLVMContext(), elementTypes, true);
printf("[%s:%d] STRUCTURETYPE \n", __FUNCTION__, __LINE__);
StructureType->dump();
  const llvm::StructLayout *layout = CGM.getContext().getTargetInfo().getDataLayout().getStructLayout(StructureType);
  // Make the allocation for the block.
  Address blockAddr = CreateTempAlloca(StructureType, CGM.getPointerAlign(), "block"); 

  // Initialize the block header.
  auto projectField = [&](unsigned index, CharUnits offset, const Twine &name) -> Address {
      return Builder.CreateStructGEP(blockAddr, index, offset, name);
    };
  auto storeField = [&](llvm::Value *value, unsigned index, CharUnits offset, const Twine &name) {
      Builder.CreateStore(value, projectField(index, offset, name));
    };
  storeField(blockFn, 0, CharUnits(), "block.invoke");             // Function *invoke;
  storeField(llvm::Constant::getIntegerValue(CGM.Int64Ty,          // Int64Ty STy;
    llvm::APInt(64, (uint64_t) StructureType)), 1, CharUnits(), "block.STy");

  // Finally, capture all the values into the block.
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable();
    QualType VT = variable->getType();
    // Fake up a new variable so that EmitScalarInit doesn't think
    // we're referring to the variable in its own initializer.
    ImplicitParamDecl BlockFieldPseudoVar(getContext(), VTl, ImplicitParamDecl::Other); 
    DeclRefExpr declRef(const_cast<VarDecl *>(variable), /*RefersToEnclosingVariableOrCapture*/ false,
                        VT, VK_LValue, SourceLocation()); 
    ImplicitCastExpr l2r(ImplicitCastExpr::OnStack, VT, CK_LValueToRValue, &declRef, VK_RValue);
    Expr *rval = &l2r;
    if (VT != VTl)
        rval = ImplicitCastExpr::Create(CGM.getContext(), VTl, CK_IntegralCast,  &l2r, nullptr, VK_RValue);
    LValueBaseInfo BaseInfo(AlignmentSource::Decl, false);
    EmitExprAsInit(rval, &BlockFieldPseudoVar, MakeAddrLValue(
        projectField(pindex, CharUnits::fromQuantity(layout->getElementOffset(pindex)), "block.captured"),
        VTl, BaseInfo), /*captured by init*/ false);
    pindex++;
  } 
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
