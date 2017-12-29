//===--- CGBlocks.cpp - Emit LLVM Code for declarations ---------*- C++ -*-===//
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

/// Get the low bit of a nonzero character count.  This is the
/// alignment of the nth byte if the 0th byte is universally aligned.
static CharUnits getLowBit(CharUnits v) {
  return CharUnits::fromQuantity(v.getQuantity() & (~v.getQuantity() + 1));
}

/// Prepare and emit a block literal expression in the current function.
llvm::Value *CodeGenFunction::EmitRuleLiteral(const RuleExpr *blockExpr) {
printf("[%s:%d]\n", __FUNCTION__, __LINE__);
  /// Enter the scope of a block.  This should be run at the entrance to
  /// a full-expression so that the block's cleanups are pushed at the
  /// right place in the stack.
  assert(HaveInsertPoint()); 
  // Allocate the block info and place it at the head of the list.
  const BlockDecl *blockDecl = blockExpr->getBlockDecl(); 
  CGBlockInfo &blockInfo = *new CGBlockInfo(blockDecl, CurFn->getName());
  blockInfo.NextBlockInfo = FirstBlockInfo;
  FirstBlockInfo = &blockInfo; 
  blockInfo.BlockAlign = CGM.getPointerAlign();

  /// Compute the layout of the given block.  The header is basically:
  //     'struct { void *invoke; void *STy; ... data for captures ...}'.
  SmallVector<llvm::Type*, 8> elementTypes;
  elementTypes.push_back(CGM.VoidPtrTy); // void *invoke;
  elementTypes.push_back(CGM.Int64Ty);   // i64   STy;
  blockInfo.BlockSize = 2 * CGM.getPointerSize();
  CharUnits endAlign = getLowBit(blockInfo.BlockSize); 
  QualType VTl = CGM.getContext().LongTy;

  // Next, all the block captures.
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable(); 
    QualType VT = variable->getType();
    if (CI.isByRef() || VT->getAsCXXRecordDecl() || VT->isObjCRetainableType()
     || CI.hasCopyExpr() || CI.isNested() || VT->isReferenceType()) {
printf("[%s:%d]ZZZZZ\n", __FUNCTION__, __LINE__); exit(-1);
    }
    if (endAlign < blockInfo.BlockAlign) {
      CharUnits padding = blockInfo.BlockAlign - endAlign;
      elementTypes.push_back(llvm::ArrayType::get(CGM.Int8Ty, padding.getQuantity()));
      blockInfo.BlockSize += padding;
      endAlign = getLowBit(blockInfo.BlockSize);
    }
    assert(endAlign >= blockInfo.BlockAlign);
    blockInfo.Captures.insert({variable,
        CGBlockInfo::Capture::makeIndex(elementTypes.size(), blockInfo.BlockSize, VT)});
    elementTypes.push_back(CGM.getTypes().ConvertTypeForMem(VTl));
    blockInfo.BlockSize += CGM.getContext().getTypeSizeInChars(VTl);
    endAlign = getLowBit(blockInfo.BlockSize);
  } 
  blockInfo.StructureType = llvm::StructType::get(CGM.getLLVMContext(), elementTypes, true);
printf("[%s:%d] STRUCTURETYPE \n", __FUNCTION__, __LINE__);
blockInfo.StructureType->dump();

  // Using the computed layout, generate the actual block function.
  QualType thisType = cast<CXXMethodDecl>(CurFuncDecl)->getThisType(CGM.getContext());
  llvm::Constant *blockFn = llvm::ConstantExpr::getBitCast(
      CodeGenFunction(CGM, true).GenerateRuleFunction(CurGD, blockInfo, thisType, blockExpr->getFunctionType()),
      VoidPtrTy);

  // Make the allocation for the block.
  Address blockAddr = CreateTempAlloca(blockInfo.StructureType, blockInfo.BlockAlign, "block"); 

  // Initialize the block header.
  auto projectField = [&](unsigned index, CharUnits offset, const Twine &name) -> Address {
      return Builder.CreateStructGEP(blockAddr, index, offset, name);
    };
  auto storeField = [&](llvm::Value *value, unsigned index, CharUnits offset, const Twine &name) {
      Builder.CreateStore(value, projectField(index, offset, name));
    };
  storeField(blockFn, 0, CharUnits(), "block.invoke");             // Function *invoke;
  storeField(llvm::Constant::getIntegerValue(CGM.Int64Ty,          // Int64Ty STy;
    llvm::APInt(64, (uint64_t) blockInfo.StructureType)), 1, CharUnits(), "block.STy");

  // Finally, capture all the values into the block.
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable();
    const CGBlockInfo::Capture &capture = blockInfo.getCapture(variable); 
    QualType VT = capture.fieldType(); 
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
        projectField(capture.getIndex(), capture.getOffset(), "block.captured"),
        VTl, BaseInfo), /*captured by init*/ false);
  } 
  // Cast to the converted block-pointer type, which happens (somewhat
  // unfortunately) to be a pointer to function type.
  return Builder.CreatePointerCast(blockAddr.getPointer(), ConvertType(blockExpr->getType()));
}

llvm::DenseMap<int, llvm::Value *> paramMap;
llvm::Value *CodeGenFunction::GetAddrOfBlockDeclRule(const VarDecl *variable) {
  return paramMap[BlockInfo->getCapture(variable).getIndex()];
}

llvm::Function *
CodeGenFunction::GenerateRuleFunction(GlobalDecl GD,
                                       const CGBlockInfo &blockInfo,
                                       QualType thisType,
                                       const FunctionProtoType *FnType) {
printf("[%s:%d]\n", __FUNCTION__, __LINE__);
  BlockInfo = &blockInfo; 
  CurGD = GD; 
  const BlockDecl *FD = blockInfo.getBlockDecl(); 
  SourceLocation loc;

  // Build the function declaration.  
  FunctionArgList Args; 
  IdentifierInfo *IThis = &CGM.getContext().Idents.get("this"); 
  Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(FD), loc,
      loc, IThis, thisType, /*TInfo=*/nullptr, SC_None, nullptr));
  for (const auto &CI : FD->captures()) {
      IdentifierInfo *II = &CGM.getContext().Idents.get(CI.getVariable()->getName()); 
      Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(FD), loc,
          loc, II, BlockInfo->getCapture(CI.getVariable()).fieldType(),
          /*TInfo=*/nullptr, SC_None, nullptr));
  }
  const CGFunctionInfo &FnInfo = CGM.getTypes().arrangeBlockFunctionDeclaration(FnType, Args);
  llvm::Function *Fn = llvm::Function::Create(CGM.getTypes().GetFunctionType(FnInfo),
      llvm::GlobalValue::InternalLinkage, "ruleTemplate", &CGM.getModule());
  auto AI = Fn->arg_begin(), AE = Fn->arg_end();
  CXXThisValue = AI;
  int paramIndex = 1;
  for (; AI != AE; AI++, paramIndex++)
      paramMap[paramIndex] = AI;

  // Emit the standard function prologue.
  Stmt *Body = FD->getBody();
  StartFunction(FD, FnType->getReturnType(), Fn, FnInfo, Args, FD->getLocation(), Body->getLocStart()); 
  // Generate the body of the function.
  EmitStmt(Body);  // triggers callbacks to GetAddrOfBlockDeclRule for Captures items

  FinishFunction(CurEHLocation);
  return Fn;
}
