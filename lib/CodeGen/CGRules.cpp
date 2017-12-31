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
  const BlockDecl *blockDecl = blockExpr->getBlockDecl(); 
  QualType longType = CGM.getContext().LongTy; // all captured data now stored as i64
  SourceLocation loc;

  QualType thisType = cast<CXXMethodDecl>(CurFuncDecl)->getThisType(CGM.getContext());
  FunctionArgList Args; 
  IdentifierInfo *IThis = &CGM.getContext().Idents.get("this"); 
  Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl),
      loc, loc, IThis, thisType, /*TInfo=*/nullptr, SC_None, nullptr));

  /// Compute the layout of the given block.  The header is basically:
  //     'struct { void *invoke; ... data for captures ...}'.
  //  All the captured data is stored as i64 values

  // Next, all the block captures.
  for (const auto &CI : blockDecl->captures()) {
    QualType VT = CI.getVariable()->getType();
    if (CI.isByRef() || VT->getAsCXXRecordDecl() || VT->isObjCRetainableType()
     || CI.hasCopyExpr() || CI.isNested() || VT->isReferenceType()) {
printf("[%s:%d]ZZZZZ\n", __FUNCTION__, __LINE__); exit(-1);
    }
    IdentifierInfo *II = &CGM.getContext().Idents.get(CI.getVariable()->getName()); 
    Args.push_back(ParmVarDecl::Create(getContext(), const_cast<BlockDecl *>(blockDecl),
        loc, loc, II, VT, /*TInfo=*/nullptr, SC_None, nullptr));
  }
  llvm::ArrayType *aType = llvm::ArrayType::get(CGM.Int64Ty, blockDecl->captures().size() + 1 /*for 'invoke'*/);

  const CGFunctionInfo &FnInfo = CGM.getTypes().arrangeBlockFunctionDeclaration(
        blockExpr->getFunctionType(), Args);
  llvm::Function *Fn = llvm::Function::Create(CGM.getTypes().GetFunctionType(FnInfo),
      llvm::GlobalValue::ExternalLinkage, //why is this not allowed?? llvm::GlobalValue::InternalLinkage
      "ruleTemplate", &CGM.getModule());

  // Make the allocation and initialize the block.
  Address blockAddr = CreateTempAlloca(aType, CGM.getPointerAlign(), "block"); 
  int pindex = 0;
  auto projectField = [&](const Twine &name) -> Address {
      auto ret = Builder.CreateConstArrayGEP(blockAddr, pindex,
           CGM.getContext().getTypeSizeInChars(CGM.getContext().LongTy), name);
      pindex++;
      return ret;
    };
  // Initialize the block header.
  Builder.CreateStore(llvm::Constant::getIntegerValue(CGM.Int64Ty, llvm::APInt(64, (uint64_t) Fn)),
      projectField("block.invoke"));// Function *invoke;

  // Finally, capture all the values into the block.
  for (const auto &CI : blockDecl->captures()) {
    const VarDecl *variable = CI.getVariable();
    QualType VT = variable->getType();
    // Fake up a new variable so that EmitScalarInit doesn't think
    // we're referring to the variable in its own initializer.
    DeclRefExpr declRef(const_cast<VarDecl *>(variable),
        /*RefersToEnclosingVariableOrCapture*/ false, VT, VK_LValue, loc); 
    Expr *rval = ImplicitCastExpr::Create(CGM.getContext(), VT,
          CK_LValueToRValue, &declRef, nullptr, VK_RValue);
    if (VT != longType)
        rval = ImplicitCastExpr::Create(CGM.getContext(), longType,
              CK_IntegralCast, rval, nullptr, VK_RValue);
    ImplicitParamDecl BlockFieldPseudoVar(getContext(), longType, ImplicitParamDecl::Other); 
    EmitExprAsInit(rval, &BlockFieldPseudoVar, MakeAddrLValue(projectField("block.captured"),
        longType, LValueBaseInfo(AlignmentSource::Decl, false)), /*captured by init*/ false);
  } 

  // Now generate function itself
  // Allocate the block info and place it at the head of the list.
  CGBlockInfo &blockInfo = *new CGBlockInfo(blockDecl, "");
  blockInfo.NextBlockInfo = FirstBlockInfo;
  FirstBlockInfo = &blockInfo; 
  auto AI = Fn->arg_begin();
  AI++; // skip 'this' parameter
  for (const auto &CI : blockDecl->captures())
      blockInfo.paramMap[CI.getVariable()] = AI++; // used by VisitDeclRefExpr for replacing values
  CodeGenFunction(CGM, true).GenerateRuleFunction(CurGD, blockInfo, FnInfo.getReturnType(),
         Fn, FnInfo, Args);

  // Cast to the converted block-pointer type, which happens
  // (somewhat unfortunately) to be a pointer to function type.
  return Builder.CreatePointerCast(blockAddr.getPointer(), ConvertType(blockExpr->getType()));
}

llvm::Function *
CodeGenFunction::GenerateRuleFunction(GlobalDecl GD,
                                    const CGBlockInfo &blockInfo,
                                    QualType RetTy,
                                    llvm::Function *Fn,
                                    const CGFunctionInfo &FnInfo,
                                    const FunctionArgList &Args) {
printf("[%s:%d]\n", __FUNCTION__, __LINE__);
  BlockInfo = &blockInfo; // needed for VisitDeclRefExpr
  CurGD = GD; 
  const BlockDecl *FD = blockInfo.getBlockDecl(); 

  CXXThisValue = Fn->arg_begin();

  // Emit the standard function prologue.
  Stmt *Body = FD->getBody();
  StartFunction(FD, RetTy, Fn, FnInfo, Args, FD->getLocation(), Body->getLocStart()); 
  // Generate the body of the function.
  EmitStmt(Body);  // captured values are replaced in CGExprScalar.cpp/VisitDeclRefExpr()

  FinishFunction(CurEHLocation);
  return Fn;
}
