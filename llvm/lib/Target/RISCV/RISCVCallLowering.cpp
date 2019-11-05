//===-- RISCVCallLowering.cpp - Call lowering -------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file implements the lowering of LLVM calls to machine code calls for
/// GlobalISel.
//
//===----------------------------------------------------------------------===//

#include "RISCVCallLowering.h"
#include "RISCVISelLowering.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/GlobalISel/MachineIRBuilder.h"

using namespace llvm;

namespace {

struct OutgoingValueHandler : public CallLowering::ValueHandler {
  OutgoingValueHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                       MachineInstrBuilder MIB, CCAssignFn *AssignFn)
      : ValueHandler(B, MRI, AssignFn), MIB(MIB) {}

  MachineInstrBuilder MIB;

  bool isIncomingArgumentHandler() const override { return false; }

  Register getStackAddress(uint64_t Size, int64_t Offset,
                           MachinePointerInfo &MPO) override {
    llvm_unreachable("not implemented");
  }

  void assignValueToAddress(Register ValVReg, Register Addr, uint64_t Size,
                            MachinePointerInfo &MPO, CCValAssign &VA) override {
    llvm_unreachable("not implemented");
  }

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        CCValAssign &VA) override {
    Register ExtReg = extendRegister(ValVReg, VA);
    MIRBuilder.buildCopy(PhysReg, ExtReg);
    MIB.addUse(PhysReg, RegState::Implicit);
  }

  bool assignArg(unsigned ValNo, MVT ValVT, MVT LocVT,
                 CCValAssign::LocInfo LocInfo,
                 const CallLowering::ArgInfo &Info, ISD::ArgFlagsTy Flags,
                 CCState &State) override {
    if (AssignFn)
      return AssignFn(ValNo, ValVT, LocVT, LocInfo, Flags, State);
    return false;
  }
};

} // namespace

RISCVCallLowering::RISCVCallLowering(const RISCVTargetLowering &TLI)
    : CallLowering(&TLI) {}

bool RISCVCallLowering::lowerReturnVal(MachineIRBuilder &MIRBuilder,
                                       const Value *Val,
                                       ArrayRef<Register> VRegs,
                                       MachineInstrBuilder &Ret) const {
  if (!Val)
    return true;

  // TODO: Only integer, pointer and aggregate types are supported now.
  if (!Val->getType()->isIntOrPtrTy() && !Val->getType()->isAggregateType())
    return false;

  MachineFunction &MF = MIRBuilder.getMF();
  const Function &F = MF.getFunction();
  const DataLayout &DL = MF.getDataLayout();
  const RISCVTargetLowering &TLI = *getTLI<RISCVTargetLowering>();

  SmallVector<EVT, 4> SplitEVTs;
  ComputeValueVTs(TLI, DL, Val->getType(), SplitEVTs);
  assert(VRegs.size() == SplitEVTs.size() &&
         "For each split Type there should be exactly one VReg.");

  ArgInfo OrigRetInfo(VRegs, Val->getType());
  setArgFlags(OrigRetInfo, AttributeList::ReturnIndex, DL, F);

  SmallVector<ArgInfo, 4> SplitRetInfos;
  splitToValueTypes(OrigRetInfo, SplitRetInfos, SplitEVTs, MF,
                    [&](ArrayRef<Register> Regs, int SplitIdx) {
                      MIRBuilder.buildUnmerge(Regs, VRegs[SplitIdx]);
                    });

  SmallVector<ISD::OutputArg, 8> Outs;
  setISDArgsForCallingConv(F, OrigRetInfo, SplitEVTs, Outs, true);

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(F.getCallingConv(), F.isVarArg(), MF, ArgLocs, F.getContext());

  TLI.analyzeOutputArgs(MF, CCInfo, Outs, /*IsRet=*/true, nullptr);
  updateArgLocInfo(ArgLocs, Outs);

  OutgoingValueHandler Handler(MIRBuilder, MF.getRegInfo(), Ret, nullptr);
  return handleAssignments(CCInfo, ArgLocs, MIRBuilder, SplitRetInfos, Handler);
}

bool RISCVCallLowering::lowerReturn(MachineIRBuilder &MIRBuilder,
                                    const Value *Val,
                                    ArrayRef<Register> VRegs) const {
  assert(!Val == VRegs.empty() && "Return value without a vreg");

  MachineInstrBuilder Ret = MIRBuilder.buildInstrNoInsert(RISCV::PseudoRET);
  if (!lowerReturnVal(MIRBuilder, Val, VRegs, Ret))
    return false;

  MIRBuilder.insertInstr(Ret);
  return true;
}

bool RISCVCallLowering::lowerFormalArguments(
    MachineIRBuilder &MIRBuilder, const Function &F,
    ArrayRef<ArrayRef<Register>> VRegs) const {

  if (F.arg_empty())
    return true;

  return false;
}

bool RISCVCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                  CallLoweringInfo &Info) const {
  return false;
}

template <typename T>
void RISCVCallLowering::setISDArgsForCallingConv(const Function &F,
                                                 const ArgInfo &OrigArg,
                                                 SmallVectorImpl<EVT> &SplitVTs,
                                                 SmallVectorImpl<T> &ISDArgs,
                                                 bool isRet) const {
  const DataLayout &DL = F.getParent()->getDataLayout();
  LLVMContext &Ctx = F.getContext();
  CallingConv::ID CC = F.getCallingConv();
  const RISCVTargetLowering &TLI = *getTLI<RISCVTargetLowering>();

  for (unsigned i = 0, e = SplitVTs.size(); i != e; ++i) {
    EVT VT = SplitVTs[i];
    Type *SplitTy = VT.getTypeForEVT(Ctx);
    MVT RegisterVT = TLI.getRegisterTypeForCallingConv(Ctx, CC, VT);
    unsigned NumParts = TLI.getNumRegistersForCallingConv(Ctx, CC, VT);

    for (unsigned j = 0; j < NumParts; ++j) {
      auto Flags = OrigArg.Flags[0];

      if (j == 0)
        Flags.setOrigAlign(TLI.getABIAlignmentForCallingConv(SplitTy, DL));
      else
        Flags.setOrigAlign(Align(1));

      ISDArgs.emplace_back(Flags, RegisterVT, VT, true, isRet ? 0 : i, 0);
    }
  }
}

void RISCVCallLowering::splitToValueTypes(const ArgInfo &OrigArg,
                                          SmallVectorImpl<ArgInfo> &SplitArgs,
                                          SmallVectorImpl<EVT> &SplitVTs,
                                          MachineFunction &MF,
                                          SplitArgTy PerformArgSplit) const {
  const RISCVTargetLowering &TLI = *getTLI<RISCVTargetLowering>();
  LLVMContext &Ctx = OrigArg.Ty->getContext();
  CallingConv::ID CC = MF.getFunction().getCallingConv();
  const DataLayout &DL = MF.getDataLayout();

  // Create one ArgInfo for each virtual register in the original ArgInfo.
  for (unsigned i = 0, e = SplitVTs.size(); i != e; ++i) {
    EVT VT = SplitVTs[i];
    Type *SplitTy = VT.getTypeForEVT(Ctx);
    auto Flags = OrigArg.Flags[0];
    Flags.setOrigAlign(Align(DL.getABITypeAlignment(SplitTy)));

    unsigned NumParts = TLI.getNumRegistersForCallingConv(Ctx, CC, VT);
    if (NumParts == 1) {
      SplitArgs.emplace_back(OrigArg.Regs[i], VT.getTypeForEVT(Ctx), Flags,
                             OrigArg.IsFixed);
      continue;
    }

    SmallVector<Register, 8> SplitRegs;

    EVT PartVT = TLI.getRegisterTypeForCallingConv(Ctx, CC, VT);
    Type *PartTy = PartVT.getTypeForEVT(Ctx);

    for (unsigned j = 0; j < NumParts; ++j) {
      ArgInfo Info = ArgInfo{MF.getRegInfo().createGenericVirtualRegister(
                                 getLLTForType(*PartTy, DL)),
                             PartTy, Flags};
      SplitArgs.push_back(Info);
      SplitRegs.push_back(Info.Regs[0]);
    }

    PerformArgSplit(SplitRegs, i);
  }
}

template <typename T>
void RISCVCallLowering::updateArgLocInfo(
    SmallVectorImpl<CCValAssign> &ArgLocs,
    const SmallVectorImpl<T> &Arguments) const {
  for (unsigned i = 0; i < ArgLocs.size(); ++i) {
    const CCValAssign &VA = ArgLocs[i];
    CCValAssign::LocInfo LocInfo = VA.getLocInfo();
    // TODO: LocInfo type for BCvt and Indirect need be changed?
    if (LocInfo == CCValAssign::BCvt || LocInfo == CCValAssign::Indirect)
      continue;

    if (Arguments[i].ArgVT.getSizeInBits() >= Arguments[i].VT.getSizeInBits()) {
      assert(LocInfo == CCValAssign::Full && "Unexpected CCValAssign::LocInfo");
      continue;
    }

    if (Arguments[i].Flags.isSExt())
      LocInfo = CCValAssign::LocInfo::SExt;
    else if (Arguments[i].Flags.isZExt())
      LocInfo = CCValAssign::LocInfo::ZExt;
    else
      LocInfo = CCValAssign::LocInfo::AExt;

    if (VA.isMemLoc())
      ArgLocs[i] =
          CCValAssign::getMem(VA.getValNo(), VA.getValVT(),
                              VA.getLocMemOffset(), VA.getLocVT(), LocInfo);
    else
      ArgLocs[i] = CCValAssign::getReg(VA.getValNo(), VA.getValVT(),
                                       VA.getLocReg(), VA.getLocVT(), LocInfo);
  }
}
