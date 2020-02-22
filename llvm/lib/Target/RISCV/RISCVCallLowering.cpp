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

struct IncomingValueHandler : public CallLowering::ValueHandler {
  IncomingValueHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                       CCAssignFn *AssignFn)
      : ValueHandler(B, MRI, AssignFn) {}

  bool isIncomingArgumentHandler() const override { return true; }

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
    // Copy argument received in physical register to desired VReg.
    MIRBuilder.getMBB().addLiveIn(PhysReg);
    MIRBuilder.buildCopy(ValVReg, PhysReg);
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

struct CallReturnHandler : public IncomingValueHandler {
  CallReturnHandler(MachineIRBuilder &B, MachineRegisterInfo &MRI,
                    CCAssignFn *AssignFn, MachineInstrBuilder &MIB)
      : IncomingValueHandler(B, MRI, AssignFn), MIB(MIB) {}

  MachineInstrBuilder MIB;

  void assignValueToReg(Register ValVReg, Register PhysReg,
                        CCValAssign &VA) override {
    // Copy argument received in physical register to desired VReg.
    MIB.addDef(PhysReg, RegState::Implicit);
    MIRBuilder.buildCopy(ValVReg, PhysReg);
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
  setISDArgsForCallingConv(F, OrigRetInfo, SplitEVTs, Outs, F.getCallingConv(),
                           true);

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

  // Early exit if there are no arguments.
  if (F.arg_empty())
    return true;

  // TODO: Support vararg functions.
  if (F.isVarArg())
    return false;

  // TODO: Support all argument types.
  for (auto &Arg : F.args()) {
    if (Arg.getType()->isIntegerTy())
      continue;
    if (Arg.getType()->isPointerTy())
      continue;
    return false;
  }

  MachineFunction &MF = MIRBuilder.getMF();
  const DataLayout &DL = MF.getDataLayout();
  const RISCVTargetLowering &TLI = *getTLI<RISCVTargetLowering>();

  SmallVector<ArgInfo, 32> SplitArgInfos;
  SmallVector<ISD::InputArg, 8> Ins;
  unsigned Index = 0;
  for (auto &Arg : F.args()) {
    // Construct the ArgInfo object from destination register and argument type.
    ArgInfo AInfo(VRegs[Index], Arg.getType());
    setArgFlags(AInfo, Index + AttributeList::FirstArgIndex, DL, F);

    SmallVector<EVT, 4> SplitEVTs;
    ComputeValueVTs(TLI, DL, Arg.getType(), SplitEVTs);
    assert(VRegs[Index].size() == SplitEVTs.size() &&
           "For each split Type there should be exactly one VReg.");

    setISDArgsForCallingConv(F, AInfo, SplitEVTs, Ins, F.getCallingConv(),
                             /*isRet=*/false);

    // Handle any required merging from split value types - as indicated in
    // SplitEVTs - from physical registers into the desired VReg. ArgInfo
    // objects are constructed correspondingly and appended to SplitArgInfos.
    splitToValueTypes(AInfo, SplitArgInfos, SplitEVTs, MF,
                      [&](ArrayRef<Register> Regs, int SplitIdx) {
                        auto MIB =
                            MIRBuilder.buildMerge(VRegs[Index][SplitIdx], Regs);
                        MIRBuilder.setInstr(*MIB.getInstr());
                      });

    ++Index;
  }

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(F.getCallingConv(), F.isVarArg(), MF, ArgLocs, F.getContext());

  // TODO: Access CC_RISCV_FastCC when using CallingConv::Fast. Preferrably
  // TLI.CCAssignFnForCall will be implemented and the approach for both CCs
  // will be unified.
  if (F.getCallingConv() == CallingConv::Fast)
    return false;
  TLI.analyzeInputArgs(MF, CCInfo, Ins, /*IsRet=*/false);

  IncomingValueHandler Handler(MIRBuilder, MF.getRegInfo(), nullptr);

  if (!handleAssignments(CCInfo, ArgLocs, MIRBuilder, SplitArgInfos, Handler))
    return false;

  return true;
}

bool RISCVCallLowering::lowerCall(MachineIRBuilder &MIRBuilder,
                                  CallLoweringInfo &Info) const {

  MachineFunction &MF = MIRBuilder.getMF();
  const Function &F = MF.getFunction();
  const DataLayout &DL = MF.getDataLayout();
  const RISCVTargetLowering &TLI = *getTLI<RISCVTargetLowering>();

  // TODO: Support vararg functions.
  if (Info.IsVarArg)
    return false;

  // TODO: Support all argument types.
  for (auto &AInfo : Info.OrigArgs) {
    if (AInfo.Ty->isIntegerTy())
      continue;
    if (AInfo.Ty->isPointerTy())
      continue;
    if (AInfo.Ty->isFloatingPointTy())
      continue;
    return false;
  }

  SmallVector<ArgInfo, 32> SplitArgInfos;
  SmallVector<ISD::OutputArg, 8> Outs;
  unsigned Index = 0;
  for (auto &AInfo : Info.OrigArgs) {
    SmallVector<EVT, 4> SplitEVTs;
    ComputeValueVTs(TLI, DL, AInfo.Ty, SplitEVTs);
    assert(AInfo.Regs.size() == SplitEVTs.size() &&
           "For each split Type there should be exactly one VReg.");

    setISDArgsForCallingConv(F, AInfo, SplitEVTs, Outs, Info.CallConv,
                             /*isRet=*/false);

    // Handle any required unmerging of split value types - as indicated in
    // SplitEVTs - from a given VReg into physical registers. ArgInfo objects
    // are constructed correspondingly and appended to SplitArgInfos.
    splitToValueTypes(AInfo, SplitArgInfos, SplitEVTs, MF,
                      [&](ArrayRef<Register> Regs, int SplitIdx) {
                        MIRBuilder.buildUnmerge(Regs, AInfo.Regs[SplitIdx]);
                      });

    ++Index;
  }

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(Info.CallConv, Info.IsVarArg, MF, ArgLocs, F.getContext());

  // TODO: Access CC_RISCV_FastCC when using CallingConv::Fast. Preferrably
  // TLI.CCAssignFnForCall will be implemented and the approach for both CCs
  // will be unified.
  if (Info.CallConv == CallingConv::Fast)
    return false;
  TLI.analyzeOutputArgs(MF, CCInfo, Outs, /*IsRet=*/false, nullptr);
  updateArgLocInfo(ArgLocs, Outs);

  if (!Info.Callee.isReg())
    Info.Callee.setTargetFlags(RISCVII::MO_CALL);

  MachineInstrBuilder Call =
      MIRBuilder
          .buildInstrNoInsert(Info.Callee.isReg() ? RISCV::PseudoCALLIndirect
                                                  : RISCV::PseudoCALL)
          .add(Info.Callee);

  OutgoingValueHandler ArgHandler(MIRBuilder, MF.getRegInfo(), Call, nullptr);
  if (!handleAssignments(CCInfo, ArgLocs, MIRBuilder, SplitArgInfos,
                         ArgHandler))
    return false;

  MIRBuilder.insertInstr(Call);

  if (Info.OrigRet.Ty->isVoidTy())
    return true;

  // TODO: Only integer, pointer and aggregate types are supported now.
  if (!Info.OrigRet.Ty->isIntOrPtrTy() && !Info.OrigRet.Ty->isAggregateType())
    return false;

  SmallVector<EVT, 4> SplitRetEVTs;
  ComputeValueVTs(TLI, DL, Info.OrigRet.Ty, SplitRetEVTs);
  assert(Info.OrigRet.Regs.size() == SplitRetEVTs.size() &&
         "For each split Type there should be exactly one VReg.");

  SmallVector<ArgInfo, 4> SplitRetInfos;
  splitToValueTypes(Info.OrigRet, SplitRetInfos, SplitRetEVTs, MF,
                    [&](ArrayRef<Register> Regs, int SplitIdx) {
                      MIRBuilder.buildMerge(Info.OrigRet.Regs[SplitIdx], Regs);
                    });

  SmallVector<ISD::InputArg, 8> Ins;
  setISDArgsForCallingConv(F, Info.OrigRet, SplitRetEVTs, Ins, Info.CallConv,
                           /*isRet=*/true);

  SmallVector<CCValAssign, 16> RVLocs;
  CCState RetCCInfo(Info.CallConv, Info.IsVarArg, MF, RVLocs, F.getContext());

  TLI.analyzeInputArgs(MF, RetCCInfo, Ins, /*IsRet=*/true);
  updateArgLocInfo(RVLocs, Ins);

  // Assignments should be handled *before* the merging of values takes place.
  // To ensure this, the insert point is temporarily adjusted to just after the
  // call instruction.
  MachineBasicBlock::iterator CallInsertPt = Call;
  MIRBuilder.setInsertPt(MIRBuilder.getMBB(), std::next(CallInsertPt));

  CallReturnHandler Handler(MIRBuilder, MF.getRegInfo(), nullptr, Call);
  if (!handleAssignments(RetCCInfo, RVLocs, MIRBuilder, SplitRetInfos, Handler))
    return false;

  // Readjust insert point to end of basic block.
  MIRBuilder.setMBB(MIRBuilder.getMBB());

  return true;
}

template <typename T>
void RISCVCallLowering::setISDArgsForCallingConv(
    const Function &F, const ArgInfo &OrigArg, SmallVectorImpl<EVT> &SplitVTs,
    SmallVectorImpl<T> &ISDArgs, CallingConv::ID CC, bool isRet) const {
  const DataLayout &DL = F.getParent()->getDataLayout();
  LLVMContext &Ctx = F.getContext();
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
