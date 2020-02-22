//===-- RISCVCallLowering.h - Call lowering ---------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
/// \file
/// This file describes how to lower LLVM calls to machine code calls.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_RISCV_RISCVCALLLOWERING_H
#define LLVM_LIB_TARGET_RISCV_RISCVCALLLOWERING_H

#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/ValueTypes.h"

namespace llvm {

class RISCVTargetLowering;
class MachineInstrBuilder;
class MachineIRBuilder;

class RISCVCallLowering : public CallLowering {

public:
  RISCVCallLowering(const RISCVTargetLowering &TLI);

  bool lowerReturn(MachineIRBuilder &MIRBuiler, const Value *Val,
                   ArrayRef<Register> VRegs) const override;

  bool lowerFormalArguments(MachineIRBuilder &MIRBuilder, const Function &F,
                            ArrayRef<ArrayRef<Register>> VRegs) const override;

  bool lowerCall(MachineIRBuilder &MIRBuilder,
                 CallLoweringInfo &Info) const override;

private:
  bool lowerReturnVal(MachineIRBuilder &MIRBuilder, const Value *Val,
                      ArrayRef<Register> VRegs, MachineInstrBuilder &Ret) const;

  /// A function of this type is used to perform value split action.
  using SplitArgTy = std::function<void(ArrayRef<Register>, int)>;

  template <typename T>
  void setISDArgsForCallingConv(const Function &F, const ArgInfo &OrigArg,
                                SmallVectorImpl<EVT> &SplitVTs,
                                SmallVectorImpl<T> &ISDArgs, CallingConv::ID CC,
                                bool isRet) const;

  void splitToValueTypes(const ArgInfo &OrigArg,
                         SmallVectorImpl<ArgInfo> &SplitArgs,
                         SmallVectorImpl<EVT> &SplitVTs, MachineFunction &MF,
                         SplitArgTy PerformArgSplit) const;

  template <typename T>
  void updateArgLocInfo(SmallVectorImpl<CCValAssign> &ArgLocs,
                        const SmallVectorImpl<T> &Arguments) const;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_RISCV_RISCVCALLLOWERING_H
