; RUN: llc -mtriple=riscv32 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV32I %s
; RUN: llc -mtriple=riscv64 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV64I %s

declare void @void_noargs()

define void @test_call_void_noargs() {
  ; RV32I-LABEL: name: test_call_void_noargs
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_noargs, implicit-def $x1
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_noargs, implicit-def $x1
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_noargs()
  ret void
}

declare void @void_args_i8(i8, i8)

define void @test_call_void_args_i8() {
  ; RV32I-LABEL: name: test_call_void_args_i8
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s8) = G_CONSTANT i8 0
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV32I-NEXT:   [[AEXT1:%[0-9]+]]:_(s32) = G_ANYEXT [[CST1]](s8)
  ; RV32I-NEXT:   $x10 = COPY [[AEXT1]](s32)
  ; RV32I-NEXT:   [[AEXT2:%[0-9]+]]:_(s32) = G_ANYEXT [[CST2]](s8)
  ; RV32I-NEXT:   $x11 = COPY [[AEXT2]](s32)
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i8, implicit-def $x1, implicit $x10, implicit $x11
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_args_i8
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s8) = G_CONSTANT i8 0
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV64I-NEXT:   [[AEXT1:%[0-9]+]]:_(s64) = G_ANYEXT [[CST1]](s8)
  ; RV64I-NEXT:   $x10 = COPY [[AEXT1]](s64)
  ; RV64I-NEXT:   [[AEXT2:%[0-9]+]]:_(s64) = G_ANYEXT [[CST2]](s8)
  ; RV64I-NEXT:   $x11 = COPY [[AEXT2]](s64)
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i8, implicit-def $x1, implicit $x10, implicit $x11
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_args_i8(i8 0, i8 1)
  ret void
}

declare void @void_args_i8_zext(i8 zeroext, i8 zeroext)

define void @test_call_void_args_i8_zext() {
  ; RV32I-LABEL: name: test_call_void_args_i8_zext
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s8) = G_CONSTANT i8 0
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV32I-NEXT:   [[ZEXT1:%[0-9]+]]:_(s32) = G_ZEXT [[CST1]](s8)
  ; RV32I-NEXT:   $x10 = COPY [[ZEXT1]](s32)
  ; RV32I-NEXT:   [[ZEXT2:%[0-9]+]]:_(s32) = G_ZEXT [[CST2]](s8)
  ; RV32I-NEXT:   $x11 = COPY [[ZEXT2]](s32)
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i8_zext, implicit-def $x1, implicit $x10, implicit $x11
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_args_i8_zext
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s8) = G_CONSTANT i8 0
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV64I-NEXT:   [[ZEXT1:%[0-9]+]]:_(s64) = G_ZEXT [[CST1]](s8)
  ; RV64I-NEXT:   $x10 = COPY [[ZEXT1]](s64)
  ; RV64I-NEXT:   [[ZEXT2:%[0-9]+]]:_(s64) = G_ZEXT [[CST2]](s8)
  ; RV64I-NEXT:   $x11 = COPY [[ZEXT2]](s64)
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i8_zext, implicit-def $x1, implicit $x10, implicit $x11
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_args_i8_zext(i8 zeroext 0, i8 zeroext 1)
  ret void
}

declare void @void_args_i16_sext(i16 signext, i16 signext)

define void @test_call_void_args_i16_sext() {
  ; RV32I-LABEL: name: test_call_void_args_i16_sext
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s16) = G_CONSTANT i16 0
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV32I-NEXT:   [[SEXT1:%[0-9]+]]:_(s32) = G_SEXT [[CST1]](s16)
  ; RV32I-NEXT:   $x10 = COPY [[SEXT1]](s32)
  ; RV32I-NEXT:   [[SEXT2:%[0-9]+]]:_(s32) = G_SEXT [[CST2]](s16)
  ; RV32I-NEXT:   $x11 = COPY [[SEXT2]](s32)
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i16_sext, implicit-def $x1, implicit $x10, implicit $x11
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_args_i16_sext
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s16) = G_CONSTANT i16 0
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV64I-NEXT:   [[SEXT1:%[0-9]+]]:_(s64) = G_SEXT [[CST1]](s16)
  ; RV64I-NEXT:   $x10 = COPY [[SEXT1]](s64)
  ; RV64I-NEXT:   [[SEXT2:%[0-9]+]]:_(s64) = G_SEXT [[CST2]](s16)
  ; RV64I-NEXT:   $x11 = COPY [[SEXT2]](s64)
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i16_sext, implicit-def $x1, implicit $x10, implicit $x11
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_args_i16_sext(i16 signext 0, i16 signext 1)
  ret void
}

declare void @void_args_i32(i32, i32)

define void @test_call_void_args_i32() {
  ; RV32I-LABEL: name: test_call_void_args_i32
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s32) = G_CONSTANT i32 0
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV32I-NEXT:   $x10 = COPY [[CST1]](s32)
  ; RV32I-NEXT:   $x11 = COPY [[CST2]](s32)
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i32, implicit-def $x1, implicit $x10, implicit $x11
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_args_i32
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s32) = G_CONSTANT i32 0
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV64I-NEXT:   [[AEXT1:%[0-9]+]]:_(s64) = G_ANYEXT [[CST1]](s32)
  ; RV64I-NEXT:   $x10 = COPY [[AEXT1]](s64)
  ; RV64I-NEXT:   [[AEXT2:%[0-9]+]]:_(s64) = G_ANYEXT [[CST2]](s32)
  ; RV64I-NEXT:   $x11 = COPY [[AEXT2]](s64)
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i32, implicit-def $x1, implicit $x10, implicit $x11
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_args_i32(i32 0, i32 1)
  ret void
}

declare void @void_args_i64(i64, i64)

define void @test_call_void_args_i64() {
  ; RV32I-LABEL: name: test_call_void_args_i64
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s64) = G_CONSTANT i64 0
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s64) = G_CONSTANT i64 1
  ; RV32I-NEXT:   [[CST1_1:%[0-9]+]]:_(s32), [[CST1_2:%[0-9]+]]:_(s32) = G_UNMERGE_VALUES [[CST1]](s64)
  ; RV32I-NEXT:   [[CST2_1:%[0-9]+]]:_(s32), [[CST2_2:%[0-9]+]]:_(s32) = G_UNMERGE_VALUES [[CST2]](s64)
  ; RV32I-NEXT:   $x10 = COPY [[CST1_1]](s32)
  ; RV32I-NEXT:   $x11 = COPY [[CST1_2]](s32)
  ; RV32I-NEXT:   $x12 = COPY [[CST2_1]](s32)
  ; RV32I-NEXT:   $x13 = COPY [[CST2_2]](s32)
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i64, implicit-def $x1, implicit $x10, implicit $x11, implicit $x12, implicit $x13
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_void_args_i64
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s64) = G_CONSTANT i64 0
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s64) = G_CONSTANT i64 1
  ; RV64I-NEXT:   $x10 = COPY [[CST1]](s64)
  ; RV64I-NEXT:   $x11 = COPY [[CST2]](s64)
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @void_args_i64, implicit-def $x1, implicit $x10, implicit $x11
  ; RV64I-NEXT:   PseudoRET
entry:
  call void @void_args_i64(i64 0, i64 1)
  ret void
}

declare i8 @i8_noargs()

define void @test_call_i8_noargs() {
  ; RV32I-LABEL: name: test_call_i8_noargs
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @i8_noargs, implicit-def $x1
  ; RV32I-NEXT:   [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_i8_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @i8_noargs, implicit-def $x1
  ; RV64I-NEXT:   [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   PseudoRET
entry:
  %a = call i8 @i8_noargs()
  ret void
}

declare i16 @i16_noargs()

define void @test_call_i16_noargs() {
  ; RV32I-LABEL: name: test_call_i16_noargs
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @i16_noargs, implicit-def $x1
  ; RV32I-NEXT:   [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_i16_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @i16_noargs, implicit-def $x1
  ; RV64I-NEXT:   [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   PseudoRET
entry:
  %a = call i16 @i16_noargs()
  ret void
}

declare i32 @i32_noargs()

define void @test_call_i32_noargs() {
  ; RV32I-LABEL: name: test_call_i32_noargs
  ; RV32I:        PseudoCALL target-flags(riscv-call) @i32_noargs, implicit-def $x1
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_i32_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @i32_noargs, implicit-def $x1
  ; RV64I-NEXT:   [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s32) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   PseudoRET
entry:
  %a = call i32 @i32_noargs()
  ret void
}

declare i64 @i64_noargs()

define void @test_call_i64_noargs() {
  ; RV32I-LABEL: name: test_call_i64_noargs
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @i64_noargs, implicit-def $x1
  ; RV32I-NEXT:   [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s64) = G_MERGE_VALUES [[VREG1]](s32), [[VREG2]](s32)
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_i64_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @i64_noargs, implicit-def $x1
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   PseudoRET
entry:
  %a = call i64 @i64_noargs()
  ret void
}

declare i32* @i32_ptr_noargs()

define void @test_call_i32_ptr_noargs() {
entry:
  ; RV32I-LABEL: name: test_call_i32_ptr_noargs
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoCALL target-flags(riscv-call) @i32_ptr_noargs, implicit-def $x1
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(p0) = COPY $x10
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_call_i32_ptr_noargs
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoCALL target-flags(riscv-call) @i32_ptr_noargs, implicit-def $x1
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(p0) = COPY $x10
  ; RV64I-NEXT:   PseudoRET
  %a = call i32* @i32_ptr_noargs()
  ret void
}
