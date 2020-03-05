; RUN: llc -mtriple=riscv32 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV32I %s
; RUN: llc -mtriple=riscv64 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV64I %s

define void @test_ret_void() {
  ; RV32I-LABEL: name: test_ret_void
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_ret_void
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   PseudoRET
entry:
  ret void
}

define i8 @test_ret_i8() {
  ; RV32I-LABEL: name: test_ret_i8
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV32I-NEXT:   [[AEXT:%[0-9]+]]:_(s32) = G_ANYEXT [[CST]](s8)
  ; RV32I-NEXT:   $x10 = COPY [[AEXT]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10

  ; RV64I-LABEL: name: test_ret_i8
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV64I-NEXT:   [[AEXT:%[0-9]+]]:_(s64) = G_ANYEXT [[CST]](s8)
  ; RV64I-NEXT:   $x10 = COPY [[AEXT]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
    ret i8 1
}

define zeroext i8 @test_ret_i8_zext() {
  ; RV32I-LABEL: name: test_ret_i8_zext
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV32I-NEXT:   [[ZEXT:%[0-9]+]]:_(s32) = G_ZEXT [[CST]](s8)
  ; RV32I-NEXT:   $x10 = COPY [[ZEXT]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10

  ; RV64I-LABEL: name: test_ret_i8_zext
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV64I-NEXT:   [[ZEXT:%[0-9]+]]:_(s64) = G_ZEXT [[CST]](s8)
  ; RV64I-NEXT:   $x10 = COPY [[ZEXT]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
    ret i8 1
}

define signext i16 @test_ret_i16_sext() {
  ; RV32I-LABEL: name: test_ret_i16_sext
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV32I-NEXT:   [[SEXT:%[0-9]+]]:_(s32) = G_SEXT [[CST]](s16)
  ; RV32I-NEXT:   $x10 = COPY [[SEXT]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10

  ; RV64I-LABEL: name: test_ret_i16_sext
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV64I-NEXT:   [[SEXT:%[0-9]+]]:_(s64) = G_SEXT [[CST]](s16)
  ; RV64I-NEXT:   $x10 = COPY [[SEXT]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
    ret i16 1
}

define i32 @test_ret_i32() {
  ; RV32I-LABEL: name: test_ret_i32
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV32I-NEXT:   $x10 = COPY [[CST]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10

  ; RV64I-LABEL: name: test_ret_i32
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV64I-NEXT:   [[AEXT:%[0-9]+]]:_(s64) = G_ANYEXT [[CST]](s32)
  ; RV64I-NEXT:   $x10 = COPY [[AEXT]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
  ret i32 1
}

define i64 @test_ret_i64() {
  ; RV32I-LABEL: name: test_ret_i64
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s64) = G_CONSTANT i64 4294967296
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s32), [[CST2:%[0-9]+]]:_(s32) = G_UNMERGE_VALUES [[CST]](s64)
  ; RV32I-NEXT:   $x10 = COPY [[CST1]](s32)
  ; RV32I-NEXT:   $x11 = COPY [[CST2]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10, implicit $x11

  ; RV64I-LABEL: name: test_ret_i64
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s64) = G_CONSTANT i64 4294967296
  ; RV64I-NEXT:   $x10 = COPY [[CST]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
  ret i64 4294967296
}

define i32* @test_ret_i32_ptr() {
  ; RV32I-LABEL: name: test_ret_i32_ptr
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[UDEF:%[0-9]+]]:_(p0) = G_IMPLICIT_DEF
  ; RV32I-NEXT:   $x10 = COPY [[UDEF]](p0)
  ; RV32I-NEXT:   PseudoRET implicit $x10

  ; RV64I-LABEL: name: test_ret_i32_ptr
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[UDEF:%[0-9]+]]:_(p0) = G_IMPLICIT_DEF
  ; RV64I-NEXT:   $x10 = COPY [[UDEF]](p0)
  ; RV64I-NEXT:   PseudoRET implicit $x10
entry:
  ret i32* undef
}

define [2 x i32] @test_ret_2xi32() {
  ; RV32I-LABEL: name: test_ret_2xi32
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   [[CST1:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV32I-NEXT:   [[CST2:%[0-9]+]]:_(s32) = G_CONSTANT i32 2
  ; RV32I-NEXT:   $x10 = COPY [[CST1]](s32)
  ; RV32I-NEXT:   $x11 = COPY [[CST2]](s32)
  ; RV32I-NEXT:   PseudoRET implicit $x10, implicit $x11

  ; RV64I-LABEL: name: test_ret_2xi32
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   [[CST1:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV64I-NEXT:   [[CST2:%[0-9]+]]:_(s32) = G_CONSTANT i32 2
  ; RV64I-NEXT:   [[AEXT1:%[0-9]+]]:_(s64) = G_ANYEXT [[CST1]](s32)
  ; RV64I-NEXT:   $x10 = COPY [[AEXT1]](s64)
  ; RV64I-NEXT:   [[AEXT2:%[0-9]+]]:_(s64) = G_ANYEXT [[CST2]](s32)
  ; RV64I-NEXT:   $x11 = COPY [[AEXT2]](s64)
  ; RV64I-NEXT:   PseudoRET implicit $x10, implicit $x11
entry:
  ret [2 x i32] [i32 1, i32 2]
}
