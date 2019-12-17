; RUN: llc -mtriple=riscv32 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV32I %s
; RUN: llc -mtriple=riscv64 -global-isel -stop-after=irtranslator -verify-machineinstrs < %s \
; RUN:   | FileCheck -check-prefix=RV64I %s

define void @test_args_i8(i8 %a) {
  ; RV32I-LABEL: name: test_args_i8
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s8) = G_ADD [[VREG]], [[CST]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_i8
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s8) = G_CONSTANT i8 1
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s8) = G_ADD [[VREG]], [[CST]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i8 %a, 1
  ret void
}

define void @test_args_i16(i16 %a) {
  ; RV32I-LABEL: name: test_args_i16
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s16) = G_ADD [[VREG]], [[CST]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_i16
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s16) = G_CONSTANT i16 1
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s16) = G_ADD [[VREG]], [[CST]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i16 %a, 1
  ret void
}

define void @test_args_i32(i32 %a) {
  ; RV32I-LABEL: name: test_args_i32
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10
  ; RV32I:        [[VREG:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s32) = G_ADD [[VREG]], [[CST]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_i32
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s32) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s32) = G_CONSTANT i32 1
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s32) = G_ADD [[VREG]], [[CST]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i32 %a, 1
  ret void
}

define void @test_args_i64(i64 %a) {
  ; RV32I-LABEL: name: test_args_i64
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10, $x11
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   [[CST:%[0-9]+]]:_(s64) = G_CONSTANT i64 1
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s64) = G_MERGE_VALUES [[VREG1]](s32), [[VREG2]](s32)
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s64) = G_ADD [[VREG]], [[CST]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_i64
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[VREG:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[CST:%[0-9]+]]:_(s64) = G_CONSTANT i64 1
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s64) = G_ADD [[VREG]], [[CST]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i64 %a, 1
  ret void
}

define void @test_args_i8_ptr(i8* %a) {
  ; RV32I-LABEL: name: test_args_i8_ptr
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10
  ; RV32I:        [[PTR:%[0-9]+]]:_(p0) = COPY $x10
  ; RV32I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_LOAD [[PTR]](p0) :: (load 1 from %ir.a)
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_i8_ptr
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[PTR:%[0-9]+]]:_(p0) = COPY $x10
  ; RV64I-NEXT:   [[VREG:%[0-9]+]]:_(s8) = G_LOAD [[PTR]](p0) :: (load 1 from %ir.a)
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = load i8, i8* %a
  ret void
}

define void @test_args_2xi8(i8 %a, i8 %b) {
  ; RV32I-LABEL: name: test_args_2xi8
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10, $x11
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[TVREG1:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   [[TVREG2:%[0-9]+]]:_(s8) = G_TRUNC [[VREG2]](s32)
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s8) = G_ADD [[TVREG1]], [[TVREG2]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_2xi8
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10, $x11
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[TVREG1:%[0-9]+]]:_(s8) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[VREG2:%[0-9]+]]:_(s64) = COPY $x11
  ; RV64I-NEXT:   [[TVREG2:%[0-9]+]]:_(s8) = G_TRUNC [[VREG2]](s64)
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s8) = G_ADD [[TVREG1]], [[TVREG2]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i8 %a, %b
  ret void
}

define void @test_args_2xi16(i16 %a, i16 %b) {
  ; RV32I-LABEL: name: test_args_2xi16
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10, $x11
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[TVREG1:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s32)
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   [[TVREG2:%[0-9]+]]:_(s16) = G_TRUNC [[VREG2]](s32)
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s16) = G_ADD [[TVREG1]], [[TVREG2]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_2xi16
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10, $x11
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[TVREG1:%[0-9]+]]:_(s16) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[VREG2:%[0-9]+]]:_(s64) = COPY $x11
  ; RV64I-NEXT:   [[TVREG2:%[0-9]+]]:_(s16) = G_TRUNC [[VREG2]](s64)
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s16) = G_ADD [[TVREG1]], [[TVREG2]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i16 %a, %b
  ret void
}

define void @test_args_2xi32(i32 %a, i32 %b) {
  ; RV32I-LABEL: name: test_args_2xi32
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10, $x11
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s32) = G_ADD [[VREG1]], [[VREG2]]

  ; RV64I-LABEL: name: test_args_2xi32
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10, $x11
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[TVREG1:%[0-9]+]]:_(s32) = G_TRUNC [[VREG1]](s64)
  ; RV64I-NEXT:   [[VREG2:%[0-9]+]]:_(s64) = COPY $x11
  ; RV64I-NEXT:   [[TVREG2:%[0-9]+]]:_(s32) = G_TRUNC [[VREG2]](s64)
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s32) = G_ADD [[TVREG1]], [[TVREG2]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i32 %a, %b
  ret void
}

define void @test_args_2xi64(i64 %a, i64 %b) {
  ; RV32I-LABEL: name: test_args_2xi64
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10, $x11, $x12, $x13
  ; RV32I:        [[VREG1:%[0-9]+]]:_(s32) = COPY $x10
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s32) = COPY $x11
  ; RV32I-NEXT:   [[VREG3:%[0-9]+]]:_(s32) = COPY $x12
  ; RV32I-NEXT:   [[VREG4:%[0-9]+]]:_(s32) = COPY $x13
  ; RV32I-NEXT:   [[EVREG2:%[0-9]+]]:_(s64) = G_MERGE_VALUES [[VREG3]](s32), [[VREG4]](s32)
  ; RV32I-NEXT:   [[EVREG1:%[0-9]+]]:_(s64) = G_MERGE_VALUES [[VREG1]](s32), [[VREG2]](s32)
  ; RV32I-NEXT:   {{%[0-9]+}}:_(s64) = G_ADD [[EVREG1]], [[EVREG2]]
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_2xi64
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10, $x11
  ; RV64I:        [[VREG1:%[0-9]+]]:_(s64) = COPY $x10
  ; RV64I-NEXT:   [[VREG2:%[0-9]+]]:_(s64) = COPY $x11
  ; RV64I-NEXT:   {{%[0-9]+}}:_(s64) = G_ADD [[VREG1]], [[VREG2]]
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = add i64 %a, %b
  ret void
}

define void @test_args_2xi8_ptr(i8* %a, i8* %b) {
  ; RV32I-LABEL: name: test_args_2xi8_ptr
  ; RV32I: bb.1.entry:
  ; RV32I-NEXT:   liveins: $x10
  ; RV32I:        [[PTR1:%[0-9]+]]:_(p0) = COPY $x10
  ; RV32I-NEXT:   [[PTR2:%[0-9]+]]:_(p0) = COPY $x11
  ; RV32I-NEXT:   [[VREG1:%[0-9]+]]:_(s8) = G_LOAD [[PTR1]](p0) :: (load 1 from %ir.a)
  ; RV32I-NEXT:   [[VREG2:%[0-9]+]]:_(s8) = G_LOAD [[PTR2]](p0) :: (load 1 from %ir.b)
  ; RV32I-NEXT:   PseudoRET

  ; RV64I-LABEL: name: test_args_2xi8_ptr
  ; RV64I: bb.1.entry:
  ; RV64I-NEXT:   liveins: $x10
  ; RV64I:        [[PTR1:%[0-9]+]]:_(p0) = COPY $x10
  ; RV64I-NEXT:   [[PTR2:%[0-9]+]]:_(p0) = COPY $x11
  ; RV64I-NEXT:   [[VREG1:%[0-9]+]]:_(s8) = G_LOAD [[PTR1]](p0) :: (load 1 from %ir.a)
  ; RV64I-NEXT:   [[VREG2:%[0-9]+]]:_(s8) = G_LOAD [[PTR2]](p0) :: (load 1 from %ir.b)
  ; RV64I-NEXT:   PseudoRET
entry:
  %0 = load i8, i8* %a
  %1 = load i8, i8* %b
  ret void
}
