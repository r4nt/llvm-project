; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -instcombine -S < %s | FileCheck %s

declare double @llvm.fabs.f64(double)
declare float @llvm.fabs.f32(float)
declare void @use(double)

declare double @llvm.copysign.f64(double, double)
declare float @llvm.copysign.f32(float, float)

define double @fabs_copysign(double %x) {
; CHECK-LABEL: @fabs_copysign(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[X:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv nnan ninf double [[X]], [[F]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %x)
  %div = fdiv nnan ninf double %x, %f
  ret double %div
}

define double @fabs_copysign_commuted(double %x) {
; CHECK-LABEL: @fabs_copysign_commuted(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[X:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv nnan ninf double [[F]], [[X]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %x)
  %div = fdiv nnan ninf double %f, %x
  ret double %div
}


define float @fabs_copysignf(float %x) {
; CHECK-LABEL: @fabs_copysignf(
; CHECK-NEXT:    [[F:%.*]] = tail call float @llvm.fabs.f32(float [[X:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv nnan ninf float [[X]], [[F]]
; CHECK-NEXT:    ret float [[DIV]]
;
  %f = tail call float @llvm.fabs.f32(float %x)
  %div = fdiv nnan ninf float %x, %f
  ret float %div
}

define double @fabs_copysign_use(double %x) {
; CHECK-LABEL: @fabs_copysign_use(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[X:%.*]])
; CHECK-NEXT:    call void @use(double [[F]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv nnan ninf double [[X]], [[F]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %x)
  call void @use(double %f)
  %div = fdiv nnan ninf double %x, %f
  ret double %div
}

; Negative tests

define double @fabs_copysign_mismatch(double %x, double %y) {
; CHECK-LABEL: @fabs_copysign_mismatch(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[Y:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv double [[X:%.*]], [[F]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %y)
  %div = fdiv double %x, %f
  ret double %div
}

define double @fabs_copysign_commuted_mismatch(double %x, double %y) {
; CHECK-LABEL: @fabs_copysign_commuted_mismatch(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[Y:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv double [[F]], [[X:%.*]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %y)
  %div = fdiv double %f, %x
  ret double %div
}

define double @fabs_copysign_no_nnan(double %x) {
; CHECK-LABEL: @fabs_copysign_no_nnan(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[X:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv ninf double [[X]], [[F]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %x)
  %div = fdiv ninf double %x, %f
  ret double %div
}

define double @fabs_copysign_no_ninf(double %x) {
; CHECK-LABEL: @fabs_copysign_no_ninf(
; CHECK-NEXT:    [[F:%.*]] = tail call double @llvm.fabs.f64(double [[X:%.*]])
; CHECK-NEXT:    [[DIV:%.*]] = fdiv nnan double [[X]], [[F]]
; CHECK-NEXT:    ret double [[DIV]]
;
  %f = tail call double @llvm.fabs.f64(double %x)
  %div = fdiv nnan double %x, %f
  ret double %div
}
