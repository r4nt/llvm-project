//===--- FormatDebug.cpp - Format C++ code ----------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements utilities used in clang-format in debug mode.
///
//===----------------------------------------------------------------------===//

#include "FormatDebug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Regex.h"

namespace clang {
namespace format {
namespace internal {

#ifndef NDEBUG

namespace {

llvm::cl::opt<std::string>
    DebugTraceTokenRe("debug-trace-token",
                      llvm::cl::desc("Regular expression to trace formatting "
                                     "decisions of tokens whose text matches."),
                      llvm::cl::Hidden);

llvm::cl::opt<std::string> DebugForceMustBreakRe(
    "debug-force-must-break",
    llvm::cl::desc("Regular expression to force line breaks before tokens "
                   "whose text matches."),
    llvm::cl::Hidden);

llvm::cl::opt<std::string> DebugForceMustNotBreakRe(
    "debug-force-must-not-break",
    llvm::cl::desc("Regular expression to force not breaking before tokens "
                   "whose text matches."),
    llvm::cl::Hidden);

} // namespace

const llvm::Regex &DebugTraceToken() {
  static llvm::Regex R(DebugTraceTokenRe);
  return R;
}

const llvm::Regex &DebugForceMustBreak() {
  static llvm::Regex R(DebugForceMustBreakRe);
  return R;
}

const llvm::Regex &DebugForceMustNotBreak() {
  static llvm::Regex R(DebugForceMustNotBreakRe);
  return R;
}

#endif

} // namespace internal
} // namespace format
} // namespace clang