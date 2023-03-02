//===--- FormatDebug.h - Format C++ code ------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file declares utilities used in clang-format in debug mode.
///
//===----------------------------------------------------------------------===//
#ifndef LLVM_CLANG_LIB_FORMAT_FORMATDEBUG_H
#define LLVM_CLANG_LIB_FORMAT_FORMATDEBUG_H

#include "llvm/Support/Regex.h"

namespace clang {
namespace format {
namespace internal {

#ifndef NDEBUG

extern const llvm::Regex &DebugTraceToken();
extern const llvm::Regex &DebugForceMustBreak();
extern const llvm::Regex &DebugForceMustNotBreak();

#define DEBUG_FORMAT_TRACE_TOKEN(Tok, Debug)                                   \
  if (internal::DebugTraceToken().match((Tok).TokenText)) {                    \
    llvm::dbgs() << __FILE__ << ":" << __LINE__ << ": tracing "                \
                 << (Tok).TokenText << ": " << Debug << "\n";                  \
  } else {                                                                     \
  }

#endif

} // namespace internal
} // namespace format
} // namespace clang

#endif