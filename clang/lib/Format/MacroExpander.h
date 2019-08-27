//===--- MacroExpander.h - Format C++ code ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the declaration of MacroExpander, which handles macro
/// configuration and expansion while formatting.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_FORMAT_MACRO_EXPANDER_H
#define LLVM_CLANG_LIB_FORMAT_MACRO_EXPANDER_H

#include <string>
#include <unordered_map>
#include <vector>

#include "Encoding.h"
#include "FormatToken.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
class MemoryBuffer;
}

namespace clang {
class IdentifierTable;
class SourceManager;

namespace format {
struct FormatStyle;

/// Takes a set of simple macro definitions as strings and allows expanding
/// calls to those macros.
class MacroExpander {
public:
  typedef llvm::ArrayRef<llvm::SmallVector<FormatToken *, 8>> ArgsList;

  /// Construct a macro expander from a set of macro definitions.
  ///
  /// Each entry in \p Macros must conform to the following simple
  /// macro-definition language:
  /// <def>    ::= <id> <exp> | <id> "(" <params> ") <exp>
  /// <params> ::= <id> | <id> "," <params>
  /// <exp>    ::= <eof> | <tok> <exp>
  ///
  MacroExpander(const std::vector<std::string> &Macros,
                clang::SourceManager &SourceMgr, const FormatStyle &Style,
                encoding::Encoding encoding,
                llvm::SpecificBumpPtrAllocator<FormatToken> &Allocator,
                IdentifierTable &IdentTable);
  ~MacroExpander();

  /// Returns whether a macro \p Name is defined.
  bool defined(llvm::StringRef Name);

  /// Returns the expanded stream of format tokens for \p ID, where
  /// each element in \p Args is a positional argument to the macro call.
  llvm::SmallVector<FormatToken *, 8> expand(FormatToken *ID, ArgsList Args);

private:
  struct Definition;
  class DefinitionParser;

  void parseDefinitions(const std::vector<std::string> &MacroExpander);

  clang::SourceManager &SourceMgr;
  const FormatStyle &Style;
  encoding::Encoding Encoding;
  llvm::SpecificBumpPtrAllocator<FormatToken> &Allocator;
  IdentifierTable &IdentTable;
  std::vector<std::unique_ptr<llvm::MemoryBuffer>> Buffers;
  llvm::StringMap<Definition> Definitions;
};

} // namespace format
} // namespace clang

#endif
