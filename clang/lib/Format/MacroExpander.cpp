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
/// This file contains the implementation of MacroExpander, which handles macro
/// configuration and expansion while formatting.
///
//===----------------------------------------------------------------------===//

#include "MacroExpander.h"

#include "FormatToken.h"
#include "FormatTokenLexer.h"
#include "clang/Format/Format.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "llvm/Support/ErrorHandling.h"

namespace clang {
namespace format {

struct MacroExpander::Definition {
  StringRef Name;
  SmallVector<FormatToken *, 8> Params;
  SmallVector<FormatToken *, 8> Tokens;
};

// A simple macro parser.
class MacroExpander::DefinitionParser {
public:
  DefinitionParser(ArrayRef<FormatToken *> Tokens) : Tokens(Tokens) {
    assert(!Tokens.empty());
    Current = Tokens[0];
  }

  // Parse the token stream and return the corresonding Defintion object.
  // Returns an empty definition object with a null-Name on error.
  MacroExpander::Definition parse() {
    if (!Current->is(tok::identifier))
      return {};
    Def.Name = Current->TokenText;
    nextToken();
    if (Current->is(tok::l_paren)) {
      if (!parseParams())
        return {};
    }
    parseExpansion();
    return Def;
  }

private:
  bool parseParams() {
    if (Current->isNot(tok::l_paren))
      return false;
    nextToken();
    while (Current->is(tok::identifier)) {
      Def.Params.push_back(Current);
      nextToken();
      if (Current->isNot(tok::comma))
        break;
      nextToken();
    }
    if (Current->isNot(tok::r_paren))
      return false;
    nextToken();
    return true;
  }

  void parseExpansion() {
    do {
      Def.Tokens.push_back(Current);
      nextToken();
    } while (Current->isNot(tok::eof));
    Def.Tokens.push_back(Current);
  }

  void nextToken() {
    if (Pos + 1 < Tokens.size())
      ++Pos;
    Current = Tokens[Pos];
    Current->Finalized = true;
  }

  size_t Pos = 0;
  FormatToken *Current = nullptr;
  Definition Def;
  ArrayRef<FormatToken *> Tokens;
};

MacroExpander::MacroExpander(
    const std::vector<std::string> &Macros, clang::SourceManager &SourceMgr,
    const FormatStyle &Style, encoding::Encoding Encoding,
    llvm::SpecificBumpPtrAllocator<FormatToken> &Allocator,
    IdentifierTable &IdentTable)
    : SourceMgr(SourceMgr), Style(Style), Encoding(Encoding),
      Allocator(Allocator), IdentTable(IdentTable) {
  parseDefinitions(Macros);
}

MacroExpander::~MacroExpander() {}

void MacroExpander::parseDefinitions(
    const std::vector<std::string> &MacroExpander) {
  for (const std::string &Macro : MacroExpander) {
    Buffers.push_back(
        llvm::MemoryBuffer::getMemBufferCopy(Macro, "<scratch space>"));
    clang::FileID FID =
        SourceMgr.createFileID(SourceManager::Unowned, Buffers.back().get());
    FormatTokenLexer Lex(SourceMgr, FID, 0, Style, Encoding, Allocator,
                         IdentTable);
    DefinitionParser Parser(Lex.lex());
    auto Definition = Parser.parse();
    Definitions[Definition.Name] = Definition;
  }
}

bool MacroExpander::defined(llvm::StringRef Name) {
  return Definitions.find(Name) != Definitions.end();
}

llvm::SmallVector<FormatToken *, 8> MacroExpander::expand(FormatToken *ID,
                                                          ArgsList Args) {
  assert(defined(ID->TokenText));
  SmallVector<FormatToken *, 8> Result;
  const Definition &Def = Definitions[ID->TokenText];

  // Map from each argument's name to its position in the argument list.
  // With "M(x, y) x + y":
  //   x -> 0
  //   y -> 1
  llvm::StringMap<size_t> ArgMap;
  for (size_t I = 0, E = Def.Params.size(); I != E; ++I) {
    ArgMap[Def.Params[I]->TokenText] = I;
  }
  bool First = true;

  // Adds the given token to Result.
  auto pushToken = [&](FormatToken *Tok) {
    Tok->MacroCtx.ExpandedFrom.push_back(ID);
    if (First) {
      Tok->MacroCtx.StartOfExpansion = true;
    }
    Result.push_back(Tok);
    First = false;
  };

  // If Tok references a parameter, adds the corresponding argument to Result.
  // Returns false if Tok does not reference a parameter.
  auto expandArgument = [&](FormatToken *Tok) -> bool {
    // If the current token references a parameter, expand the corresponding
    // argument.
    if (!Tok->is(tok::identifier))
      return false;
    auto I = ArgMap.find(Tok->TokenText);
    if (I == ArgMap.end())
      return false;
    // If there are fewer arguments than referenced parameters, skip the
    // parameter.
    // FIXME: Potentially fully abort the expansion instead.
    if (I->getValue() >= Args.size())
      return true;
    for (const auto &Tok : Args[I->getValue()]) {
      // A token can be part of multiple macro arguments.
      // For example, with "ID(x) x":
      // in ID(ID(x)), 'x' is expanded first as argument to the inner
      // ID, then again as argument to the outer ID. We keep the macro
      // role the token had from the inner expansion.
      if (Tok->MacroCtx.Role == MR_None)
        Tok->MacroCtx.Role = MR_ExpandedArg;
      pushToken(Tok);
    }
    return true;
  };

  // Expand the definition into Restlt.
  for (FormatToken *Tok : Definitions[ID->TokenText].Tokens) {
    if (expandArgument(Tok))
      continue;
    // Create a copy of the tokens that were not part of the macro argument,
    // i.e. were not provided by user code.
    FormatToken *New = new (Allocator.Allocate()) FormatToken;
    Tok->copyInto(*New);
    assert(New->MacroCtx.Role == MR_None);
    // Tokens that are not part of the user code do not need to be formatted.
    New->MacroCtx.Role = MR_Hidden;
    pushToken(New);
  }
  assert(Result.size() >= 2);
  ++Result[Result.size() - 2]->MacroCtx.EndOfExpansion;
  return Result;
}

} // namespace format
} // namespace clang
