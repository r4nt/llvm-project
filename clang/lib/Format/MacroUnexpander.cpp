//===--- MacroUnexpander.h - Format C++ code -------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the implementation of MacroUnexpander, which fits an
/// unexpanded macro call to a parsed set of UnwrappedLines.
///
//===----------------------------------------------------------------------===//

#include "MacroUnexpander.h"

#include "FormatToken.h"
#include "UnwrappedLineParser.h"
#include "clang/Basic/TokenKinds.h"
#include "llvm/Support/Debug.h"
#include <map>

#define DEBUG_TYPE "unexpand"

namespace clang {
namespace format {

Unexpander::Unexpander(
    unsigned Level,
    const std::map<FormatToken *, std::unique_ptr<UnwrappedLine>> &Unexpanded)
    : Level(Level), IdToUnexpanded(Unexpanded) {
  Output.Tokens.push_back(std::make_unique<LineNode>());
  Lines.push_back(&Output);
}

void Unexpander::addLine(const UnwrappedLine &Line) {
  assert(State != Finalized);
  forEachToken(Line, [&](FormatToken *Token, FormatToken *Parent, bool First) {
    add(Token, Parent, First);
  });
}

UnwrappedLine Unexpander::getResult() {
  finalize();
  UnwrappedLine Final =
      createUnwrappedLine(*Output.Tokens.front()->Children.front(), Level);
  assert(!Final.Tokens.empty());
  return Final;
}

// Call \p Call for each token in the unwrapped line given, passing
// the token, its parent and whether it is the first token in the line.
void Unexpander::forEachToken(
    const UnwrappedLine &Line,
    const std::function<void(FormatToken *, FormatToken *, bool)> &Call,
    FormatToken *Parent) {
  bool First = true;
  for (const auto &N : Line.Tokens) {
    Call(N.Tok, Parent, First);
    First = false;
    for (const auto &Child : N.Children) {
      forEachToken(Child, Call, N.Tok);
    }
  }
}

// Unexand \p Token, given its parent \p OriginalParent in the incoming
// unwrapped line. \p First specifies whether it is the first token in a given
// unwrapped line.
void Unexpander::add(FormatToken *Token, FormatToken *OriginalParent,
                     bool First) {
  // In order to be able to find the correct parent in the unexpanded token
  // stream, we need to continue the unexpansion until we find the right token
  // if it is part of the unexpanded token stream.
  // Note that hidden tokens can be part of the unexpanded stream in nested
  // macro calls.
  // For example, given: BRACED(a) {a}
  // And the call: BRACED(BRACED(x))
  // The outer macro call will be BRACED({a}), and the hidden tokens '{' and
  // '}' can be found in the unexpanded macro stream of that level.
  if (!Unexpanded.empty() && Token->MacroCtx.Role != MR_None &&
      (Token->MacroCtx.Role != MR_Hidden ||
       Unexpanded.size() != Token->MacroCtx.ExpandedFrom.size())) {
    continueUnexpansionUntil(Token);
  }

  prepareParent(OriginalParent, First);

  // Closing braces/parentheses go into the same line as the corresponding
  // opening brace/parenthesis.
  if ((Token->is(tok::r_brace) || Token->is(tok::r_paren)) &&
      !ExpandedParens.empty()) {
    Lines.pop_back();
    Lines.push_back(ExpandedParens.back());
    ExpandedParens.pop_back();
  }

  if (Token->MacroCtx.Role == MR_None) {
    // If this token was not generated by a macro call, we add it to the current
    // line.
    pushToken(Token);
  } else {
    // Otherwise add the unexpanded equivalent of the token.
    unexpand(Token);
  }

  if (Token->is(tok::l_brace) || Token->is(tok::l_paren)) {
    assert(!Lines.empty());
    ExpandedParens.push_back(Lines.back());
  }
}

// Ensures that:
// Lines.back() is the line that has \p OriginalParent or its unexpanded
// replacement token as a parent - that is, the last token in
// \c Lines[Lines.size()-2] is the parent of Lines.back() in the
// unexpanded unwrapped line.
void Unexpander::prepareParent(FormatToken *OriginalParent, bool First) {
  // We want to find the parent in the new unwrapped line, where the original
  // parent might have been replaced by an unxpansion.
  FormatToken *Parent = getParentInOutput(OriginalParent);
  if (First || Parent == Lines.back()->Tokens.back()->Tok) {
    // If we are at the first token in a new line, we want to also
    // create a new line in the resulting unexpanded unwrapped line.
    while (Parent != Lines.back()->Tokens.back()->Tok) {
      Lines.pop_back();
      assert(!Lines.empty());
    }
    assert(!Lines.empty());
    Lines.back()->Tokens.back()->Children.push_back(std::make_unique<Line>());
    Lines.push_back(&*Lines.back()->Tokens.back()->Children.back());
  } else if (parentLine().Tokens.back()->Tok != Parent) {
    // If we're not the first token in a new line, pop lines until we find
    // the child of \c Parent in the stack.
    while (Parent != parentLine().Tokens.back()->Tok) {
      Lines.pop_back();
      assert(!Lines.empty());
    }
  }
  assert(parentLine().Tokens.back()->Tok == Parent);
  assert(!Lines.empty());
}

// For a given \p Parent in the incoming expanded token stream, find the
// corresponding parent in the output.
FormatToken *Unexpander::getParentInOutput(FormatToken *Parent) {
  auto I = TokenToParentInOutput.find(Parent);
  if (I == TokenToParentInOutput.end())
    return Parent;
  for (; I != TokenToParentInOutput.end();
       I = TokenToParentInOutput.find(Parent)) {
    Parent = I->second;
  }
  // If we use a different token than the parent in the expanded token stream
  // as parent, mark it as a special parent, so the formatting code knows it
  // needs to have its children formatted.
  Parent->MacroCtx.MacroParent = true;
  return Parent;
}

// Unexpand a \p Token that was expanded from a macro call.
void Unexpander::unexpand(FormatToken *Token) {
  assert(Token->MacroCtx.Role != MR_None);
  // A single token can be the only result of a macro call:
  // Given: ID(x, y) ;
  // And the call: ID(<some>, <tokens>)
  // ';' in the expanded stream will unexpand all of ID(<some>, <tokens>).
  if (Token->MacroCtx.StartOfExpansion) {
    startUnexpansion(Token);
    // If the order of tokens in the expanded token stream is not the
    // same as the order of tokens in the unexpanded stream, we need
    // to unexpand tokens that arrive later in the stream.
    if (Token->MacroCtx.Role != MR_Hidden) {
      continueUnexpansionUntil(Token);
    }
  }
  assert(!Unexpanded.empty());
  if (Unexpanded.back().I != Unexpanded.back().End) {
    assert(Unexpanded.size() == Token->MacroCtx.ExpandedFrom.size());
    if (Token->MacroCtx.Role != MR_Hidden) {
      // The current token in the unexpanded token stream must be the token
      // we're looking for - we either arrive here after startUnexpansion,
      // which initiates the stream to the first token, or after
      // continueExpansionUntil skipped until the expected token in the
      // unexpanded stream at the start of add(...).
      assert(Unexpanded.back().I->Tok == Token);
      processNextUnexpanded();
    } else {
      assert(!currentLine()->Tokens.empty());
      // Map all hidden tokens to the last visible token in the output.
      // If the hidden token is a parent, we'll use the last visible
      // token as the parent of the hidden token's children.
      TokenToParentInOutput[Token] = currentLine()->Tokens.back()->Tok;
    }
  }
  if (Token->MacroCtx.EndOfExpansion)
    endUnexpansion(Token);
}

// Given a \p Token that starts an expansion, unexpand the beginning of the
// macro call.
// For example, given: ID(x) x
// And the call: ID(int a)
// Unexpands: ID(
void Unexpander::startUnexpansion(FormatToken *Token) {
  assert(!Token->MacroCtx.ExpandedFrom.empty());
  assert(Unexpanded.size() <= Token->MacroCtx.ExpandedFrom.size());
#ifndef NDEBUG
  // Check that the token's unexpansion stack matches our current unexpansion
  // stack.
  for (size_t I = 0; I < Unexpanded.size(); ++I) {
    assert(Unexpanded[I].ID ==
           Token->MacroCtx
               .ExpandedFrom[Token->MacroCtx.ExpandedFrom.size() - 1 - I]);
  }
#endif
  // Start unexpansion for all calls for which this token is the first token
  // generated by the call.
  for (size_t I = Unexpanded.size(); I < Token->MacroCtx.ExpandedFrom.size();
       ++I) {
    FormatToken *ID =
        Token->MacroCtx
            .ExpandedFrom[Token->MacroCtx.ExpandedFrom.size() - 1 - I];
    // We found a macro call to be unexpanded; the next time our unexpansion
    // stack is empty we know we finished an unexpansion.
    State = InProgress;
    // Put the unexpanded macro call's token into our unexpansion stack.
    auto IU = IdToUnexpanded.find(ID);
    assert(IU != IdToUnexpanded.end());
    Unexpanded.push_back(
        {ID, IU->second->Tokens.begin(), IU->second->Tokens.end()});
    // Process the macro call's identifier.
    processNextUnexpanded();
    if (Unexpanded.back().I == Unexpanded.back().End)
      continue;
    assert(Unexpanded.back().I->Tok->is(tok::l_paren));
    // Process the optional opening parenthesis.
    processNextUnexpanded();
  }
}

// Add all tokens in the unexpansion stream to the output until we find the
// given \p Token.
bool Unexpander::continueUnexpansionUntil(FormatToken *Token) {
  assert(!Unexpanded.empty());
  // FIXME: If Token was already expanded earlier, due to
  // a change in order, we will not find it, but need to
  // skip it.
  for (; Unexpanded.back().I != Unexpanded.back().End &&
         Unexpanded.back().I->Tok != Token;) {
    processNextUnexpanded();
  }
  assert(Unexpanded.back().I == Unexpanded.back().End ||
         Unexpanded.back().I->Tok == Token);
  return true;
}

// End all unexpansions for which \p Token is the final token.
void Unexpander::endUnexpansion(FormatToken *Token) {
  assert(Unexpanded.size() >= Token->MacroCtx.EndOfExpansion);
  for (size_t I = 0; I < Token->MacroCtx.EndOfExpansion; ++I) {
#ifndef NDEBUG
    // Check all remaining tokens but the final closing parenthesis and optional
    // trailing comment were already expanded at an inner expansion level.
    for (auto T = Unexpanded.back().I; T != Unexpanded.back().End; ++T) {
      FormatToken *Token = T->Tok;
      bool ClosingParen = (std::next(T) == Unexpanded.back().End ||
                           std::next(T)->Tok->isTrailingComment()) &&
                          Token->MacroCtx.Role == MR_None &&
                          Token->is(tok::r_paren);
      bool TrailingComment = Token->isTrailingComment();
      bool PreviousLevel =
          Unexpanded.size() < Token->MacroCtx.ExpandedFrom.size();
      assert(ClosingParen || TrailingComment || PreviousLevel);
    }
#endif
    // Expand the closing parenthesis, if it exists, including an optional
    // trailing comment.
    for (auto T = Unexpanded.back().I; T != Unexpanded.back().End; ++T) {
      processNextUnexpanded();
    }
    Unexpanded.pop_back();
  }
}

// If visible, add the next token of the unexpanded token sequence to the
// output.
void Unexpander::processNextUnexpanded() {
  FormatToken *Token = Unexpanded.back().I->Tok;
  ++Unexpanded.back().I;
  // Skip tokens that are not part of the macro call.
  if (Token->MacroCtx.Role == MR_Hidden) {
    return;
  }
  // Skip tokens we already expanded during an inner unexpansion.
  // For example, given: ID(x) {x}
  // And the call: ID(ID(f))
  // We get two unexpansions:
  // ID(f) -> {f}
  // ID({f}) -> {{f}}
  // We unexpand f during the first unexpansion, and skip it during the second
  // unexpansion.
  if (Unexpanded.size() < Token->MacroCtx.ExpandedFrom.size()) {
    return;
  }
  // Put the parentheses and commas of a macro call into the same line;
  // if the arguments produce new unwrapped lines, they will become children
  // of the corresponding opening parenthesis or comma tokens in the
  // unexpanded call.
  if (Token->MacroCtx.Role == MR_None &&
      Token->isOneOf(tok::comma, tok::r_paren) && !MacroCallStructure.empty()) {
    if (Token->is(tok::comma)) {
      TokenToParentInOutput[MacroCallStructure.back()->Tokens.back()->Tok] =
          Token;
    }
    pushToken(Token, MacroCallStructure.back());
    if (Token->is(tok::r_paren)) {
      MacroCallStructure.pop_back();
    }
    return;
  }
  if (Token->MacroCtx.Role == MR_None && Token->is(tok::l_paren)) {
    MacroCallStructure.push_back(currentLine());
  }
  // Note that any tokens that are tagged with MR_None have been passed as
  // arguments to the macro that have not been expanded, for example:
  // Given: ID(X) x
  // When calling: ID(a, b)
  // 'b' will be part of the unexpanded token stream, but tagged MR_None.
  // Given that erroring out in this case would be disruptive, we continue
  // pushing the (unformatted) token.
  // FIXME: This can lead to unfortunate formatting decisions - give the user
  // a hint that their macro definition is broken.
  pushToken(Token);
}

void Unexpander::finalize() {
  if (State == Finalized)
    return;
  assert(State == InProgress && Unexpanded.empty());
  State = Finalized;

  // We created corresponding unwrapped lines for each incoming line as children
  // the the toplevel null token.
  assert(Output.Tokens.size() == 1 && !Output.Tokens.front()->Children.empty());
  // The first line becomes the top level line in the resuling unwrapped line.
  auto *I = Output.Tokens.front()->Children.begin();
  ++I;
  LLVM_DEBUG({
    llvm::dbgs() << "Finalizing unexpanded lines:\n";
    debug(Output, 0);
  });
  for (auto *E = Output.Tokens.front()->Children.end(); I != E; ++I) {
    if ((*I)->Tokens.empty())
      continue;

    // Every subsequent line will become a child of the last token in the
    // previous line, which is the token prior to the first token in the line.
    auto L = TokenToPrevious.find((*I)->Tokens.front()->Tok);
    assert(L != TokenToPrevious.end());
    assert(L->second->Children.empty());
    L->second->Children.push_back(std::move(*I));

    // Mark the previous line's last token as generated by a macro expansion
    // so the formatting algorithm can take that into account.
    L->second->Tok->MacroCtx.MacroParent = true;
  }
  Output.Tokens.front()->Children.resize(1);
}

void Unexpander::pushToken(FormatToken *Token, Line *L) {
  L = L ? L : currentLine();
  L->Tokens.push_back(std::make_unique<LineNode>(Token));
  if (PreviousNode != nullptr) {
    assert(TokenToPrevious.find(Token) == TokenToPrevious.end());
    TokenToPrevious[Token] = PreviousNode;
  }
  PreviousNode = &*L->Tokens.back();
}

UnwrappedLine Unexpander::createUnwrappedLine(const Line &Line, int Level) {
  UnwrappedLine Result;
  Result.Level = Level;
  for (const auto &N : Line.Tokens) {
    Result.Tokens.push_back(N->Tok);
    for (const auto &Child : N->Children) {
      if (Child->Tokens.empty())
        continue;
      Result.Tokens.back().Children.push_back(
          createUnwrappedLine(*Child, Level + 1));
    }
  }
  return Result;
}

void Unexpander::debug(const Line &Line, int Level) {
  for (int i = 0; i < Level; ++i)
    llvm::dbgs() << " ";
  for (const auto &N : Line.Tokens) {
    if (!N)
      continue;
    if (N->Tok)
      llvm::dbgs() << N->Tok->TokenText << " ";
    for (const auto &Child : N->Children) {
      llvm::dbgs() << "\n";
      debug(*Child, Level + 1);
      for (int i = 0; i < Level; ++i)
        llvm::dbgs() << " ";
    }
  }
  llvm::dbgs() << "\n";
}

Unexpander::Line &Unexpander::parentLine() {
  return **std::prev(std::prev(Lines.end()));
}

Unexpander::Line *Unexpander::currentLine() { return Lines.back(); }

} // namespace format
} // namespace clang