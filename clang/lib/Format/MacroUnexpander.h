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
/// This file contains the declaration of MacroUnexpander, which fits an
/// unexpanded macro call to a parsed set of UnwrappedLines.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_FORMAT_MACRO_UNEXPANDER_H
#define LLVM_CLANG_LIB_FORMAT_MACRO_UNEXPANDER_H

#include "FormatToken.h"
#include "UnwrappedLineParser.h"
#include "clang/Basic/TokenKinds.h"
#include "llvm/Support/Debug.h"
#include <map>

namespace clang {
namespace format {

/// Matches formatted lines that were created by macro expansion to a format
/// for the original macro call.
///
/// Given a mapping from the macro name identifier token in the macro call
/// to the tokens of the macro call, for example:
/// CLASSA -> CLASSA({public: void x();})
///
/// When getting the formatted lines of the expansion via the \c addLine method:
/// -> class A {
/// -> public:
/// ->   void x();
/// -> };
///
/// Creates the unwrapped lines containing the macro call tokens so that
/// the macro call tokens fit the semantic structure of the expanded formatted
/// lines:
/// -> CLASSA({
/// -> public:
/// ->   void x();
/// -> })
class Unexpander {
public:
  /// Create an Unexpander whose resulting unexpanded line will start at
  /// \p Level, unexpanding using the map from name identifier token
  /// to the corresponding tokens of the original macro call.
  Unexpander(unsigned Level,
             const std::map<FormatToken *, std::unique_ptr<UnwrappedLine>>
                 &Unexpanded);

  /// For the given \p Line, match all occurences of tokens expanded from a
  /// macro and replace them with the original macro call in \c getResult().
  void addLine(const UnwrappedLine &Line);

  /// Check whether at the current state there is no open macro expansion
  /// that needs to be processed to finish an macro call.
  bool finished() { return State == InProgress && Unexpanded.empty(); }

  /// Retrieve the formatted \c UnwrappedLine containing the orginal
  /// macro calls, formatted according to the expanded token stream received
  /// via \c addLine().
  /// Generally, this line tries to have the same structure as the expanded,
  /// formatted unwrapped lines handed in via \c addLine():
  /// If a token in a macro argument is a child of a token in the expansion,
  /// the parent will be the corresponding token in the macro call.
  /// For example:
  /// #define C(a, b) class C { a b
  /// C(int x;, int y;) would expand to class C { int x; int y; where in a
  /// formatted line "int x;" and "int y;" would both be new separate lines (at
  /// different indentation levels). In the result, "int x;" will be a child of
  /// the opening parenthesis in "C(" and "int y;" will be a child of the ","
  /// token.
  UnwrappedLine getResult();

private:
  void forEachToken(
      const UnwrappedLine &Line,
      const std::function<void(FormatToken *, FormatToken *, bool)> &Call,
      FormatToken *Parent = nullptr);
  void add(FormatToken *Token, FormatToken *OriginalParent, bool First);
  void prepareParent(FormatToken *OriginalParent, bool First);
  FormatToken *getParentInOutput(FormatToken *Parent, bool First);
  void unexpand(FormatToken *Token);
  void startUnexpansion(FormatToken *Token);
  bool continueUnexpansionUntil(FormatToken *Token);
  void endUnexpansion(FormatToken *Token);
  void processNextUnexpanded();
  void finalize();

  struct Line;

  void pushToken(FormatToken *Token, Line *L = nullptr);
  UnwrappedLine createUnwrappedLine(const Line &Line, int Level);
  void debug(const Line &Line, int Level);
  Line &parentLine();
  Line *currentLine();

  enum UnexpanderState {
    Start,      // No macro expansion was found in the input yet.
    InProgress, // During a macro unexpansion.
    Finalized,  // Past macro unexpansion, the result is finalized.
  };
  UnexpanderState State = Start;

  // Node in which we build up the resulting unwrapped line; this type is
  // analogous to UnwrappedLineNode.
  struct LineNode {
    LineNode() = default;
    LineNode(FormatToken *Tok) : Tok(Tok) {}
    FormatToken *Tok = nullptr;
    llvm::SmallVector<std::unique_ptr<Line>, 4> Children;
  };

  // Line in which we build up the resulting unwrapped line.
  // FIXME: Investigate changing UnwrappedLine to a pointer type and using it
  // instead of rolling our own type.
  struct Line {
    llvm::SmallVector<std::unique_ptr<LineNode>, 4> Tokens;
  };

  // The line in which we collect the resulting unexpanded output.
  // To reduce special cases in the algorithm, the first level of the line
  // contains a single null token that has the unexpanded incoming
  // lines as children.
  // In the end, we stich the lines together so that each subsequent line
  // is a child of the last token of the previous line. This is necessary
  // in order to format the overall expression as a single logical line -
  // if we created separate lines, we'd format them with their own top-level
  // indent depending on the semantic structure, which is not desired.
  Line Output;

  // Stack of currently "open" lines, where each line's predecessor's last
  // token is the parent token for that line.
  llvm::SmallVector<Line *, 4> Lines;

  // Maps from the original token to the token that takes its place in the
  // unexpanded token stream in terms of parent-child relationships.
  // Note that it might take multiple steps to arrive at the correct
  // parent in the output.
  // Given: C(a, b) []() { a; b; }
  // And a call: C(f(), g())
  // The structure in the incoming formatted unwrapped line will be:
  // []() {
  //      |- f();
  //      \- g();
  // }
  // with f and g being children of the opening brace.
  // In the unexpanded call:
  // C(f(), g())
  //  \- f()
  //      \- g()
  // We want f to be a child of the opening parenthesis and g to be a child
  // of the comma token in the macro call.
  // Thus, we map
  // { -> (
  // and add
  // ( -> ,
  // once we're past the comma in the unexpansion.
  llvm::DenseMap<FormatToken *, FormatToken *> TokenToParentInOutput;

  // Keeps track of a single expansion while we're unexpanding tokens it
  // generated.
  struct Expansion {
    // The identifier token of the macro call.
    FormatToken *ID;
    // Our current position in the unexpansion.
    std::list<UnwrappedLineNode>::iterator I;
    // The end of the unexpanded token sequence.
    std::list<UnwrappedLineNode>::iterator End;
  };

  // Stack of unexpanded macro calls for which we're in the middle
  // of an expansion.
  llvm::SmallVector<Expansion, 2> Unexpanded;

  // Keeps track of the lines into which the opening brace/parenthesis for
  // the incoming expanded token stream go.
  // This is needed as the braces/parentheses might be split across multiple
  // unwarpped lines in the expanded stream, but we'll want to put them
  // into the corresponding lines in the unexpanded unwrapped line.
  // For example, given ID(x) x
  // And the call ID(class C { int a; });
  // The expanded incoming lines are:
  // 1: class C {
  // 2:   int a;
  // 3: };
  // In the resulting unwrapped line, we want the closing brace to be part
  // of the same line as the opening brace.
  llvm::SmallVector<Line *, 4> ExpandedParens;

  struct MacroCallState {
    Line *Line;
    FormatToken *Token;
    unsigned Lines;
  };

  // Keeps track of the lines into which the opening brace/parenthesis &
  // argument separating commas for each level in the macro call go in order to
  // put the corresponding closing brace/parenthesis into the same line in the
  // output and keep track of which parents in the expanded token stream map to
  // which tokens in the unexpanded stream.
  // When an opening brace/parenthesis has children, we want the structure of
  // the output line to be:
  // |- MACRO
  // |- (
  // |  \- <argument>
  // |- ,
  // |  \- <argument>
  // \- )
  llvm::SmallVector<MacroCallState, 4> MacroCallStructure;

  // For each token, the previous token in the resulting unexpanded token
  // stream.
  llvm::DenseMap<FormatToken *, LineNode *> TokenToPrevious;

  // The previous token's node we put into the resulting unexpanded token
  // stream.
  LineNode *PreviousNode = nullptr;

  // Level the generated UnwrappedLine will be at.
  const unsigned Level;

  // Maps from identifier of the macro call to an unwrapped line containing
  // all tokens of the macro call.
  const std::map<FormatToken *, std::unique_ptr<UnwrappedLine>> &IdToUnexpanded;
};

} // namespace format
} // namespace clang

#endif
