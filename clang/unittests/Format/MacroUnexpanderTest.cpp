#include "../../lib/Format/MacroUnexpander.h"
#include "TestLexer.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/TokenKinds.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include <memory>
#include <vector>

namespace clang {
namespace format {
namespace {

using UnexpandedMap = std::map<FormatToken *, std::unique_ptr<UnwrappedLine>>;

class Expansion {
public:
  Expansion(TestLexer &Lex, MacroExpander &Macros) : Lex(Lex), Macros(Macros) {}

  TokenList
  expand(llvm::StringRef Name,
         const SmallVector<llvm::SmallVector<FormatToken *, 8>, 1> &Args) {
    auto *ID = Lex.id(Name);
    auto UnexpandedLine = std::make_unique<UnwrappedLine>();
    UnexpandedLine->Tokens.push_back(ID);
    if (!Args.empty()) {
      UnexpandedLine->Tokens.push_back(Lex.id("("));
      for (auto I = Args.begin(), E = Args.end(); I != E; ++I) {
        if (I != Args.begin())
          UnexpandedLine->Tokens.push_back(Lex.id(","));
        UnexpandedLine->Tokens.insert(UnexpandedLine->Tokens.end(), I->begin(),
                                      I->end());
      }
      UnexpandedLine->Tokens.push_back(Lex.id(")"));
    }
    Unexpanded[ID] = std::move(UnexpandedLine);

    auto Expanded = uneof(Macros.expand(ID, Args));
    Tokens.append(Expanded.begin(), Expanded.end());

    TokenList UnexpandedTokens;
    for (const UnwrappedLineNode &Node : Unexpanded[ID]->Tokens) {
      UnexpandedTokens.push_back(Node.Tok);
    }
    return UnexpandedTokens;
  }

  TokenList expand(llvm::StringRef Name,
                   const std::vector<std::string> &Args = {}) {
    return expand(Name, lexArgs(Args));
  }

  const UnexpandedMap &getUnexpanded() const { return Unexpanded; }

  const TokenList &getTokens() const { return Tokens; }

private:
  llvm::SmallVector<TokenList, 1>
  lexArgs(const std::vector<std::string> &Args) {
    llvm::SmallVector<TokenList, 1> Result;
    for (const auto &Arg : Args) {
      Result.push_back(uneof(Lex.lex(Arg)));
    }
    return Result;
  }
  std::map<FormatToken *, std::unique_ptr<UnwrappedLine>> Unexpanded;
  llvm::SmallVector<FormatToken *, 8> Tokens;
  TestLexer &Lex;
  MacroExpander &Macros;
};

struct Chunk {
  Chunk(llvm::ArrayRef<FormatToken *> Tokens)
      : Tokens(Tokens.begin(), Tokens.end()) {}
  Chunk(llvm::ArrayRef<UnwrappedLine> Children)
      : Children(Children.begin(), Children.end()) {}
  llvm::SmallVector<UnwrappedLineNode, 1> Tokens;
  llvm::SmallVector<UnwrappedLine, 0> Children;
};

bool tokenMatches(const FormatToken *Left, const FormatToken *Right) {
  if (Left->getType() == Right->getType() &&
      Left->TokenText == Right->TokenText)
    return true;
  llvm::dbgs() << Left->TokenText << " != " << Right->TokenText << "\n";
  return false;
}

struct Matcher {
  Matcher(const TokenList &Tokens) : Tokens(Tokens), It(this->Tokens.begin()) {}

  Chunk consume(const TokenList &Tokens) {
    TokenList Result;
    for (const FormatToken *Token : Tokens) {
      assert(tokenMatches(*It, Token));
      Result.push_back(*It);
      ++It;
    }
    return Chunk(Result);
  }

  TokenList Tokens;
  TokenList::iterator It;
};

UnexpandedMap mergeUnexpanded(const UnexpandedMap &M1,
                              const UnexpandedMap &M2) {
  UnexpandedMap Result;
  for (const auto &KV : M1) {
    Result[KV.first] = std::make_unique<UnwrappedLine>(*KV.second);
  }
  for (const auto &KV : M2) {
    Result[KV.first] = std::make_unique<UnwrappedLine>(*KV.second);
  }
  return Result;
}

class MacroUnexpanderTest : public ::testing::Test {
public:
  std::unique_ptr<MacroExpander>
  create(const std::vector<std::string> &MacroDefinitions) {
    return std::make_unique<MacroExpander>(
        MacroDefinitions, Lex.SourceMgr.get(), Lex.Style, Lex.Encoding,
        Lex.Allocator, Lex.IdentTable);
  }

  UnwrappedLine line(llvm::ArrayRef<FormatToken *> Tokens) {
    UnwrappedLine Result;
    for (FormatToken *Tok : Tokens) {
      Result.Tokens.push_back(UnwrappedLineNode(Tok));
    }
    return Result;
  }

  UnwrappedLine line(llvm::StringRef Text) { return line({lex(Text)}); }

  UnwrappedLine line(llvm::ArrayRef<Chunk> Chunks) {
    UnwrappedLine Result;
    for (const Chunk &Chunk : Chunks) {
      Result.Tokens.insert(Result.Tokens.end(), Chunk.Tokens.begin(),
                           Chunk.Tokens.end());
      assert(!Result.Tokens.empty());
      Result.Tokens.back().Children.append(Chunk.Children.begin(),
                                           Chunk.Children.end());
    }
    return Result;
  }

  TokenList lex(llvm::StringRef Text) { return uneof(Lex.lex(Text)); }

  Chunk tokens(llvm::StringRef Text) { return Chunk(lex(Text)); }

  Chunk children(llvm::ArrayRef<UnwrappedLine> Children) {
    return Chunk(Children);
  }

  TestLexer Lex;
};

bool matchesTokens(const UnwrappedLine &L1, const UnwrappedLine &L2) {
  if (L1.Tokens.size() != L2.Tokens.size())
    return false;
  for (auto L1It = L1.Tokens.begin(), L2It = L2.Tokens.begin();
       L1It != L1.Tokens.end(); ++L1It, ++L2It) {
    if (L1It->Tok != L2It->Tok)
      return false;
    if (L1It->Children.size() != L2It->Children.size())
      return false;
    for (auto L1ChildIt = L1It->Children.begin(),
              L2ChildIt = L2It->Children.begin();
         L1ChildIt != L1It->Children.end(); ++L1ChildIt, ++L2ChildIt) {
      if (!matchesTokens(*L1ChildIt, *L2ChildIt))
        return false;
    }
  }
  return true;
}
MATCHER_P(matchesLine, line, "") { return matchesTokens(arg, line); }

TEST_F(MacroUnexpanderTest, Identifier) {
  auto Macros = create({"X x"});
  Expansion Exp(Lex, *Macros);
  TokenList Call = Exp.expand("X");

  Unexpander Unexp(0, Exp.getUnexpanded());
  Unexp.addLine(line(Exp.getTokens()));
  EXPECT_TRUE(Unexp.finished());
  UnwrappedLine Result = Unexp.getResult();
  Matcher U(Call);
  EXPECT_THAT(Unexp.getResult(), matchesLine(line(U.consume(lex("X")))));
}

TEST_F(MacroUnexpanderTest, NestedLineWithinCall) {
  auto Macros = create({"C(a) class C { a; };"});
  Expansion Exp(Lex, *Macros);
  TokenList Call = Exp.expand("C", {"void f()"});

  Unexpander Unexp(0, Exp.getUnexpanded());
  Matcher E(Exp.getTokens());
  Unexp.addLine(line(E.consume(lex("class C {"))));
  Unexp.addLine(line(E.consume(lex("void f();"))));
  Unexp.addLine(line(E.consume(lex("};"))));
  EXPECT_TRUE(Unexp.finished());
  Matcher U(Call);
  EXPECT_THAT(Unexp.getResult(),
              matchesLine(line({U.consume(lex("C(")),
                                children({line(U.consume(lex("void f()")))}),
                                U.consume(lex(")"))})));
}

TEST_F(MacroUnexpanderTest, StatementSequence) {
  auto Macros = create({"SEMI ;"});
  Expansion Exp(Lex, *Macros);
  TokenList Call1 = Exp.expand("SEMI");
  TokenList Call2 = Exp.expand("SEMI");
  TokenList Call3 = Exp.expand("SEMI");

  Unexpander Unexp(0, Exp.getUnexpanded());
  Matcher E(Exp.getTokens());
  Unexp.addLine(line(E.consume(lex(";"))));
  Unexp.addLine(line(E.consume(lex(";"))));
  Unexp.addLine(line(E.consume(lex(";"))));
  EXPECT_TRUE(Unexp.finished());
  Matcher U1(Call1);
  Matcher U2(Call2);
  Matcher U3(Call3);
  EXPECT_THAT(
      Unexp.getResult(),
      matchesLine(line(
          {U1.consume(lex("SEMI")),
           children({line({U2.consume(lex("SEMI")),
                           children({line(U3.consume(lex("SEMI")))})})})})));
}

TEST_F(MacroUnexpanderTest, NestedBlock) {
  auto Macros = create({"ID(x) x"});
  // Test: ID({ ID(a *b); })
  // 1. expand ID(a *b) -> a *b
  Expansion Exp1(Lex, *Macros);
  TokenList Call1 = Exp1.expand("ID", {"a *b"});
  // 2. expand ID({ a *b; })
  TokenList Arg;
  Arg.push_back(Lex.id("{"));
  Arg.append(Exp1.getTokens().begin(), Exp1.getTokens().end());
  Arg.push_back(Lex.id(";"));
  Arg.push_back(Lex.id("}"));
  Expansion Exp2(Lex, *Macros);
  TokenList Call2 = Exp2.expand("ID", {Arg});

  // Consume as-if formatted:
  // {
  //   a *b;
  // }
  UnexpandedMap Unexpanded =
      mergeUnexpanded(Exp1.getUnexpanded(), Exp2.getUnexpanded());
  Unexpander Unexp(0, Unexpanded);
  Matcher E(Exp2.getTokens());
  Unexp.addLine(line(E.consume(lex("{"))));
  Unexp.addLine(line(E.consume(lex("a *b;"))));
  Unexp.addLine(line(E.consume(lex("}"))));
  EXPECT_TRUE(Unexp.finished());

  // Expect lines:
  // ID({
  //   ID(a *b);
  // })
  Matcher U1(Call1);
  Matcher U2(Call2);
  auto Chunk2Start = U2.consume(lex("ID("));
  auto Chunk2LBrace = U2.consume(lex("{"));
  U2.consume(lex("a *b"));
  auto Chunk2Mid = U2.consume(lex(";"));
  auto Chunk2RBrace = U2.consume(lex("}"));
  auto Chunk2End = U2.consume(lex(")"));
  auto Chunk1 = U1.consume(lex("ID(a *b)"));

  auto Expected = line({Chunk2Start,
                        children({
                            line(Chunk2LBrace),
                            line({Chunk1, Chunk2Mid}),
                            line(Chunk2RBrace),
                        }),
                        Chunk2End});
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}

TEST_F(MacroUnexpanderTest, NestedChildBlocks) {
  auto Macros = create({"ID(x) x", "CALL(x) f([] { x })"});
  // Test: ID(CALL(CALL(return a * b;)))
  // 1. expand CALL(return a * b;)
  Expansion Exp1(Lex, *Macros);
  TokenList Call1 = Exp1.expand("CALL", {"return a * b;"});
  // 2. expand CALL(f([] { return a * b; }))
  Expansion Exp2(Lex, *Macros);
  TokenList Call2 = Exp2.expand("CALL", {Exp1.getTokens()});
  // 3. expand ID({ f([] { f([] { return a * b; }) }) })
  TokenList Arg3;
  Arg3.push_back(Lex.id("{"));
  Arg3.append(Exp2.getTokens().begin(), Exp2.getTokens().end());
  Arg3.push_back(Lex.id("}"));
  Expansion Exp3(Lex, *Macros);
  TokenList Call3 = Exp3.expand("ID", {Arg3});

  // Consume as-if formatted in three unwrapped lines:
  // 0: {
  // 1:   f([] {
  //        f([] {
  //          return a * b;
  //        })
  //      })
  // 2: }
  UnexpandedMap Unexpanded = mergeUnexpanded(
      Exp1.getUnexpanded(),
      mergeUnexpanded(Exp2.getUnexpanded(), Exp3.getUnexpanded()));
  Unexpander Unexp(0, Unexpanded);
  Matcher E(Exp3.getTokens());
  Unexp.addLine(line(E.consume(lex("{"))));
  Unexp.addLine(
      line({E.consume(lex("f([] {")),
            children({line({E.consume(lex("f([] {")),
                            children({line(E.consume(lex("return a * b;")))}),
                            E.consume(lex("})"))})}),
            E.consume(lex("})"))}));
  Unexp.addLine(line(E.consume(lex("}"))));
  EXPECT_TRUE(Unexp.finished());

  // Expect lines:
  // ID({
  //   CALL(
  //     CALL(
  //       return a * b;
  //     )
  //   )
  // })
  Matcher U1(Call1);
  Matcher U2(Call2);
  Matcher U3(Call3);
  auto Chunk3Start = U3.consume(lex("ID("));
  auto Chunk3LBrace = U3.consume(lex("{"));
  U3.consume(lex("f([] { f([] { return a * b; }) })"));
  auto Chunk3RBrace = U3.consume(lex("}"));
  auto Chunk3End = U3.consume(lex(")"));
  auto Chunk2Start = U2.consume(lex("CALL("));
  U2.consume(lex("f([] { return a * b; })"));
  auto Chunk2End = U2.consume(lex(")"));
  auto Chunk1Start = U1.consume(lex("CALL("));
  auto Chunk1Mid = U1.consume(lex("return a * b;"));
  auto Chunk1End = U1.consume(lex(")"));

  auto Expected = line({
      Chunk3Start,
      children({
          line(Chunk3LBrace),
          line({
              Chunk2Start,
              children({line({
                  Chunk1Start,
                  children({line({Chunk1Mid})}),
                  Chunk1End,
              })}),
              Chunk2End,
          }),
          line(Chunk3RBrace),
      }),
      Chunk3End,
  });
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}

TEST_F(MacroUnexpanderTest, NestedChildrenMultipleArguments) {
  auto Macros = create({"CALL(a, b) f([] { a; b; })"});
  Expansion Exp(Lex, *Macros);
  TokenList Call = Exp.expand("CALL", {std::string("int a"), "int b"});

  Unexpander Unexp(0, Exp.getUnexpanded());
  Matcher E(Exp.getTokens());
  Unexp.addLine(line({
      E.consume(lex("f([] {")),
      children({
          line(E.consume(lex("int a;"))),
          line(E.consume(lex("int b;"))),
      }),
      E.consume(lex("})")),
  }));
  EXPECT_TRUE(Unexp.finished());
  Matcher U(Call);
  auto Expected = line({
      U.consume(lex("CALL(")),
      children({line(U.consume(lex("int a")))}),
      U.consume(lex(",")),
      children({line(U.consume(lex("int b")))}),
      U.consume(lex(")")),
  });
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}

TEST_F(MacroUnexpanderTest, ReverseOrderArgumentsInExpansion) {
  auto Macros = create({"CALL(a, b) b + a"});
  Expansion Exp(Lex, *Macros);
  TokenList Call = Exp.expand("CALL", {std::string("x"), "y"});

  Unexpander Unexp(0, Exp.getUnexpanded());
  Matcher E(Exp.getTokens());
  Unexp.addLine(line(E.consume(lex("y + x"))));
  EXPECT_TRUE(Unexp.finished());
  Matcher U(Call);
  auto Expected = line(U.consume(lex("CALL(x, y)")));
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}

TEST_F(MacroUnexpanderTest, MultipleToplevelUnwrappedLines) {
  auto Macros = create({"ID(a, b) a b"});
  Expansion Exp(Lex, *Macros);
  TokenList Call = Exp.expand("ID", {std::string("x; x"), "y"});

  Unexpander Unexp(0, Exp.getUnexpanded());
  Matcher E(Exp.getTokens());
  Unexp.addLine(line(E.consume(lex("x;"))));
  Unexp.addLine(line(E.consume(lex("x y"))));
  EXPECT_TRUE(Unexp.finished());
  Matcher U(Call);
  auto Expected = line({
      U.consume(lex("ID(")),
      children({
          line(U.consume(lex("x;"))),
          line(U.consume(lex("x"))),
      }),
      U.consume(lex(", y)")),
  });
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}

TEST_F(MacroUnexpanderTest, NestedCallsMultipleLines) {
  auto Macros = create({"ID(x) x"});
  // Test: ID({ID(a * b);})
  // 1. expand ID(a * b)
  Expansion Exp1(Lex, *Macros);
  TokenList Call1 = Exp1.expand("ID", {"a * b"});
  // 2. expand ID({ a * b; })
  Expansion Exp2(Lex, *Macros);
  TokenList Arg2;
  Arg2.push_back(Lex.id("{"));
  Arg2.append(Exp1.getTokens().begin(), Exp1.getTokens().end());
  Arg2.push_back(Lex.id(";"));
  Arg2.push_back(Lex.id("}"));
  TokenList Call2 = Exp2.expand("ID", {Arg2});

  // Consume as-if formatted in three unwrapped lines:
  // 0: {
  // 1:   a * b;
  // 2: }
  UnexpandedMap Unexpanded = 
      mergeUnexpanded(Exp1.getUnexpanded(), Exp2.getUnexpanded());
  Unexpander Unexp(0, Unexpanded);
  Matcher E(Exp2.getTokens());
  Unexp.addLine(line(E.consume(lex("{"))));
  Unexp.addLine(line(E.consume(lex("a * b;"))));
  Unexp.addLine(line(E.consume(lex("}"))));
  EXPECT_TRUE(Unexp.finished());

  // Expect lines:
  // ID(
  //     {
  //     ID(a * b);
  //     }
  // )
  Matcher U1(Call1);
  Matcher U2(Call2);
  auto Chunk2Start = U2.consume(lex("ID("));
  auto Chunk2LBrace = U2.consume(lex("{"));
  U2.consume(lex("a * b"));
  auto Chunk2Semi = U2.consume(lex(";"));
  auto Chunk2RBrace = U2.consume(lex("}"));
  auto Chunk2End = U2.consume(lex(")"));
  auto Chunk1 = U1.consume(lex("ID(a * b)"));

  auto Expected = line({
      Chunk2Start,
      children({
        line({Chunk2LBrace}),
        line({Chunk1,Chunk2Semi}),
        line({Chunk2RBrace}),
      }),
      Chunk2End,
  });
  EXPECT_THAT(Unexp.getResult(), matchesLine(Expected));
}
} // namespace
} // namespace format
} // namespace clang
