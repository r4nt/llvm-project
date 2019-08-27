#include "../../lib/Format/MacroExpander.h"
#include "TestLexer.h"
#include "clang/Basic/FileManager.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace clang {
namespace format {

namespace {

class MacroExpanderTest : public ::testing::Test {
public:
  std::unique_ptr<MacroExpander>
  create(const std::vector<std::string> &MacroDefinitions) {
    return std::make_unique<MacroExpander>(
        MacroDefinitions, Lex.SourceMgr.get(), Lex.Style, Lex.Encoding,
        Lex.Allocator, Lex.IdentTable);
  }

  std::string expand(MacroExpander &Macros, llvm::StringRef Name,
                     const std::vector<std::string> &Args = {}) {
    EXPECT_TRUE(Macros.defined(Name));
    return text(Macros.expand(Lex.id(Name), lexArgs(Args)));
  }

  llvm::SmallVector<TokenList, 1>
  lexArgs(const std::vector<std::string> &Args) {
    llvm::SmallVector<TokenList, 1> Result;
    for (const auto &Arg : Args) {
      Result.push_back(uneof(Lex.lex(Arg)));
    }
    return Result;
  }

  struct MacroAttributes {
    clang::tok::TokenKind Kind;
    MacroRole Role;
    bool Start;
    size_t End;
    llvm::SmallVector<FormatToken *, 1> ExpandedFrom;
  };

  void expectAttributes(const TokenList &Tokens,
                        const std::vector<MacroAttributes> &Attributes) {
    EXPECT_EQ(Tokens.size(), Attributes.size()) << text(Tokens);
    for (size_t I = 0, E = Tokens.size(); I != E; ++I) {
      if (I >= Attributes.size())
        continue;
      std::string Context =
          ("for token " + llvm::Twine(I) + ": " + Tokens[I]->Tok.getName() +
           " / " + Tokens[I]->TokenText)
              .str();
      EXPECT_TRUE(Tokens[I]->is(Attributes[I].Kind))
          << Context << " in " << text(Tokens);
      EXPECT_EQ(Tokens[I]->MacroCtx.Role, Attributes[I].Role)
          << Context << " in " << text(Tokens);
      EXPECT_EQ(Tokens[I]->MacroCtx.StartOfExpansion, Attributes[I].Start)
          << Context << " in " << text(Tokens);
      EXPECT_EQ(Tokens[I]->MacroCtx.EndOfExpansion, Attributes[I].End)
          << Context << " in " << text(Tokens);
      EXPECT_EQ(Tokens[I]->MacroCtx.ExpandedFrom, Attributes[I].ExpandedFrom)
          << Context << " in " << text(Tokens);
    }
  }

  TestLexer Lex;
};

TEST_F(MacroExpanderTest, SkipsDefinitionOnError) {
  auto Macros =
      create({"A(", "B(,", "C(a,", "D(a a", "E(a, a", "F(,)", "G(a;"});
  for (const auto *Name : {"A", "B", "C", "D", "E", "F", "G"}) {
    EXPECT_FALSE(Macros->defined(Name)) << "for Name " << Name;
  }
}

TEST_F(MacroExpanderTest, ExpandsWithoutArguments) {
  auto Macros = create({
      "A",
      "B b",
      "C c + c",
      "D()",
  });
  EXPECT_EQ("", expand(*Macros, "A"));
  EXPECT_EQ("b", expand(*Macros, "B"));
  EXPECT_EQ("c+c", expand(*Macros, "C"));
  EXPECT_EQ("", expand(*Macros, "D"));
}

TEST_F(MacroExpanderTest, ExpandsWithArguments) {
  auto Macros = create({
      "A(x)",
      "B(x, y) x + y",
  });
  EXPECT_EQ("", expand(*Macros, "A", {"a"}));
  EXPECT_EQ("b1+b2+b3", expand(*Macros, "B", {"b1", "b2 + b3"}));
  EXPECT_EQ("x+", expand(*Macros, "B", {"x"}));
}

TEST_F(MacroExpanderTest, AttributizesTokens) {
  auto Macros = create({
      "A(x, y) { x + y; }",
      "B(x, y) x + 3 + y",
  });
  auto *A = Lex.id("A");
  auto AArgs = lexArgs({"a1 * a2", "a3 * a4"});
  auto Result = Macros->expand(A, AArgs);
  EXPECT_EQ(11U, Result.size()) << text(Result) << " / " << Result;
  EXPECT_EQ("{a1*a2+a3*a4;}", text(Result));
  std::vector<MacroAttributes> Attributes = {
      {tok::l_brace, MR_Hidden, true, 0, {A}},
      {tok::identifier, MR_ExpandedArg, false, 0, {A}},
      {tok::star, MR_ExpandedArg, false, 0, {A}},
      {tok::identifier, MR_ExpandedArg, false, 0, {A}},
      {tok::plus, MR_Hidden, false, 0, {A}},
      {tok::identifier, MR_ExpandedArg, false, 0, {A}},
      {tok::star, MR_ExpandedArg, false, 0, {A}},
      {tok::identifier, MR_ExpandedArg, false, 0, {A}},
      {tok::semi, MR_Hidden, false, 0, {A}},
      {tok::r_brace, MR_Hidden, false, 1, {A}},
      {tok::eof, MR_Hidden, false, 0, {A}},
  };
  expectAttributes(Result, Attributes);

  auto *B = Lex.id("B");
  auto BArgs = lexArgs({"b1", "b2"});
  Result = Macros->expand(B, BArgs);
  EXPECT_EQ(6U, Result.size()) << text(Result) << " / " << Result;
  EXPECT_EQ("b1+3+b2", text(Result));
  Attributes = {
      {tok::identifier, MR_ExpandedArg, true, 0, {B}},
      {tok::plus, MR_Hidden, false, 0, {B}},
      {tok::numeric_constant, MR_Hidden, false, 0, {B}},
      {tok::plus, MR_Hidden, false, 0, {B}},
      {tok::identifier, MR_ExpandedArg, false, 1, {B}},
      {tok::eof, MR_Hidden, false, 0, {B}},
  };
  expectAttributes(Result, Attributes);
}

TEST_F(MacroExpanderTest, RecursiveExpansion) {
  auto Macros = create({
      "A(x) x",
      "B(x) x",
      "C(x) x",
  });

  auto *A = Lex.id("A");
  auto *B = Lex.id("B");
  auto *C = Lex.id("C");

  auto Args = lexArgs({"id"});
  auto CResult = uneof(Macros->expand(C, Args));
  auto BResult = uneof(Macros->expand(B, CResult));
  auto AResult = uneof(Macros->expand(A, BResult));

  std::vector<MacroAttributes> Attributes = {
      {tok::identifier, MR_ExpandedArg, true, 3, {C, B, A}},
  };
  expectAttributes(AResult, Attributes);
}

} // namespace
} // namespace format
} // namespace clang
