#include "Util.hpp"

using namespace grc;

ExprAST* HandleExpression(const std::string &Op, grc::ExprAST *Operand) {
  std::unique_ptr<ExprAST> o(Operand);
  return new UnaryExprAST(Op, std::move(o)); 
}

ExprAST* HandleExpression(const std::string &Op, ExprAST *ExprA, ExprAST *ExprB) {
  std::unique_ptr<ExprAST> e(ExprA);
  std::unique_ptr<ExprAST> d(ExprB);
  return new BinaryExprAST(Op, std::move(e), std::move(d)); 
}

ExprAST* HandleExpression(uint8_t Op, ExprAST* ExprA, ExprAST* ExprB) {
  std::unique_ptr<ExprAST> e(ExprA);
  std::unique_ptr<ExprAST> d(ExprB);
  switch(Op) {
    case 1:
      return new grc::BinaryExprAST(">", std::move(e), std::move(d));
      break;
    case 2:
      return new grc::BinaryExprAST("<", std::move(e), std::move(d));
      break;
    case 3:
      return new grc::BinaryExprAST("<>", std::move(e), std::move(d));
      break;
    case 4:
      return new grc::BinaryExprAST(">=", std::move(e), std::move(d));
      break;
    default:
      return new grc::BinaryExprAST("<=", std::move(e), std::move(d));
  }
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then, ExprAST* Else) {
  std::unique_ptr<ExprAST> c(Cond);
  std::unique_ptr<ExprAST> t(Then);
  std::unique_ptr<ExprAST> e(Else);
  return new grc::IfExprAST(std::move(c), std::move(t), std::move(e));
}

ExprAST* HandleCmdIf(ExprAST* Cond, ExprAST* Then) {
  std::unique_ptr<ExprAST> c(Cond);
  std::unique_ptr<ExprAST> t(Then);
  return new IfExprAST(std::move(c), std::move(t), nullptr);
}

BlockAST* HandleCmd() {
  return new BlockAST();
}

void HandleCmd(BlockAST *Block, ExprAST *Expr) {
  std::unique_ptr<ExprAST> e(Expr);
  Block->addExprAST(std::move(e));
}
