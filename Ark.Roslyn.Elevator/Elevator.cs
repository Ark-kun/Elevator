using System.Collections.Generic;
using System.Linq;

namespace Roslyn.Compilers.CSharp {
    //TODO: check postfix unary operators; lift built-in operators; lift methods
    public static class ElevatorHelpers {
        const string _opApplicationName = "op_Application";

        public static bool IsElevatedType(this TypeSymbol type) {
            return type.GetMembers().Where(m => m.Name == _opApplicationName).Any(); //TODO: write real checks
        }

        internal static BoundExpression BindUnaryOperatorCoreEx(this Binder binder, SyntaxNode syntax, string operatorText, BoundExpression operand, DiagnosticBag diagnostics) {
            DiagnosticBag nonElevatedDiagnostics = DiagnosticBag.GetInstance();
            var boundOperator = binder.BindUnaryOperatorCore(syntax, operatorText, operand, nonElevatedDiagnostics);

            if (boundOperator.ResultKind != LookupResultKind.OverloadResolutionFailure) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            IEnumerable<BoundExpression> originalArguments = new BoundExpression[] { operand };

            //TODO: We need to search ancestors here and in most of the following code
            if (!operand.Type.IsElevatedType()) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            NamedTypeSymbol elevatedTypeSymbol = (NamedTypeSymbol)operand.Type;

            //What if ancestors have additional type arguments?
            if (elevatedTypeSymbol.TypeArguments.Count != 1) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                //binder.Error(diagnostics, ErrorCode.ERR_AmbigUnaryOp, ...);
                return boundOperator;
            }

            DiagnosticBag elevatedDiagnostics = DiagnosticBag.GetInstance();

            //Should we unwrap multiple times?
            TypeSymbol loweredArgumentType = elevatedTypeSymbol.TypeArguments.Single();

            var callSyntax = Syntax.InvocationExpression(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.ParseTypeName(elevatedTypeSymbol.ToDisplayString()),
                    Syntax.IdentifierName(_opApplicationName)
                ),
                Syntax.ArgumentList(
                    Syntax.SeparatedList<ArgumentSyntax>(
                        Syntax.Argument((ExpressionSyntax)syntax),
                        Syntax.Token(SyntaxKind.CommaToken),
                        Syntax.Argument(
                            Syntax.ParenthesizedLambdaExpression(
                                Syntax.ParameterList(
                                    Syntax.SeparatedList<ParameterSyntax>(
                                        Syntax.Parameter(Syntax.Identifier("$a"))
                                            .WithType(Syntax.ParseTypeName(loweredArgumentType.ToDisplayString()))
                                    )
                                ),
                                Syntax.PrefixUnaryExpression(
                                    syntax.Kind,
                                    Syntax.IdentifierName("$a")
                                )
                            )
                        )
                    )
                )
            );
            ////var clonedCallSyntax = syntax.SyntaxTree.GetRoot().ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            //var clonedCallSyntax = ((SyntaxNode)syntax).ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            //var clonedCallSyntax = ((SyntaxNode)syntax.Parent).ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            ////var clonedCallSyntax = callSyntax.Green.ToRed();
            var clonedCallSyntax = SyntaxNode.CloneNodeAsRoot(callSyntax, syntax.SyntaxTree);

            var applicationOperatorCallExpr = binder.BindExpression(
                clonedCallSyntax,
                elevatedDiagnostics
            );

            diagnostics.Add(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applicationOperatorCallExpr;
        }

        internal static BoundExpression BindBinaryOperatorCoreEx(this Binder binder, BinaryExpressionSyntax syntax, DiagnosticBag diagnostics) {
            DiagnosticBag nonElevatedDiagnostics = DiagnosticBag.GetInstance();
            var boundOperator = binder.BindBinaryOperatorCore(syntax, nonElevatedDiagnostics);

            if (boundOperator.ResultKind != LookupResultKind.OverloadResolutionFailure) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            BoundExpression left = binder.BindValue(syntax.Left, diagnostics, Binder.GetBinaryAssignmentKind(syntax.Kind));
            BoundExpression right = binder.BindValue(syntax.Right, diagnostics, Binder.BindValueKind.RValue);

            IEnumerable<BoundExpression> originalArguments = new BoundExpression[] { left, right };
            var argumentTypes = originalArguments.Select(arg => arg.Type).ToList();
            var elevatedArgumentsTypes = argumentTypes.Where(type => type.IsElevatedType()).ToList();
            //var elevatedArguments = originalArguments.Where(arg => arg.Type.IsElevatedType()).ToList();

            //TODO: We need to search ancestors here and in most of the following code
            if (!elevatedArgumentsTypes.Any()) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            //TODO: Choose the first appropriate, not just the first elevated type
            NamedTypeSymbol elevatedTypeSymbol = (NamedTypeSymbol)elevatedArgumentsTypes.First();

            //What if ancestors have additional type arguments?
            if (elevatedTypeSymbol.TypeArguments.Count != 1) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                //binder.Error(diagnostics, ErrorCode.ERR_AmbigUnaryOp, ...);
                return boundOperator;
            }

            DiagnosticBag elevatedDiagnostics = DiagnosticBag.GetInstance();

            var loweredArgumentTypes = argumentTypes.Select(type => type.IsElevatedType() ? ((NamedTypeSymbol)type).TypeArguments.Single() : type).ToList();

            var callSyntax = Syntax.InvocationExpression(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.ParseTypeName(elevatedTypeSymbol.ToDisplayString()),
                    Syntax.IdentifierName(_opApplicationName)
                ),
                Syntax.ArgumentList(
                    Syntax.SeparatedList<ArgumentSyntax>(
                        Syntax.Argument((ExpressionSyntax)left.Syntax),
                        Syntax.Token(SyntaxKind.CommaToken),
                        Syntax.Argument((ExpressionSyntax)right.Syntax),
                        Syntax.Token(SyntaxKind.CommaToken),
                        Syntax.Argument(
                            Syntax.ParenthesizedLambdaExpression(
                                Syntax.ParameterList(
                                    Syntax.SeparatedList<ParameterSyntax>(
                                        Syntax.Parameter(Syntax.Identifier("$a"))
                                            .WithType(Syntax.ParseTypeName(loweredArgumentTypes[0].ToDisplayString())),
                                        Syntax.Token(SyntaxKind.CommaToken),
                                        Syntax.Parameter(Syntax.Identifier("$b"))
                                            .WithType(Syntax.ParseTypeName(loweredArgumentTypes[1].ToDisplayString()))
                                    )
                                ),
                                Syntax.BinaryExpression(
                                    syntax.Kind,
                                    Syntax.IdentifierName("$a"),
                                    syntax.OperatorToken,
                                    Syntax.IdentifierName("$b")
                                )
                            )
                        )
                    )
                )
            );
            ////var res = syntax.SyntaxTree.GetRoot().ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            //var res = ((SyntaxNode)syntax).ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            //var res2 = ((SyntaxNode)syntax.Parent).ReplaceNode((SyntaxNode)syntax, (SyntaxNode)callSyntax);
            ////var redSyntax = callSyntax.Green.ToRed
            var clonedCallSyntax = SyntaxNode.CloneNodeAsRoot(callSyntax, syntax.SyntaxTree);

            var applicationOperatorCallExpr = binder.BindExpression(
                clonedCallSyntax,
                elevatedDiagnostics
            );

            diagnostics.Add(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applicationOperatorCallExpr;
        }
    }
}
