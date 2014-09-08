using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Symbols;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.CSharp {
    //TODO: check postfix unary operators; lift built-in operators; lift methods
    internal static class ElevatorHelpers {
        const string _opApplicationName = "op_Application";

        public static bool IsElevatedType(this TypeSymbol type) {
            return type.GetMembers().Where(m => m.Name == _opApplicationName).Any(); //TODO: write real checks
        }

        internal static BoundExpression BindUnaryOperatorEx(this Binder binder, PrefixUnaryExpressionSyntax node, DiagnosticBag diagnostics) {
            BoundExpression operand = binder.BindValue(node.Operand, diagnostics, Binder.GetUnaryAssignmentKind(node.Kind));
            return (binder.BindIntegralMinValConstants(node, operand, diagnostics) ?? binder.BindUnaryOperatorCoreEx(node, node.OperatorToken.Text, operand, diagnostics));
        }

        internal static BoundExpression BindUnaryOperatorCoreEx(this Binder binder, CSharpSyntaxNode syntax, string operatorText, BoundExpression operand, DiagnosticBag diagnostics) {
            DiagnosticBag nonElevatedDiagnostics = DiagnosticBag.GetInstance();
            var boundOperator = binder.BindUnaryOperatorCore(syntax, operatorText, operand, nonElevatedDiagnostics);

            if (boundOperator.ResultKind != LookupResultKind.OverloadResolutionFailure) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            IEnumerable<BoundExpression> originalArguments = new BoundExpression[] { operand };

            //TODO: We need to search ancestors here and in most of the following code
            if (!operand.Type.IsElevatedType()) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            NamedTypeSymbol elevatedTypeSymbol = (NamedTypeSymbol)operand.Type;

            //What if ancestors have additional type arguments?
            if (elevatedTypeSymbol.TypeArguments.Count() != 1) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                //binder.Error(diagnostics, ErrorCode.ERR_AmbigUnaryOp, ...);
                return boundOperator;
            }

            DiagnosticBag elevatedDiagnostics = DiagnosticBag.GetInstance();

            //Should we unwrap multiple times?
            TypeSymbol loweredArgumentType = elevatedTypeSymbol.TypeArguments.Single();

            var callSyntax = SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.ParseTypeName(elevatedTypeSymbol.ToDisplayString()),
                    SyntaxFactory.IdentifierName(_opApplicationName)
                ),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                        new[] {
                            SyntaxFactory.Argument((ExpressionSyntax)operand.Syntax),
                            SyntaxFactory.Argument(
                                SyntaxFactory.ParenthesizedLambdaExpression(
                                    SyntaxFactory.ParameterList(
                                        SyntaxFactory.SingletonSeparatedList(
                                            SyntaxFactory.Parameter(SyntaxFactory.Identifier("$a"))
                                                .WithType(SyntaxFactory.ParseTypeName(loweredArgumentType.ToDisplayString()))
                                        )
                                    ),
                                    SyntaxFactory.PrefixUnaryExpression(
                                        syntax.Kind,
                                        SyntaxFactory.IdentifierName("$a")
                                    )
                                )
                            )
                        }
                    )
                )
            );
            var reintegratedCallSyntax = CSharpSyntaxNode.CloneNodeAsRoot(callSyntax, syntax.SyntaxTree);

            var applicationOperatorCallExpr = binder.BindExpression(
                reintegratedCallSyntax,
                elevatedDiagnostics
            );

            diagnostics.AddRange(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applicationOperatorCallExpr;
        }

        //TODO: Implement the elevation-aware version of the BindConditionalLogicalOperator method
        internal static BoundExpression BindSimpleBinaryOperatorEx(this Binder binder, BinaryExpressionSyntax syntax, DiagnosticBag diagnostics, BoundExpression left, BoundExpression right) {
            DiagnosticBag nonElevatedDiagnostics = DiagnosticBag.GetInstance();
            var boundOperator = binder.BindSimpleBinaryOperator(syntax, nonElevatedDiagnostics, left, right);

            if (boundOperator.ResultKind != LookupResultKind.OverloadResolutionFailure) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            IEnumerable<BoundExpression> originalArguments = new BoundExpression[] { left, right };
            var argumentTypes = originalArguments.Select(arg => arg.Type).ToList();
            var elevatedArgumentsTypes = argumentTypes.Where(type => type.IsElevatedType()).ToList();
            //var elevatedArguments = originalArguments.Where(arg => arg.Type.IsElevatedType()).ToList();

            //TODO: We need to search ancestors here and in most of the following code
            if (!elevatedArgumentsTypes.Any()) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }

            //TODO: Choose the first appropriate, not just the first elevated type
            NamedTypeSymbol elevatedTypeSymbol = (NamedTypeSymbol)elevatedArgumentsTypes.First();

            //What if ancestors have additional type arguments?
            if (elevatedTypeSymbol.TypeArguments.Count() != 1) {
                diagnostics.AddRange(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                //binder.Error(diagnostics, ErrorCode.ERR_AmbigUnaryOp, ...);
                return boundOperator;
            }

            DiagnosticBag elevatedDiagnostics = DiagnosticBag.GetInstance();

            var loweredArgumentTypes = argumentTypes.Select(type => type.IsElevatedType() ? ((NamedTypeSymbol)type).TypeArguments.Single() : type).ToList();

            var callSyntax = SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.ParseTypeName(elevatedTypeSymbol.ToDisplayString()),
                    SyntaxFactory.IdentifierName(_opApplicationName)
                ),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                        new[] {
                            SyntaxFactory.Argument((ExpressionSyntax)left.Syntax),
                            SyntaxFactory.Argument((ExpressionSyntax)right.Syntax),
                            SyntaxFactory.Argument(
                                SyntaxFactory.ParenthesizedLambdaExpression(
                                    SyntaxFactory.ParameterList(
                                        SyntaxFactory.SeparatedList(
                                            new[] {
                                                SyntaxFactory.Parameter(SyntaxFactory.Identifier("$a"))
                                                    .WithType(SyntaxFactory.ParseTypeName(loweredArgumentTypes[0].ToDisplayString())),
                                                SyntaxFactory.Parameter(SyntaxFactory.Identifier("$b"))
                                                    .WithType(SyntaxFactory.ParseTypeName(loweredArgumentTypes[1].ToDisplayString()))
                                            }
                                        )
                                    ),
                                    SyntaxFactory.BinaryExpression(
                                        syntax.Kind,
                                        SyntaxFactory.IdentifierName("$a"),
                                        syntax.OperatorToken,
                                        SyntaxFactory.IdentifierName("$b")
                                    )
                                )
                            )
                        }
                    )
                )
            );
            var reintegratedCallSyntax = CSharpSyntaxNode.CloneNodeAsRoot(callSyntax, syntax.SyntaxTree);

            var applicationOperatorCallExpr = binder.BindExpression(
                reintegratedCallSyntax,
                elevatedDiagnostics
            );

            diagnostics.AddRange(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applicationOperatorCallExpr;
        }
    }
}
