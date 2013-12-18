using Roslyn.Compilers;
using Roslyn.Compilers.Common;
using Roslyn.Compilers.CSharp;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Roslyn.Compilers.CSharp {
    public static class ElevatorHelpers {
        public static bool IsElevatedType(this TypeSymbol type) {
            return type.GetMembers().Where(m => m.Name == "op_Apply").Any(); //TODO: write real checks
        }

        internal static BoundExpression BindUnaryOperatorCoreEx(this Binder binder, SyntaxNode syntax, string operatorText, BoundExpression operand, DiagnosticBag diagnostics) {
            DiagnosticBag nonElevatedDiagnostics = DiagnosticBag.GetInstance();
            var boundOperator = binder.BindUnaryOperatorCore(syntax, operatorText, operand, nonElevatedDiagnostics);
           
            if (boundOperator.ResultKind != LookupResultKind.OverloadResolutionFailure) {
                diagnostics.Add(nonElevatedDiagnostics);
                nonElevatedDiagnostics.Free();
                return boundOperator;
            }
            //TODO: search ancestors here and after
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
            TypeSymbol loweredType = elevatedTypeSymbol.TypeArguments.Single();

            //TODO:!!!Handle built-in operators

            BoundParameter loweredOperatorFishingNode = new BoundParameter(
                default(SyntaxNode),
                new SourceSimpleParameterSymbol(
                    default(Symbol),
                    loweredType,
                    0,
                    RefKind.None,
                    "$_dummy_name_1",
                    ReadOnlyArray<Location>.Empty
                ),
                loweredType
            );
            BoundUnaryOperator loweredOperatorNode = (BoundUnaryOperator)binder.BindUnaryOperatorCore(
                syntax, //change
                operatorText,
                loweredOperatorFishingNode,
                elevatedDiagnostics
            );
            MethodSymbol loweredOperatorSymbol = (MethodSymbol)loweredOperatorNode.ExpressionSymbol;
            //[Roslyn.Compilers.CSharp.SourceUserDefinedOperatorSymbol] = "Method MyClass MyClass.op_UnaryNegation(MyClass a)"

            var loweredOperatorTypeExpr = new BoundTypeExpression(
                default(SyntaxNode),
                (TypeSymbol)loweredOperatorSymbol.ContainingSymbol
            );
            var elevatedOperatorTypeExpr = new BoundTypeExpression(
                default(SyntaxNode),
                elevatedTypeSymbol
            );

            var lr1 = LookupResult.GetInstance();
            lr1.SetFrom(LookupResult.Good(loweredOperatorSymbol));
            var loweredOperatorMethodExpr = new BoundMethodGroup(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.IdentifierName("$_dummy_target_2"),
                    Syntax.IdentifierName("$_dummy_member_2")
                ),
                ReadOnlyArray<TypeSymbol>.Empty,
                loweredOperatorTypeExpr,
                loweredOperatorSymbol.Name,
                ReadOnlyArray.Singleton(loweredOperatorSymbol),
                lr1,
                false
            );
            loweredOperatorMethodExpr.WasCompilerGenerated = true;
            lr1.Free();

            var applyOperatorAccessExpr = binder.BindMemberAccessWithBoundLeft(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.IdentifierName("$_dummy_target_3"),
                    Syntax.IdentifierName("$_dummy_member_3")
                ),
                elevatedOperatorTypeExpr,
                Syntax.IdentifierName("op_Apply"),
                Syntax.Token(SyntaxKind.DotToken),
                true,
                elevatedDiagnostics
            );

            //<BindInvocationExpression - unpacked>
            AnalyzedArguments arguments = AnalyzedArguments.GetInstance();
            arguments.Arguments.Add(operand);
            arguments.Arguments.Add(loweredOperatorMethodExpr);

            var applyOperatorCallExpr = binder.BindInvocationExpression(
                syntax,
                Syntax.EmptyStatement(),
                "op_Apply",
                applyOperatorAccessExpr,
                arguments,
                elevatedDiagnostics,
                null
            );
            applyOperatorCallExpr.WasCompilerGenerated = true;
            arguments.Free();
            //</BindInvocationExpression - unpacked>

            diagnostics.Add(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applyOperatorCallExpr;
        }
    }
}
