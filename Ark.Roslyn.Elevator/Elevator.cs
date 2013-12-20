﻿using System.Collections.Generic;
using System.Linq;

namespace Roslyn.Compilers.CSharp {
    //TODO: check postfix unary operators; lift built-in operators; lift methods
    public static class ElevatorHelpers {
        const string _opApplicationName = "op_Application";

        public static bool IsElevatedType(this TypeSymbol type) {
            return type.GetMembers().Where(m => m.Name == _opApplicationName).Any(); //TODO: write real checks
        }

        private static BoundExpression CreateDummyTypedExpression(TypeSymbol type) {
            return new BoundParameter( //replace with something easier
                default(SyntaxNode),
                new SourceSimpleParameterSymbol(
                    default(Symbol),
                    type,
                    0,
                    RefKind.None,
                    "$_dummy_name_1",
                    ReadOnlyArray<Location>.Empty
                ),
                type
            );
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
            TypeSymbol loweredType = elevatedTypeSymbol.TypeArguments.Single();

            //TODO:!!!Handle built-in operators
            //TODO: handle conversions
            //BoundUnaryOperator loweredOperatorNode = (BoundUnaryOperator)binder.BindUnaryOperatorCore(
            //    syntax, //change
            //    operatorText,
            //    CreateDummyTypedExpression(loweredType),
            //    elevatedDiagnostics
            //);
            BoundUnaryOperator loweredOperatorNode = (BoundUnaryOperator)binder.BindUnaryOperator(
                Syntax.PrefixUnaryExpression(
                    syntax.Kind,
                    Syntax.DefaultExpression(Syntax.ParseTypeName(loweredType.ToDisplayString()))
                ),
                elevatedDiagnostics
            );

            var applicationOperatorCallExpr = binder.BindElevatedOperation(
                syntax,
                elevatedTypeSymbol,
                originalArguments,
                loweredOperatorNode.MethodOpt,
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

            //Should we unwrap multiple times?
            TypeSymbol loweredType = elevatedTypeSymbol.TypeArguments.Single();


            var loweredArgumentTypes = argumentTypes.Select(type => type.IsElevatedType() ? ((NamedTypeSymbol)type).TypeArguments.Single() : type).ToList();

            //Syntax.Parameter(new SyntaxList<AttributeListSyntax>(), new SyntaxTokenList(), Syntax.ParseTypeName(left.Type.ToDisplayString()), Syntax.Identifier("dummy"), null),
            var loweredOperatorSyntax = Syntax.BinaryExpression(
                syntax.Kind,
                Syntax.DefaultExpression(Syntax.ParseTypeName(loweredArgumentTypes[0].ToDisplayString())),
                Syntax.DefaultExpression(Syntax.ParseTypeName(loweredArgumentTypes[1].ToDisplayString()))
            );
            //TODO:!!!Handle built-in operators
            //TODO: handle conversions
            BoundExpression loweredOperatorExpr = binder.BindBinaryOperatorCore(
                loweredOperatorSyntax, //change
                elevatedDiagnostics
            );

            BoundBinaryOperator loweredOperatorNode = (BoundBinaryOperator)loweredOperatorExpr;

            var applicationOperatorCallExpr = binder.BindElevatedOperation(
                syntax,
                elevatedTypeSymbol,
                originalArguments,
                loweredOperatorNode.MethodOpt,
                elevatedDiagnostics
            );

            diagnostics.Add(elevatedDiagnostics);
            elevatedDiagnostics.Free();
            return applicationOperatorCallExpr;
        }

        private static BoundCall BindElevatedOperation(this Binder binder, SyntaxNode originalOperationSyntax, NamedTypeSymbol primaryElevatedTypeSymbol, IEnumerable<BoundExpression> originalArguments, MethodSymbol loweredOperationSymbol, DiagnosticBag diagnostics) {
            var loweredOperationTargetTypeExpr = new BoundTypeExpression(
                default(SyntaxNode),
                (TypeSymbol)loweredOperationSymbol.ContainingSymbol
            );
            var elevatedTypeExpr = new BoundTypeExpression(
                default(SyntaxNode),
                primaryElevatedTypeSymbol
            );

            var loweredOperationLookupResult = LookupResult.GetInstance();
            loweredOperationLookupResult.SetFrom(LookupResult.Good(loweredOperationSymbol));
            var loweredOperationMethodExpr = new BoundMethodGroup(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.IdentifierName("$_dummy_target_2"),
                    Syntax.IdentifierName("$_dummy_member_2")
                ),
                ReadOnlyArray<TypeSymbol>.Empty,
                loweredOperationTargetTypeExpr,
                loweredOperationSymbol.Name,
                ReadOnlyArray.Singleton(loweredOperationSymbol),
                loweredOperationLookupResult,
                false
            );
            loweredOperationMethodExpr.WasCompilerGenerated = true;
            loweredOperationLookupResult.Free();


            var applicationOperatorCallExpr = binder.BindElevatedOperation(
                originalOperationSyntax,
                elevatedTypeExpr,
                originalArguments,
                loweredOperationMethodExpr,
                diagnostics
            );
            return applicationOperatorCallExpr;
        }

        private static BoundCall BindElevatedOperation(this Binder binder, SyntaxNode originalOperationSyntax, BoundTypeExpression primaryElevatedType, IEnumerable<BoundExpression> originalArguments, BoundMethodGroup loweredOperation, DiagnosticBag diagnostics) {
            var applicationOperatorExpr = binder.BindMemberAccessWithBoundLeft(
                Syntax.MemberAccessExpression(
                    SyntaxKind.MemberAccessExpression,
                    Syntax.IdentifierName("$_dummy_target_3"),
                    Syntax.IdentifierName("$_dummy_member_3")
                ),
                primaryElevatedType,
                Syntax.IdentifierName(_opApplicationName),
                Syntax.Token(SyntaxKind.DotToken),
                true,
                diagnostics
            );

            //<BindInvocationExpression - unpacked>
            AnalyzedArguments arguments = AnalyzedArguments.GetInstance();
            arguments.Arguments.AddRange(originalArguments);
            arguments.Arguments.Add(loweredOperation);

            var applicationOperatorCallExpr = binder.BindInvocationExpression(
                originalOperationSyntax,
                Syntax.EmptyStatement(),
                _opApplicationName,
                applicationOperatorExpr,
                arguments,
                diagnostics,
                null
            );
            applicationOperatorCallExpr.WasCompilerGenerated = true;
            arguments.Free();
            //</BindInvocationExpression - unpacked>

            return applicationOperatorCallExpr;
        }
    }



}
