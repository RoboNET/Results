using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace RoboNet.Results.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class ResultAnalyzer : DiagnosticAnalyzer
{
    // The category of the diagnostic (Design, Naming etc.).
    private const string Category = "Usage";

    private const string BaseNamespace = "RoboNet.Results";
    private const string TypeName = BaseNamespace + ".Result";
    private const string GenericTypeName = BaseNamespace + ".Result`1";


    private static readonly DiagnosticDescriptor Rule = new("RBRS0001",
        new LocalizableResourceString(
            nameof(Resources.RBRS0001Title), Resources.ResourceManager, typeof(Resources)),
        new LocalizableResourceString(
            nameof(Resources.RBRS0001MessageFormat), Resources.ResourceManager, typeof(Resources)),
        Category,
        DiagnosticSeverity.Warning, isEnabledByDefault: true);

    private static readonly DiagnosticDescriptor OkRule = new("RBRS0002",
        new LocalizableResourceString(
            nameof(Resources.RBRS0002Title), Resources.ResourceManager, typeof(Resources)),
        new LocalizableResourceString(
            nameof(Resources.RBRS0002MessageFormat), Resources.ResourceManager, typeof(Resources)),
        Category,
        DiagnosticSeverity.Warning, isEnabledByDefault: true);


    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule, OkRule);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();

        context.RegisterCompilationStartAction(ctx =>
        {
            if (ShouldAnalyze(ctx.Compilation))
            {
                AnalyzeCompilation(ctx);
            }
        });

        context.RegisterSyntaxNodeAction(ctx =>
        {
            if (ShouldAnalyze(ctx.Compilation))
            {
                AnalyseSymbolNode(ctx);
            }
        }, SyntaxKind.VariableDeclaration);
    }

    private void AnalyseSymbolNode(SyntaxNodeAnalysisContext ctx)
    {
        var result = ctx.Compilation.GetTypeByMetadataName(TypeName);
        var resultWithData = ctx.Compilation.GetTypeByMetadataName(GenericTypeName);

        if (ctx.Node is VariableDeclarationSyntax node)
        {
            var symbol = ctx.SemanticModel.GetSymbolInfo(node.Type);

            var type = symbol.Symbol?.OriginalDefinition;
            var resultVariables = type != null &&
                                  (type.Equals(result, SymbolEqualityComparer.Default) ||
                                   type.Equals(resultWithData, SymbolEqualityComparer.Default));

            if (!resultVariables)
            {
                return;
            }

            var block = node.Ancestors().OfType<BlockSyntax>().FirstOrDefault();

            Dictionary<VariableDeclaratorSyntax, int> variablesUsage = new();
            Dictionary<VariableDeclaratorSyntax, int> resultCheckingUsage = new();

            foreach (var variable in node.Variables)
            {
                variablesUsage[variable] = 0;
                resultCheckingUsage[variable] = 0;

                // SomeMethod(result)
                foreach (var memberAccessExpression in block.DescendantNodes().OfType<InvocationExpressionSyntax>())
                {
                    foreach (var argumentListArgument in memberAccessExpression.ArgumentList.Arguments)
                    {
                        if (argumentListArgument.Expression is IdentifierNameSyntax identifierNameSyntax)
                        {
                            if (identifierNameSyntax.Identifier.ValueText == variable.Identifier.ValueText)
                            {
                                variablesUsage[variable] += 1;
                                resultCheckingUsage[variable] += 1;
                            }
                        }
                    }
                }

                // result is {Ok: true}
                foreach (var memberAccessExpression in block.DescendantNodes().OfType<IsPatternExpressionSyntax>())
                {
                    if (memberAccessExpression.Expression is IdentifierNameSyntax identifierNameSyntax)
                    {
                        if (identifierNameSyntax.Identifier.ValueText == variable.Identifier.ValueText)
                        {
                            variablesUsage[variable] += 1;
                            resultCheckingUsage[variable] += 1;
                        }
                    }
                }

                //return result
                var returnSyntaxes = block.DescendantNodes().OfType<ReturnStatementSyntax>();
                foreach (var returnSyntax in returnSyntaxes)
                {
                    if (returnSyntax.Expression is IdentifierNameSyntax identifierNameSyntax)
                    {
                        if (identifierNameSyntax.Identifier.ValueText == variable.Identifier.ValueText)
                        {
                            variablesUsage[variable] += 1;
                        }
                    }
                }

                // result.Ok, result.Unwrap() ....
                foreach (var memberAccessExpression in block.DescendantNodes().OfType<MemberAccessExpressionSyntax>())
                {
                    if (memberAccessExpression.Expression is IdentifierNameSyntax identifierNameSyntax)
                    {
                        if (identifierNameSyntax.Identifier.ValueText == variable.Identifier.ValueText)
                        {
                            if (memberAccessExpression.Name.Identifier.ValueText == "Ok")
                            {
                                resultCheckingUsage[variable] += 1;
                            }
                            else if (memberAccessExpression.Name.Identifier.ValueText == "Data" ||
                                     memberAccessExpression.Name.Identifier.ValueText == "Unwrap")
                            {
                                if (resultCheckingUsage[variable] == 0)
                                {
                                    ctx.ReportDiagnostic(
                                        Diagnostic.Create(
                                            OkRule,
                                            memberAccessExpression.GetLocation()));
                                }
                            }

                            variablesUsage[variable] += 1;
                        }
                    }
                }
            }

            foreach (var i in variablesUsage)
            {
                if (i.Value == 0)
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            Rule,
                            i.Key.GetLocation()));
                }
            }
        }
    }

    private void AnalyzeCompilation(CompilationStartAnalysisContext ctx)
    {
        var result = ctx.Compilation.GetTypeByMetadataName(TypeName);
        var resultWithData = ctx.Compilation.GetTypeByMetadataName(GenericTypeName);

        ctx.RegisterOperationAction(ctx =>
        {
            var type = ctx.Operation.Type;
            if (type == null || !IsResult(type))
            {
                return;
            }

            var parent = ctx.Operation.Parent;

            if (parent is IExpressionStatementOperation)
            {
                ctx.ReportDiagnostic(
                    Diagnostic.Create(
                        Rule,
                        ctx.Operation.Syntax.GetLocation()));
            }
        }, OperationKind.Invocation, OperationKind.Await);

        bool IsResult(ITypeSymbol type)
        {
            if (SymbolEqualityComparer.Default.Equals(type, result))
            {
                return true;
            }

            if (SymbolEqualityComparer.Default.Equals(type.OriginalDefinition, resultWithData))
            {
                return true;
            }

            return false;
        }
    }

    private bool ShouldAnalyze(Compilation ctx)
    {
        return ctx.ReferencedAssemblyNames.Any(a => a.Name == BaseNamespace);
    }
}