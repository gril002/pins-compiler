/**
 * @ Author: turk
 * @ Description: Preverjanje in razreševanje imen.
 */

package compiler.seman.name;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.name.env.SymbolTable;
import compiler.seman.name.env.SymbolTable.DefinitionAlreadyExistsException;

import java.util.Optional;

public class NameChecker implements Visitor {
    /**
     * Opis vozlišč, ki jih povežemo z njihovimi
     * definicijami.
     */
    private NodeDescription<Def> definitions;

    /**
     * Simbolna tabela.
     */
    private SymbolTable symbolTable;

    /**
     * Ustvari nov razreševalnik imen.
     */
    public NameChecker(
        NodeDescription<Def> definitions,
        SymbolTable symbolTable
    ) {
        requireNonNull(definitions, symbolTable);
        this.definitions = definitions;
        this.symbolTable = symbolTable;
    }

    @Override
    public void visit(Call call) {
        if (symbolTable.definitionFor(call.name).isEmpty())
            Report.error(call.position, String.format("Function %s is not defined in the current scope", call.name));
        Def def = symbolTable.definitionFor(call.name).get();
        if (def instanceof VarDef)
            Report.error(def.position, String.format("Cannot call variable %s as function", def.name));
        definitions.store(symbolTable.definitionFor(call.name).get(), call);
        call.arguments.forEach(arg -> arg.accept(this));
    }

    @Override
    public void visit(Binary binary) {
        binary.left.accept(this);
        binary.right.accept(this);
        Optional<Def> def = definitions.valueFor(binary.left);
        if (binary.operator == Binary.Operator.ARR && binary.left instanceof Name && def.isPresent() && def.get() instanceof FunDef) {
            Report.error(binary.left.position, "Cannot use function as array");
        }
    }

    @Override
    public void visit(Block block) {
        block.expressions.forEach(expr -> expr.accept(this));
    }

    @Override
    public void visit(For forLoop) {
        forLoop.counter.accept(this);
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.step.accept(this);
        forLoop.body.accept(this);
    }

    @Override
    public void visit(Name name) {
        if (symbolTable.definitionFor(name.name).isEmpty())
            Report.error(name.position, String.format("Name %s is not defined in the current scope", name.name));
        definitions.store(symbolTable.definitionFor(name.name).get(), name);
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        ifThenElse.condition.accept(this);
        ifThenElse.thenExpression.accept(this);
        ifThenElse.elseExpression.ifPresent(expr -> expr.accept(this));
    }

    @Override
    public void visit(Literal literal) {
    }

    @Override
    public void visit(Unary unary) {
        unary.expr.accept(this);
    }

    @Override
    public void visit(While whileLoop) {
        whileLoop.condition.accept(this);
        whileLoop.body.accept(this);
    }

    @Override
    public void visit(Where where) {
        symbolTable.pushScope();
        where.defs.accept(this);
        where.expr.accept(this);
        symbolTable.popScope();
    }

    @Override
    public void visit(Defs defs) {
        defs.definitions.forEach(def -> {
            try {
                symbolTable.insert(def);
            } catch (DefinitionAlreadyExistsException e) {
                Report.error(def.position, String.format("Definition of %s already exists in the current scope", def.name));
            }
        });

        defs.definitions.forEach(def -> def.accept(this));
    }

    @Override
    public void visit(FunDef funDef) {
        if (symbolTable.definitionFor(funDef.name).isEmpty())
            Report.error(funDef.position, String.format("function %s is not defined in the current scope", funDef.name));

        funDef.type.accept(this);
        funDef.parameters.forEach(parameter -> {
            parameter.type.accept(this);
            Optional<Def> def = definitions.valueFor(parameter.type);
            if(def.isPresent() && def.get() instanceof VarDef)
                Report.error(parameter.position, "Cannot variable definition as type");
        });
        symbolTable.pushScope();

        funDef.parameters.forEach(parameter -> parameter.accept(this));
        funDef.body.accept(this);
        symbolTable.popScope();
    }

    @Override
    public void visit(TypeDef typeDef) {
        if (symbolTable.definitionFor(typeDef.name).isEmpty())
            Report.error(typeDef.position, String.format("%s is not defined in the current scope", typeDef.name));
        typeDef.type.accept(this);
    }

    @Override
    public void visit(VarDef varDef) {
        if (symbolTable.definitionFor(varDef.name).isEmpty())
            Report.error(varDef.position, String.format("Variable %s is not defined in the current scope", varDef.name));
        varDef.type.accept(this);
        if (definitions.valueFor(varDef.type).isPresent() && definitions.valueFor(varDef.type).get() instanceof VarDef)
            Report.error(varDef.position, String.format("Invalid type for variable definition %s", varDef.name));
    }

    @Override
    public void visit(Parameter parameter) {
        try {
            symbolTable.insert(parameter);
        } catch (DefinitionAlreadyExistsException e) {
            Report.error(parameter.position, String.format("Definition of %s already exists in the current scope", parameter.name));
        }
    }

    @Override
    public void visit(Array array) {
        array.type.accept(this);
    }

    @Override
    public void visit(Atom atom) {
    }

    @Override
    public void visit(TypeName name) {
        if (symbolTable.definitionFor(name.identifier).isEmpty())
            Report.error(name.position, String.format("Type name %s is not defined in the current scope", name.identifier));
        definitions.store(symbolTable.definitionFor(name.identifier).get(), name);
    }
}
