/**
 * @ Author: turk
 * @ Description: Preverjanje tipov.
 */

package compiler.seman.type;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

public class TypeChecker implements Visitor {
    /**
     * Opis vozlišč in njihovih definicij.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč, ki jim priredimo podatkovne tipe.
     */
    private NodeDescription<Type> types;

    public TypeChecker(NodeDescription<Def> definitions, NodeDescription<Type> types) {
        requireNonNull(definitions, types);
        this.definitions = definitions;
        this.types = types;
    }

    @Override
    public void visit(Call call) {
        List<Type> paramList = new ArrayList<>();
        List<Type> argList = new ArrayList<>();
        Optional<Def> optDef = definitions.valueFor(call);
        optDef.ifPresentOrElse(def -> {
            if (!(def instanceof FunDef))
                Report.error(call.position, String.format("%s is not a function", def.name));
            if (types.valueFor(def).isEmpty())
                def.accept(this);
            types.store(types.valueFor(def).get().asFunction().get().returnType, call);
            ((FunDef)def).parameters.forEach(parameter -> {
                Optional<Type> optType = types.valueFor(parameter);
                optType.ifPresentOrElse(type -> paramList.add(type),
                        () -> Report.error(parameter.position, String.format("Type of parameter %s is not defined", parameter.name)));
            });
        }, () -> Report.error(call.position, String.format("No definition found for called function %s", call.name)));
        call.arguments.forEach(expr -> {
            expr.accept(this);
            Optional<Type> optType = types.valueFor(expr);
            optType.ifPresentOrElse(type -> {
                //types.store(type, expr);
                argList.add(type);
            }, () -> Report.error(expr.position, "Type of parameter %s is not defined"));
        });
        if (paramList.size() != argList.size())
            Report.error(call.position, String.format("Function %s takes %d parameters but %d arguments were given", call.name, paramList.size(), argList.size()));
        for (int i = 0; i < paramList.size(); i++) {
            if (!Objects.equals(paramList.get(i).toString(), argList.get(i).toString()))
                Report.error(call.position, String.format("%d. parameter should be of type %s but an argument of type %s was provided", i, paramList.get(i), argList.get(i)));
        }
    }

    @Override
    public void visit(Binary binary) {
        binary.left.accept(this);
        binary.right.accept(this);
        Optional<Type> optLeft = types.valueFor(binary.left);
        if (optLeft.isEmpty())
            Report.error(binary.left.position, "Left binary parameter is not defined");
        Optional<Type> optRight = types.valueFor(binary.right);
        if (optRight.isEmpty())
            Report.error(binary.right.position, "Right binary parameter is not defined");
        if (optLeft.isPresent() && optRight.isPresent()) {
            Type left = optLeft.get();
            Type right = optRight.get();
            if (binary.operator.isAndOr()) {
                if (left.isLog() && right.isLog())
                    types.store(new Type.Atom(Type.Atom.Kind.LOG), binary);
                else
                    Report.error(binary.position, String.format("Operator %s can only be used with two logicals", binary.operator.name()));
            } else if (binary.operator.isArithmetic()) {
                if (left.isInt() && right.isInt())
                    types.store(new Type.Atom(Type.Atom.Kind.INT), binary);
                else
                    Report.error(binary.position, String.format("Operator %s can only be used with two integers", binary.operator.name()));
            } else if (binary.operator.isComparison()) {
                if ((left.isInt() && right.isInt()) || (left.isLog() && right.isLog()))
                    types.store(new Type.Atom(Type.Atom.Kind.LOG), binary);
                else
                    Report.error(binary.position, String.format("Operator %s can only be used with two integers or two logicals", binary.operator.name()));
            } else if (binary.operator.compareTo(Binary.Operator.ASSIGN) == 0) {
                if ((left.isInt() && right.isInt()) || (left.isLog() && right.isLog()) || (left.isStr() && right.isStr()) ||
                        (left.isArray() && right.isInt()) || (left.isInt() && right.isArray()) || (left.isArray() && right.isArray()))
                    types.store(left, binary);
                else
                    Report.error(binary.position, String.format("Cannot assign value of type %s to variable of type %s", right, left));
            } else if (binary.operator.compareTo(Binary.Operator.ARR) == 0) {
                if ((left.isArray() && right.isInt()))
                    types.store(left.asArray().get().type, binary);
                else
                    Report.error(binary.position, String.format("Cannot use type %s for array index", right));
            }
        }
    }

    @Override
    public void visit(Block block) {
        block.expressions.forEach(expr -> {
            expr.accept(this);
            types.store(types.valueFor(expr).get(), block);
        });
    }

    @Override
    public void visit(For forLoop) {
        forLoop.counter.accept(this);
        forLoop.low.accept(this);
        Optional<Type> typeOpt = types.valueFor(forLoop.low);
        typeOpt.ifPresentOrElse(type -> {
            if (!type.isInt())
                Report.error(forLoop.low.position, "Low in for loop must be of type integer");
        }, () -> Report.error(forLoop.low.position, "Low type is not defined"));
        forLoop.high.accept(this);
        typeOpt = types.valueFor(forLoop.high);
        typeOpt.ifPresentOrElse(type -> {
            if (!type.isInt())
                Report.error(forLoop.high.position, "High in for loop must be of type integer");
        }, () -> Report.error(forLoop.high.position, "High type is not defined"));
        forLoop.step.accept(this);
        typeOpt = types.valueFor(forLoop.step);
        typeOpt.ifPresentOrElse(type -> {
            if (!type.isInt())
                Report.error(forLoop.step.position, "Step in for loop must be of type integer");
        }, () -> Report.error(forLoop.step.position, "Step type is not defined"));
        forLoop.body.accept(this);
        types.store(new Type.Atom(Type.Atom.Kind.VOID), forLoop);
    }

    @Override
    public void visit(Name name) {
        Optional<Def> optDef = definitions.valueFor(name);
        optDef.ifPresentOrElse(def -> {
            Optional<Type> optType =  types.valueFor(def);
            optType.ifPresentOrElse(type -> {
                types.store(type, name);
            }, () -> Report.error(name.position, String.format(""))); // TODO add error messages
        }, () -> Report.error(name.position, String.format("")));
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        ifThenElse.condition.accept(this);
        Optional<Type> optType = types.valueFor(ifThenElse.condition);
        optType.ifPresentOrElse(type -> {
            if (type.isLog())
                types.store(new Type.Atom(Type.Atom.Kind.VOID), ifThenElse);
            else
                Report.error(ifThenElse.position, String.format("Condition of if statement must be logical but received %s", type));
        }, () -> Report.error(ifThenElse.position, "Condition type of if statement is not defined"));

        ifThenElse.thenExpression.accept(this);
        ifThenElse.elseExpression.ifPresent(expr -> expr.accept(this));
    }

    @Override
    public void visit(Literal literal) {
        types.store(new Type.Atom(Type.Atom.Kind.valueOf(literal.type.name())), literal);
    }

    @Override
    public void visit(Unary unary) {
        unary.expr.accept(this);
        if (unary.operator.compareTo(Unary.Operator.ADD) == 0 || unary.operator.compareTo(Unary.Operator.SUB) == 0)
            types.store(new Type.Atom(Type.Atom.Kind.INT), unary);
        else if (unary.operator.compareTo(Unary.Operator.NOT) == 0)
            types.store(new Type.Atom(Type.Atom.Kind.LOG), unary);
        else Report.error(unary.position, "Invalid unary operator");
    }

    @Override
    public void visit(While whileLoop) {
        whileLoop.condition.accept(this);
        Optional<Type> optType = types.valueFor(whileLoop.condition);
        optType.ifPresentOrElse(type -> {
            if (type.isLog())
                types.store(new Type.Atom(Type.Atom.Kind.VOID), whileLoop);
            else
                Report.error(whileLoop.position, String.format("Condition of while statement must be logical but received %s", type));
        }, () -> Report.error(whileLoop.position, "Condition type of while statement is not defined"));
        whileLoop.body.accept(this);
    }

    @Override
    public void visit(Where where) {
        where.defs.definitions.forEach(def -> def.accept(this));
        where.expr.accept(this);
        Optional<Type> optType = types.valueFor(where.expr);
        optType.ifPresentOrElse(type -> {
            types.store(type, where);
        }, () -> Report.error(where.position, "Missing definitions for where statement"));
    }

    @Override
    public void visit(Defs defs) {
        defs.definitions.forEach(def -> def.accept(this));
    }

    @Override
    public void visit(FunDef funDef) {
        List<Type> paramTypes = new ArrayList<>();
        funDef.parameters.forEach(parameter -> {
            parameter.accept(this);
            Optional<Type> optType1 = types.valueFor(parameter.type);
            optType1.ifPresentOrElse(type -> {
                paramTypes.add(type);
            }, () -> Report.error(funDef.position, String.format("Function return type '%s' is not defined", funDef.name)));
        });
        funDef.type.accept(this);
        Optional<Type> optType = types.valueFor(funDef.type);
        optType.ifPresentOrElse(type -> {
            types.store(new Type.Function(paramTypes, type), funDef);
        }, () -> Report.error(funDef.position, String.format("Function return type '%s' is not defined", funDef.name)));

        funDef.body.accept(this);
    }

    @Override
    public void visit(TypeDef typeDef) {
        // TODO check for loops
        typeDef.type.accept(this);
        Optional<Type> optType = types.valueFor(typeDef.type);
        optType.ifPresentOrElse(type -> {
            types.store(type, typeDef);
        }, () -> Report.error(typeDef.position, String.format("Type definition type '%s' is not defined", typeDef.name)));
    }

    @Override
    public void visit(VarDef varDef) {
        varDef.type.accept(this);
        Optional<Type> optType = types.valueFor(varDef.type);
        optType.ifPresentOrElse(type -> {
            types.store(type, varDef);
        }, () -> Report.error(varDef.position, String.format("Variable type '%s' is not defined", varDef.name)));
    }

    @Override
    public void visit(Parameter parameter) {
        parameter.type.accept(this);
        Optional<Type> optType = types.valueFor(parameter.type);
        optType.ifPresentOrElse(type -> {
            types.store(type, parameter);
        }, () -> Report.error(parameter.position, String.format("Parameter type '%s' is not defined", parameter.name)));
    }

    @Override
    public void visit(Array array) {
        array.type.accept(this);
        Optional<Type> optType = types.valueFor(array.type);
        optType.ifPresentOrElse(type -> {
            types.store(new Type.Array(array.size, type), array);
        }, () -> Report.error(array.position, String.format("Type %s of array is not defined", array.type))
        );

    }

    @Override
    public void visit(Atom atom) {
        types.store(new Type.Atom(Type.Atom.Kind.valueOf(atom.type.name())), atom);
    }

    @Override
    public void visit(TypeName name) {
        Optional<Def> defOpt = definitions.valueFor(name);
        defOpt.ifPresentOrElse(def -> {
            def.accept(this);
            Optional<Type> optType = types.valueFor(def);
            optType.ifPresentOrElse(type -> {
                types.store(type, name);
            }, () -> Report.error(name.position, String.format("Type '%s' is not defined", name.identifier)));
        }, () -> Report.error(name.position, String.format("Type '%s' is not defined", name.identifier)));
    }
}
