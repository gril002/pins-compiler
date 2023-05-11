/**
 * @ Author: turk
 * @ Description: Analizator klicnih zapisov.
 */

package compiler.frm;

import static common.RequireNonNull.requireNonNull;

import common.Constants;
import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.TypeName;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class FrameEvaluator implements Visitor {
    /**
     * Opis definicij funkcij in njihovih klicnih zapisov.
     */
    private NodeDescription<Frame> frames;

    /**
     * Opis definicij spremenljivk in njihovih dostopov.
     */
    private NodeDescription<Access> accesses;

    /**
     * Opis vozlišč in njihovih definicij.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč in njihovih podatkovnih tipov.
     */
    private final NodeDescription<Type> types;

    int currentOffset = 0;

    Frame.Builder currentFrame;
    public int getStaticLevel() {
        if (currentFrame == null)
            return 0;
        else
            return currentFrame.staticLevel;
    }

    public FrameEvaluator(
        NodeDescription<Frame> frames, 
        NodeDescription<Access> accesses,
        NodeDescription<Def> definitions,
        NodeDescription<Type> types
    ) {
        requireNonNull(frames, accesses, definitions, types);
        this.frames = frames;
        this.accesses = accesses;
        this.definitions = definitions;
        this.types = types;
    }

    @Override
    public void visit(Call call) {
        AtomicInteger argSize = new AtomicInteger(Constants.WordSize); // prvi argument je FP kličoče funk.
        call.arguments.forEach(expr ->{
            Optional<Type> typeOptional = types.valueFor(expr);
            typeOptional.ifPresentOrElse(type -> {
                int size = type.sizeInBytesAsParam();
                //accesses.store(new Access.Local(size, currentOffset, getStaticLevel()), expr);
                argSize.addAndGet(size);
            }, () -> Report.error(expr.position, "Type of parameter is not defined"));
            //expr.accept(this);
        });

        currentFrame.addFunctionCall(argSize.get());
    }


    @Override
    public void visit(Binary binary) {
        binary.left.accept(this);
        binary.right.accept(this);
    }


    @Override
    public void visit(Block block) {
        block.expressions.forEach(expr -> expr.accept(this));
    }


    @Override
    public void visit(For forLoop) {
        forLoop.counter.accept(this);
        forLoop.step.accept(this);
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.body.accept(this);
    }


    @Override
    public void visit(Name name) {
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
        where.defs.definitions.forEach(def -> def.accept(this));
        where.expr.accept(this);
    }


    @Override
    public void visit(Defs defs) {
        defs.definitions.forEach(def -> def.accept(this));
    }


    @Override
    public void visit(FunDef funDef) {
        Frame.Label label;
        Frame.Builder parentFrame = null;
        if (getStaticLevel() == 0)
            label = Frame.Label.named(funDef.name);
        else {
            parentFrame = currentFrame;
            label = Frame.Label.nextAnonymous();
        }

        currentFrame= new Frame.Builder(label, getStaticLevel()+1);

        currentFrame.addParameter(4); // Za FP kličoče funkcije
        currentOffset = 4;
        funDef.parameters.forEach(parameter -> parameter.accept(this));
        currentOffset = 0;
        funDef.body.accept(this);
        currentOffset = 0;
        frames.store(currentFrame.build(), funDef);
        currentFrame = parentFrame;
    }


    @Override
    public void visit(TypeDef typeDef) {
        Optional<Type> typeOptional = types.valueFor(typeDef);
        typeOptional.ifPresentOrElse(type -> {
            int size = type.sizeInBytes();
            if (getStaticLevel() == 0)
                accesses.store(new Access.Global(size, Frame.Label.named(typeDef.name)), typeDef);
            else {
                currentFrame.addLocalVariable(size);
                currentOffset -= size;
                accesses.store(new Access.Local(size, currentOffset, getStaticLevel()), typeDef);
            }
        }, () -> Report.error(typeDef.position, "Type of variable is not defined"));
    }


    @Override
    public void visit(VarDef varDef) {
        Optional<Type> typeOptional = types.valueFor(varDef);
        typeOptional.ifPresentOrElse(type -> {
            int size = type.sizeInBytes();
            if (getStaticLevel() == 0)
                accesses.store(new Access.Global(size, Frame.Label.named(varDef.name)), varDef);
            else {
                currentFrame.addLocalVariable(size);
                currentOffset -= size;
                accesses.store(new Access.Local(size, currentOffset, getStaticLevel()), varDef);
            }
        }, () -> Report.error(varDef.position, "Type of variable is not defined"));
    }


    @Override
    public void visit(Parameter parameter) {
        Optional<Type> typeOptional = types.valueFor(parameter);
        typeOptional.ifPresentOrElse(type -> {
            int size = type.sizeInBytesAsParam();
            currentFrame.addParameter(size);
            accesses.store(new Access.Parameter(size, currentOffset, getStaticLevel()), parameter);
            currentOffset += size;
        }, () -> Report.error(parameter.position, "Type of parameter is not defined"));
    }


    @Override
    public void visit(Array array) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(Atom atom) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(TypeName name) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }
}
