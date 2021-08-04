package VASSAL.tools.deprecation;

import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.TypePath;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;

public class DependencyWalker {
  private Consumer<String> thisClassCallback = s -> {};
  private Consumer<String> classCallback = s -> {};
  private Consumer<String> methodCallback = s -> {};
  private Consumer<String> fieldCallback = s -> {};

  private ClassReader reader;

  public void setThisClassCallback(Consumer<String> cb) {
    thisClassCallback = cb;
  }

  public void setClassCallback(Consumer<String> cb) {
    classCallback = cb;
  }

  public void setMethodCallback(Consumer<String> cb) {
    methodCallback = cb;
  }

  public void setFieldCallback(Consumer<String> cb) {
    fieldCallback = cb;
  }

  public void setInput(byte[] classFile) {
    reader = new ClassReader(classFile);
  }

  public void setInput(InputStream in) throws IOException {
    reader = new ClassReader(in);
  }

  public void setInput(String className) throws IOException {
    reader = new ClassReader(className);
  }

  public void walk() {
    reader.accept(new ClassDependencyVisitor(), 0);
  }

  private class ClassDependencyVisitor extends ClassVisitor {
    public ClassDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public void visit(
      int version, 
      int access,
      String name,
      String signature,
      String superName,
      String[] interfaces) {

      addThisClassName(name);

      if (signature == null) {
        if (superName != null) {
          addInternalName(superName);
        }
        addInternalNames(interfaces);
      }
      else {
        addSignature(signature);
      }
    }
   
    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitTypeAnnotation(
      int typeRef,
      TypePath typePath,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public FieldVisitor visitField(
      int access,
      String name,
      String desc,
      String signature, 
      Object value) {

      if (signature == null) {
        addDesc(desc);
      } 
      else {
        addTypeSignature(signature);
      }

      if (value instanceof Type) {
        addType((Type) value);
      }

      return new FieldDependencyVisitor();
    }

    @Override
    public MethodVisitor visitMethod(
      int access,
      String name,
      String desc,
      String signature,
      String[] exceptions) {

      if (signature == null) {
        addMethodDesc(desc);
      }
      else {
        addSignature(signature);
      }

      if (exceptions != null) {
        addInternalNames(exceptions);
      }

      return new MethodDependencyVisitor(); 
    }
  }

  private class AnnotationDependencyVisitor extends AnnotationVisitor {
    public AnnotationDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public void visit(String name, Object value) {
      if (value instanceof Type) {
        addType((Type) value);
      }
    }

    @Override
    public void visitEnum(String name, String desc, String value) {
      addDesc(desc);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String name, String desc) {
      addDesc(desc);
      return this;
    }

    @Override
    public AnnotationVisitor visitArray(String name) {
      return this;
    }
  }

  private class FieldDependencyVisitor extends FieldVisitor {
    public FieldDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      addDesc(desc); 
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitTypeAnnotation(
      int typeRef,
      TypePath typePath,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }
  }

  private class MethodDependencyVisitor extends MethodVisitor {
    public MethodDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public AnnotationVisitor visitAnnotationDefault() {
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitTypeAnnotation(
      int typeRef,
      TypePath typePath,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitParameterAnnotation(
      int parameter,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public AnnotationVisitor visitInsnAnnotation(
      int typeRef,
      TypePath typePath,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public void visitTypeInsn(int opcode, String type) {
      addType(Type.getObjectType(type));
    }

    @Override
    public void visitFieldInsn(
      int opcode,
      String owner,
      String name,
      String desc) {

      addInternalName(owner);
      addDesc(desc);
      addField(owner, name);
    }

    @Override
    public void visitMethodInsn(
      int opcode,
      String owner,
      String name,
      String desc,
      boolean isInterface) {

      addInternalName(owner);
      addMethodDesc(desc);
      addMethod(owner, name, desc);
    }

    @Override
    public void visitInvokeDynamicInsn(
      String name,
      String desc,
      Handle bsm,
      Object... bsmArgs) {

      addMethodDesc(desc);
      addConstant(bsm);
      addConstants(bsmArgs);
    }

    @Override
    public void visitLdcInsn(Object cst) {
      addConstant(cst);
    }

    @Override
    public void visitMultiANewArrayInsn(String desc, int dims) {
      addDesc(desc);
    }

    @Override
    public void visitLocalVariable(
      String name,
      String desc,
      String signature,
      Label start,
      Label end,
      int index) {

      if (signature == null) {
        addDesc(desc);
      }
      else {
        addSignature(signature);
      }
    }

    @Override
    public AnnotationVisitor visitLocalVariableAnnotation(
      int typeRef,
      TypePath typePath,
      Label[] start,
      Label[] end,
      int[] index,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }

    @Override
    public void visitTryCatchBlock(
      Label start,
      Label end,
      Label handler,
      String type) {

      if (type != null) {
        addInternalName(type);
      }
    }

    @Override
    public AnnotationVisitor visitTryCatchAnnotation(
      int typeRef,
      TypePath typePath,
      String desc,
      boolean visible) {

      addDesc(desc);
      return new AnnotationDependencyVisitor();
    }
  }

  private class SignatureDependencyVisitor extends SignatureVisitor {
    private String signatureClassName;

    public SignatureDependencyVisitor() {
      super(Opcodes.ASM9);
    }
 
    @Override
    public void visitClassType(String name) {
      signatureClassName = name;
      addInternalName(name);
    }

    @Override
    public void visitInnerClassType(String name) {
      signatureClassName += "$" + name;
      addInternalName(signatureClassName);
    }
  }

  private void addDesc(String desc) {
    addType(Type.getType(desc));
  }

  private void addType(Type t) {
    switch (t.getSort()) {
    case Type.ARRAY:
      addType(t.getElementType());
      break;
    case Type.OBJECT:
      addName(t.getInternalName());
      break;
    case Type.METHOD:
      addMethodDesc(t.getDescriptor());
      break;
    }
  }

  private void addMethodDesc(String desc) {
    addType(Type.getReturnType(desc));
    for (final Type t: Type.getArgumentTypes(desc)) {
      addType(t);
    }
  }

  private void addName(String name) {
    if (name != null) {
      classCallback.accept(name.replace('/', '.'));
    }
  }

  private void addThisClassName(String name) {
    addInternalName(name);
    thisClassCallback.accept(Type.getObjectType(name).getInternalName().replace('/', '.'));
  }

  private void addInternalName(String name) {
    addType(Type.getObjectType(name));
  }

  private void addInternalNames(String[] names) {
    for (final String n: names) {
      addInternalName(n);
    }
  }

  private void addConstant(Object cst) {
    if (cst instanceof Type) {
      addType((Type) cst);
    }
    else if (cst instanceof Handle) {
      final Handle h = (Handle) cst;
      addInternalName(h.getOwner());
      addMethodDesc(h.getDesc());
    }
  }

  private void addConstants(Object[] cst) {
    for (final Object c: cst) {
      addConstant(c);
    }
  }

  private String typeString(Type t) {
    switch (t.getSort()) {
    case Type.BOOLEAN:
      return "boolean";
    case Type.CHAR:
      return "char";
    case Type.BYTE:
      return "byte";
    case Type.SHORT:
      return "short";
    case Type.INT:
      return "int";
    case Type.FLOAT:
      return "float";
    case Type.LONG:
      return "long";
    case Type.DOUBLE:
      return "double";
    case Type.ARRAY:
      return typeString(t.getElementType()) + "[]".repeat(t.getDimensions()); 
    case Type.OBJECT:
      return t.getInternalName();
    default:
      throw new IllegalArgumentException();
    }
  }

  private void addMethod(String owner, String name, String desc) {
    final List<String> args = new ArrayList<>();
    for (final Type t: Type.getArgumentTypes(desc)) {
      args.add(typeString(t));
    }

    methodCallback.accept((owner + "." + name + "(" + String.join(", ", args) + ")").replace('/', '.'));
  }

  private void addField(String owner, String name) {
    fieldCallback.accept(owner.replace('/', '.') + "." + name);
  }

  private void addSignature(String signature) {
    new SignatureReader(signature).accept(new SignatureDependencyVisitor());
  }

  private void addTypeSignature(String signature) {
    new SignatureReader(signature).acceptType(new SignatureDependencyVisitor());
  }
}
