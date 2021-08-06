/*
 * Copyright (c) 2021 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.deprecation;

import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class DeprecationWalker {

  @FunctionalInterface
  public interface Callback {
    void accept(String name, String since, boolean forRemoval);
  }

  private Callback callback = (n, s, r) -> {};

  public void setCallback(Callback cb) {
    callback = cb;
  }

  private final List<String> path = new ArrayList<>();

  private ClassReader reader;

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

  private static final String DEPRECATED = Deprecated.class.getName();

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

      path.clear();
      path.add(classString(name));
    }
   
    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      return DEPRECATED.equals(descString(desc)) ? 
        new AnnotationDependencyVisitor() : null;
    }

    @Override
    public FieldVisitor visitField(
      int access,
      String name,
      String desc,
      String signature, 
      Object value) {

      path.add(name);
      return new FieldDependencyVisitor();
    }

    @Override
    public MethodVisitor visitMethod(
      int access,
      String name,
      String desc,
      String signature,
      String[] exceptions) {

      path.add(methodString(name, desc));
      return new MethodDependencyVisitor(); 
    }
  }

  private class AnnotationDependencyVisitor extends AnnotationVisitor {
    public AnnotationDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    private String since = null;
    private boolean forRemoval = false; 

    @Override
    public void visit(String name, Object value) {
      if ("since".equals(name)) {
        since = (String) value;
      }
      else if ("forRemoval".equals(name)) {
        forRemoval = (Boolean) value;
      }
    }

    @Override
    public void visitEnd() {
      callback.accept(String.join(".", path), since, forRemoval);
    }
  }

  private class FieldDependencyVisitor extends FieldVisitor {
    public FieldDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      return DEPRECATED.equals(descString(desc)) ? 
        new AnnotationDependencyVisitor() : null;
    }

    @Override
    public void visitEnd() {
      path.remove(path.size() - 1);
    }
  }

  private class MethodDependencyVisitor extends MethodVisitor {
    public MethodDependencyVisitor() {
      super(Opcodes.ASM9);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
      return DEPRECATED.equals(descString(desc)) ? 
        new AnnotationDependencyVisitor() : null;
    }

    @Override
    public void visitEnd() {
      path.remove(path.size() - 1);
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

  private String classString(String name) {
    return Type.getObjectType(name).getInternalName().replace('/', '.');
  }

  private String descString(String desc) {
    return Type.getType(desc).getInternalName().replace('/', '.');
  }

  private String methodString(String name, String desc) {
    final List<String> args = new ArrayList<>();
    for (final Type t: Type.getArgumentTypes(desc)) {
      args.add(typeString(t));
    }

    return (name + "(" + String.join(", ", args) + ")").replace('/', '.');
  } 
}
