/*
 * Copyright (c) 2023 by Joel Uckelman
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

package org.vassalengine.agent;

import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.agent.builder.AgentBuilder.InitializationStrategy;
import net.bytebuddy.agent.builder.AgentBuilder.Listener;
import net.bytebuddy.agent.builder.AgentBuilder.RedefinitionStrategy;
import net.bytebuddy.agent.builder.AgentBuilder.TypeStrategy;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.dynamic.loading.ClassInjector;
import net.bytebuddy.matcher.StringMatcher;

import java.io.File;
import java.io.IOException;
import java.lang.instrument.Instrumentation;
import java.nio.file.Files;
import java.util.Collections;

import static net.bytebuddy.description.type.TypeDescription.ForLoadedType;
import static net.bytebuddy.dynamic.ClassFileLocator.ForClassLoader.read;
import static net.bytebuddy.dynamic.loading.ClassInjector.UsingInstrumentation.Target.BOOTSTRAP;
import static net.bytebuddy.matcher.ElementMatchers.*;

public class Agent {
  private static final Class<?> INTERCEPTOR_CLASS = Win32ShellFolderManager2Interceptor.class;

  // From https://stackoverflow.com/questions/44747219/byte-buddy-advice-onmethodexit-constructor-retransformation
  public static void premain(String arg, Instrumentation instrumentation) throws Exception {
    injectBootstrapClasses(instrumentation);
    new AgentBuilder.Default()
      .with(RedefinitionStrategy.RETRANSFORMATION)
      .with(InitializationStrategy.NoOp.INSTANCE)
      .with(TypeStrategy.Default.REDEFINE)
      .ignore(none())
      .with(new Listener.Filtering(
        new StringMatcher("sun.awt.shell.Win32ShellFolderManager2", StringMatcher.Mode.EQUALS_FULLY), Listener.StreamWriting.toSystemOut()))
      .type(named("sun.awt.shell.Win32ShellFolderManager2"))
      .transform((builder, type, classLoader, module, protectionDomain) ->
          builder.visit(Advice.to(INTERCEPTOR_CLASS).on(named("compareShellFolders"))))
      .installOn(instrumentation);
    }

  private static void injectBootstrapClasses(Instrumentation instrumentation) throws IOException {
    final File temp = Files.createTempDirectory("tmp").toFile();
    temp.deleteOnExit();

    ClassInjector.UsingInstrumentation.of(temp, BOOTSTRAP, instrumentation)
      .inject(Collections.singletonMap(new ForLoadedType(INTERCEPTOR_CLASS), read(INTERCEPTOR_CLASS)));
  }
}
