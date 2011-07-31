/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Brent Easton
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
package VASSAL.script;

import VASSAL.build.GameModule;
import VASSAL.build.module.properties.PropertySource;
import bsh.EvalError;
import bsh.NameSpace;

public class ScriptInterpreter extends AbstractInterpreter {

  private static final long serialVersionUID = 1L;

  public ScriptInterpreter(ClassLoader loader) {
    super();
    setClassLoader(loader);

    myNameSpace = new NameSpace(getClassManager(), "script");

    setNameSpace(myNameSpace);
    getNameSpace().importClass("VASSAL.build.module.properties.PropertySource");
    getNameSpace().importClass("VASSAL.script.ExpressionInterpreter");
    getNameSpace().importClass("VASSAL.script.ScriptInterpreter");

    setVar(THIS, this);

  }

  public Object evaluate(String statement) throws EvalError {
    setVar(SOURCE, (PropertySource) GameModule.getGameModule());
    return super.eval(statement);
  }

  public Object evaluate(String statement, PropertySource source) throws EvalError {
    setVar(SOURCE, source);
    return super.eval(statement);
  }
}