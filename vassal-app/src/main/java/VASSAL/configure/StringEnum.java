/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 24, 2002
 * Time: 10:11:51 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.configure;

import VASSAL.build.AutoConfigurable;

/**
 * Wrapper class for an enumerated type.  If an AutoConfigurable object lists an attribute of type StringEnum.class,
 * the AutoConfigurer class will build a StringEnumConfigurer as that attribute's editor.
 * The AutoConfigurable object should return a sub-class of StringEnum with a no-arg constructor
 */
public abstract class StringEnum {
  public StringEnum() {
  }
  public abstract String[] getValidValues(AutoConfigurable target);
}
