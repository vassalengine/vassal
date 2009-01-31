/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat.jabber;

import VASSAL.build.GameModule;

public class ModuleAccountInfo implements AccountInfo {
  private String login;
  private String password;

  public ModuleAccountInfo(String login, String password) {
    this.login = login;
    this.password = password;
  }

  public String getPassword() {
    return password;
  }

  public String getUserName() {
    return login;
  }

  public String getRealName() {
    return login;
  }

  public String getModule() {
    return GameModule.getGameModule().getGameName();
  }
}
