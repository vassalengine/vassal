/*
 *
 * Copyright (c) 2020 by Brian Reynolds, VASSAL
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
package VASSAL.build.module;

import VASSAL.build.GameModule;
import ch.qos.logback.classic.PatternLayout;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

/**
 * Grabs errorlog output and displays it in the Chatter.
 */
public class ConsoleAppender extends AppenderBase<ILoggingEvent> {
  PatternLayout patternLayout;

  /**
   * Formatter for our errorlog entries
   */
  @Override
  public void start() {
    patternLayout = new PatternLayout();
    patternLayout.setContext(getContext());
    patternLayout.setPattern("%date [%contextName-%thread] %-5level %logger - %msg%n"); //NON-NLS
    patternLayout.start();

    super.start();
  }

  /**
   * This method siphons off tasty errorlog messages (via logback.xml's "CHATTER" entry) and processes them
   * @param event Event to process
   */
  @Override
  protected void append(ILoggingEvent event) {
    final GameModule module = GameModule.getGameModule();
    if (module == null) return;
    if (!module.isErrorLogToChat()) {
      return;
    }
    module.warn(patternLayout.doLayout(event));
  }
}
