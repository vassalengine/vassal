/*
 * $Id$
 *
 * Copyright (c) 2026 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.bug;

import VASSAL.build.GameModule;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.version.VersionUtils;

public class OldVASLBugHandler implements BugHandler {
  @Override
  public boolean accept(Throwable thrown) {
    final String modver = GameModule
      .getGameModule()
      .getAttributeValueString(GameModule.MODULE_VERSION);

    // VASL 6.7.3 has several annoying bugs which are fixed in a beta.
    // We have asked repeatedly for a bugfix release, but they insist
    // on keeping to their Spring/Autumn release schedule. We are tired
    // of seeing bug reports from VASL 6.7.3
    return VersionUtils.compareVersions("6.7.4", modver) > 0;
  }

  @Override
  public void handle(Throwable thrown) {
    ErrorDialog.showDetails(
      thrown, ThrowableUtils.getStackTrace(thrown), "OldVASLBug"
    );
  }
}

