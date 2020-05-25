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
package VASSAL.tools;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javazoom.jl.decoder.JavaLayerException;
import javazoom.jl.player.Player;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.AudioClip;
import VASSAL.tools.io.IOUtils;

public class Mp3AudioClip implements AudioClip {

  protected URL url = null;
  protected String name = null;

  public Mp3AudioClip(String name) throws IOException {
    this.name = name;
  }

  public Mp3AudioClip(URL url) throws IOException {
    this.url = url;
  }

  protected InputStream getStream() {
    try {
      if (name != null) {
        try {
          return GameModule.getGameModule().getDataArchive().getInputStream(name);
        }
        catch (FileNotFoundException e) {
          ErrorDialog.dataError(new BadDataReport(
            Resources.getString("Error.not_found", name), "", e));
        }
      }
      else {
        return url.openStream();
      }
    }
    catch (IOException e) {
      ErrorDialog.bug(e);
    }
    return null;
  }

  protected Player getPlayer(InputStream stream) {
    Player player = null;
    try {
      player = new Player(stream);
    }
    catch (JavaLayerException e) {
      ErrorDialog.bug(e);
    }
    finally {
      if (player == null) {
        // close the stream if player ctor fails
        // otherwise, keep it open for the thread to close
        IOUtils.closeQuietly(stream);
      }
    }

    return player;
  }

  @Override
  public void play() {
    // load the stream
    final InputStream stream = getStream();
    if (stream == null) {
      return;
    }

    // create the player
    final Player player = getPlayer(stream);
    if (player == null) {
      return;
    }

    // run in new thread to play in background
    new Thread() {
      @Override
      public void run() {
        try (stream) {
          player.play();
        }
        catch (JavaLayerException | IOException e) {
          ErrorDialog.dataError(new BadDataReport(
            "Error reading sound file", name, e
          ));
        }
      }
    }.start();
  }
}
