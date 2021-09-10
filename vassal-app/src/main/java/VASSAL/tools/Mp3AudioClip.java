/*
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
import java.nio.file.NoSuchFileException;

import javazoom.jl.decoder.JavaLayerException;
import javazoom.jl.player.Player;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;

public class Mp3AudioClip implements AudioClip {

  private static final Logger log = LoggerFactory.getLogger(Mp3AudioClip.class);

  protected URL url = null;
  protected String name = null;

  public Mp3AudioClip(String name) {
    this.name = name;
  }

  public Mp3AudioClip(URL url) {
    this.url = url;
  }

  protected InputStream getStream() {
    try {
      return name != null ?
        GameModule.getGameModule().getDataArchive().getInputStream(name) :
        url.openStream();
    }
    catch (FileNotFoundException | NoSuchFileException e) {
      ErrorDialog.dataWarning(new BadDataReport(
        Resources.getString(
          "Error.not_found", name != null ? name : url.toString()
        ),
        "", e
      ));
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, name != null ? name : url.toString());
    }
    return null;
  }

  @SuppressWarnings("PMD.UseTryWithResources")
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
        if (stream != null) {
          try {
            stream.close();
          }
          catch (IOException e) {
            log.error("Error while closing stream", e); //NON-NLS
          }
        }
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
    new Thread(() -> {
      try (stream) {
        player.play();
      }
      catch (JavaLayerException | IOException e) {
        ErrorDialog.dataWarning(new BadDataReport(
          "Error reading sound file", name, e //NON-NLS
        ));
      }
    }).start();
  }
}
