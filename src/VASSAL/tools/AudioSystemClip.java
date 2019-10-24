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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

import VASSAL.tools.AudioClip;
import VASSAL.tools.io.IOUtils;

public class AudioSystemClip implements AudioClip {
  protected Clip clip;

  public AudioSystemClip(URL url) throws IOException {
    AudioInputStream ais = null;
    try {
      try {
        ais = AudioSystem.getAudioInputStream(url.openStream());
      }
      catch (UnsupportedAudioFileException e) {
        throw new IOException(e); 
      }

      try {
        clip = AudioSystem.getClip();
        clip.open(ais);
      }
      catch (IllegalArgumentException e) {
        throw new IOException(e);
      }
      catch (LineUnavailableException e) {
        throw new IOException(e); 
      }
    }
    finally {
      IOUtils.closeQuietly(ais);
    }
  }

  public AudioSystemClip(InputStream in) throws IOException {
    final BufferedInputStream bis = new BufferedInputStream(in);
    try {
      AudioInputStream ais = null;
      try {
        try {
          ais = AudioSystem.getAudioInputStream(bis);
        }
        catch (UnsupportedAudioFileException e) {
          throw new IOException(e);
        }

        try {
          clip = AudioSystem.getClip();
          clip.open(ais);
        }
        catch (IllegalArgumentException e) {
          throw new IOException(e);
        }
        catch (LineUnavailableException e) {
          throw new IOException(e);
        }
      }
      finally {
        IOUtils.closeQuietly(ais);
      }
    }
    finally {
      IOUtils.closeQuietly(bis);
    }
  }

  public void play() {
    clip.setFramePosition(0);
    clip.start();
  }

  public void stop() {
    clip.stop();
  }
}
