/*
 * $Id$
 *
 * Copyright (c) 2006-2008 by Joel Uckelman
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
package VASSAL.tools.filechooser;

/**
 * A FileFilter for AIFF, AU, and WAV files. Used by file choosers to
 * filter out files which aren't audio files.
 *
 * @author Joel Uckelman
 */
public class AudioFileFilter extends ExtensionFileFilter {
  public static final String[] types = {
    ".aiff", ".au", ".wav"
  };

  public AudioFileFilter() {
    super("Audio files", types);
  }
}
