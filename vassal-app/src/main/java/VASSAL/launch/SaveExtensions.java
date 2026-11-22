/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.launch;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.IOUtils;

import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.GameState;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.io.DeobfuscatingInputStream;
import VASSAL.tools.version.VersionUtils;

/**
 * Reads which extensions a saved game or log was made with, without loading
 * its module, and compares them with the extensions a module has active.
 *
 * <p>A save records each extension that was loaded as an extension
 * registration command (name and version) in its game data; see
 * {@link VASSAL.build.module.ModuleExtension.RegCmd}. The name is the
 * extension's file name without its file extension.</p>
 */
public final class SaveExtensions {
  /** The separator GameModule puts between encoded commands. */
  private static final char COMMAND_SEPARATOR = 27; // KeyEvent.VK_ESCAPE

  private SaveExtensions() {}

  /** An extension recorded in a save, or one that a module has. */
  public static final class Extension {
    public final String name;
    public final String version;

    public Extension(String name, String version) {
      this.name = name;
      this.version = version == null ? "" : version;
    }

    @Override
    public String toString() {
      return version.isEmpty() ? name : name + " " + version;
    }
  }

  /** What is wrong with one extension the save needs. */
  public enum Problem { MISSING, INACTIVE, OLDER }

  /** An extension the save needs, and what is wrong with it. */
  public static final class Shortfall {
    public final Extension needed;
    public final Problem problem;
    /** The installed version, for {@link Problem#OLDER}; else null. */
    public final String installedVersion;

    Shortfall(Extension needed, Problem problem, String installedVersion) {
      this.needed = needed;
      this.problem = problem;
      this.installedVersion = installedVersion;
    }
  }

  /**
   * @param save a saved game or log
   * @return the extensions recorded in it, in order and without repeats
   * @throws IOException if the file cannot be read
   */
  public static List<Extension> read(File save) throws IOException {
    try (InputStream in = new BufferedInputStream(Files.newInputStream(save.toPath()));
         ZipInputStream zin = new ZipInputStream(in)) {
      for (ZipEntry e = zin.getNextEntry(); e != null; e = zin.getNextEntry()) {
        if (GameState.SAVEFILE_ZIP_ENTRY.equals(e.getName())) {
          try (InputStream din = new DeobfuscatingInputStream(zin)) {
            return parse(IOUtils.toString(din, StandardCharsets.UTF_8));
          }
        }
      }
    }
    return new ArrayList<>();
  }

  /**
   * @param commands the decoded game data of a save or log
   * @return the extensions registered in it, in order and without repeats
   */
  static List<Extension> parse(String commands) {
    final Map<String, Extension> found = new LinkedHashMap<>();
    if (commands != null && commands.contains(ExtensionsLoader.COMMAND_PREFIX)) {
      collect(commands, found);
    }
    return new ArrayList<>(found.values());
  }

  /** Splits commands the way GameModule.decode does. */
  private static void collect(String command, Map<String, Extension> found) {
    final SequenceEncoder.Decoder st =
      new SequenceEncoder.Decoder(command, COMMAND_SEPARATOR);
    final String first = st.nextToken();
    if (command.equals(first)) {
      if (first.startsWith(ExtensionsLoader.COMMAND_PREFIX)) {
        final SequenceEncoder.Decoder ext = new SequenceEncoder.Decoder(
          first.substring(ExtensionsLoader.COMMAND_PREFIX.length()), '\t'
        );
        final String name = ext.nextToken("");
        if (!name.isEmpty()) {
          found.putIfAbsent(name, new Extension(name, ext.nextToken("")));
        }
      }
      return;
    }

    if (first != null && first.contains(ExtensionsLoader.COMMAND_PREFIX)) {
      collect(first, found);
    }
    while (st.hasMoreTokens()) {
      final String next = st.nextToken();
      if (next != null && next.contains(ExtensionsLoader.COMMAND_PREFIX)) {
        collect(next, found);
      }
    }
  }

  /**
   * @param needed    the extensions recorded in the save
   * @param active    the module's active extensions
   * @param installed all of the module's extensions, active or not
   * @return each needed extension which is not active, or is active only in
   *         an older version than the save recorded
   */
  public static List<Shortfall> compare(Collection<Extension> needed,
                                        Collection<Extension> active,
                                        Collection<Extension> installed) {
    final List<Shortfall> out = new ArrayList<>();
    for (final Extension n : needed) {
      final Extension a = find(active, n.name);
      if (a == null) {
        out.add(new Shortfall(n,
          find(installed, n.name) == null ? Problem.MISSING : Problem.INACTIVE,
          null));
      }
      else if (!n.version.isEmpty() &&
               VersionUtils.compareVersions(a.version, n.version) < 0) {
        out.add(new Shortfall(n, Problem.OLDER, a.version));
      }
    }
    return out;
  }

  private static Extension find(Collection<Extension> l, String name) {
    for (final Extension e : l) {
      if (e.name.equals(name)) {
        return e;
      }
    }
    return null;
  }

  /**
   * @param f an extension file
   * @return the name a save records for it: the file name without its
   *         file extension
   */
  public static String nameOf(File f) {
    final String n = f.getName();
    final int dot = n.lastIndexOf('.');
    return dot > 0 ? n.substring(0, dot) : n;
  }
}
