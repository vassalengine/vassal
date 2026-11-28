/*
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.image.tilecache;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageTileSource;
import VASSAL.tools.io.FileStore;

/**
 * An on-disk {@link ImageTileSource} and {@link FileStore} for image tiles.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ImageTileDiskCache implements ImageTileSource, FileStore {
  private static final Logger logger =
    LoggerFactory.getLogger(ImageTileDiskCache.class);

  /** The smallest scale, 1:32768, at which a missing tile's image is probed. */
  private static final int MAX_PROBE_DIV = 1 << 15;

  /** Written the way the log writes its own timestamps, to be read against them. */
  private static final DateTimeFormatter WHEN =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss") //NON-NLS
                     .withZone(ZoneId.systemDefault());

  protected final String cpath;

  /** The images, by scale, whose missing tiles have already been described. */
  private final Set<String> described = ConcurrentHashMap.newKeySet();

  /**
   * Creates an {@code ImageTileDiskCache}.
   *
   * @param cpath path to the root directory of the cache
   */
  public ImageTileDiskCache(String cpath) {
    this.cpath = cpath;
  }

  private String tileNameFor(String name, int tileX, int tileY, double scale) {
    return cpath + '/' + TileUtils.tileName(name, tileX, tileY, (int)(1.0 / scale));
  }

  /**
   * Logs what the cache looks like around a tile which is not in it, once
   * for each image at each scale.
   *
   * <p>The Module Manager checks that every tile of every image is present
   * before it starts the Player, so a tile missing here was either there a
   * moment ago or was never where that check looked. The exception alone
   * cannot say which, nor how much else is gone. See vassalengine/vassal#15413,
   * where the check found every tile 27 seconds before one could not be
   * opened.</p>
   */
  private void describeMissingTileOnce(String name, int tileX, int tileY, double scale) {
    final int div = (int)(1.0 / scale);
    if (described.add(name + "@1:" + div)) { //NON-NLS
      logger.warn(describeMissingTile(new File(cpath), name, tileX, tileY, div));
    }
  }

  /**
   * Describes the state of a tile cache around a tile which is missing
   * from it: whether the cache is there at all, how many tiles the missing
   * one's subdirectory still holds, when those directories last changed,
   * and at which scales the image still has its first tile.
   *
   * <p>A directory's modification time moves whenever an entry is added to
   * it or removed from it, on NTFS and on the usual Unix filesystems alike,
   * so the times say when tiles last came or went.</p>
   *
   * @param root the root directory of the cache
   * @param name the name of the image
   * @param tileX the column of the missing tile
   * @param tileY the row of the missing tile
   * @param div the scale divisor of the missing tile
   * @return the description
   */
  static String describeMissingTile(File root, String name, int tileX, int tileY, int div) {
    final StringBuilder sb = new StringBuilder();
    sb.append("Tile ").append(tileX).append(',').append(tileY) //NON-NLS
      .append("@1:").append(div).append(" of ").append(name) //NON-NLS
      .append(" is missing from ").append(root).append(": "); //NON-NLS

    if (!root.isDirectory()) {
      return sb.append("the cache directory does not exist").toString(); //NON-NLS
    }

    sb.append("the cache directory last changed ").append(when(root)); //NON-NLS

    final String tname = TileUtils.tileName(name, tileX, tileY, div);
    final String sname = tname.substring(0, tname.lastIndexOf('/'));
    final File sub = new File(root, sname);
    final String[] siblings = sub.list();

    sb.append("; subdirectory ").append(sname); //NON-NLS
    if (siblings == null) {
      sb.append(" does not exist"); //NON-NLS
    }
    else {
      sb.append(" holds ").append(siblings.length) //NON-NLS
        .append(" tiles and last changed ").append(when(sub)); //NON-NLS
    }

    // We do not know how large the image is, so cannot say which scales it
    // ought to have; say which it has, and leave the gaps to be read off.
    final List<String> present = new ArrayList<>();
    for (int d = 1; d <= MAX_PROBE_DIV; d <<= 1) {
      if (new File(root, TileUtils.tileName(name, 0, 0, d)).isFile()) {
        present.add("1:" + d); //NON-NLS
      }
    }

    sb.append("; the first tile of this image is present at ") //NON-NLS
      .append(present.isEmpty() ? "no scale" : String.join(", ", present)); //NON-NLS

    return sb.toString();
  }

  private static String when(File f) {
    final long t = f.lastModified();
    return t == 0L ? "at an unknown time" : WHEN.format(Instant.ofEpochMilli(t)); //NON-NLS
  }

  /** {@inheritDoc} */
  @Override
  public BufferedImage getTile(
    String name,
    int tileX,
    int tileY,
    double scale) throws ImageIOException {

    try {
      return TileUtils.read(tileNameFor(name, tileX, tileY, scale));
    }
    catch (TileNotFoundException e) {
      describeMissingTileOnce(name, tileX, tileY, scale);
      throw new TileNotFoundException(name, tileX, tileY, scale, e);
    }
  }

  /** {@inheritDoc} */
  @Override
  public Dimension getTileSize(
    String name,
    int tileX,
    int tileY,
    double scale) throws ImageIOException {

    try {
      return TileUtils.size(tileNameFor(name, tileX, tileY, scale));
    }
    catch (TileNotFoundException e) {
      describeMissingTileOnce(name, tileX, tileY, scale);
      throw new TileNotFoundException(name, tileX, tileY, scale, e);
    }
  }

  /** {@inheritDoc} */
  @Override
  public boolean tileExists(
    String name,
    int tileX,
    int tileY,
    double scale) throws ImageIOException {

    final File f = new File(tileNameFor(name, tileX, tileY, scale));
    return f.isFile();
  }

  /** {@inheritDoc} */
  @Override
  public boolean contains(String path) throws IOException {
    return new File(cpath + "/" + path).exists();
  }

  /** {@inheritDoc} */
  @Override
  public InputStream getInputStream(String path) throws IOException {
    return Files.newInputStream(Path.of(cpath, path));
  }

  /** {@inheritDoc} */
  @Override
  public long getSize(String path) throws IOException {
    return new File(cpath + "/" + path).length();
  }

  /** {@inheritDoc} */
  @Override
  public long getMTime(String path) throws IOException {
    return new File(cpath + "/" + path).lastModified();
  }

  /** {@inheritDoc} */
  @Override
  public List<String> getFiles() throws IOException {
    final File[] files = new File(cpath).listFiles();
    final List<String> names = new ArrayList<>(files.length);

    for (final File f : files) names.add(f.getPath());

    return names;
  }

  /** {@inheritDoc} */
  @Override
  public List<String> getFiles(String root) throws IOException {
    final File[] files = new File(cpath).listFiles();
    final List<String> names = new ArrayList<>(files.length);

    for (final File f : files) {
      final String path = f.getPath();
      if (path.startsWith(root)) {
        names.add(path);
      }
    }

    return names;
  }

  /** {@inheritDoc} */
  @Override
  public boolean isClosed() {
    return false;
  }

  /** {@inheritDoc} */
  @Override
  public void close() {}
}
