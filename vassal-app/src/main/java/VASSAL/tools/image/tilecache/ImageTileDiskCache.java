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
import java.util.ArrayList;
import java.util.List;

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

  protected final String cpath;

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
    return f.exists() && f.isFile();
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
