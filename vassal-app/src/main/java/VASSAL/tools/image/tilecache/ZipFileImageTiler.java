/*
 *
 * Copyright (c) 2010, 2011 by Joel Uckelman
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

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.IMAGE_FINISHED;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.STARTING_IMAGE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILE_WRITTEN;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILER_READY;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILING_FINISHED;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.LineIterator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.IteratorUtils;
import VASSAL.tools.concurrent.DaemonThreadFactory;
import VASSAL.tools.image.ImageIOImageLoader;
import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.image.ImageTypeConverter;
import VASSAL.tools.image.MemoryImageTypeConverter;
import VASSAL.tools.io.ArgEncoding;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.lang.Callback;

/**
 * Tiles images contained in a ZIP archive.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ZipFileImageTiler {
  private static final Logger logger =
    LoggerFactory.getLogger(ZipFileImageTiler.class);

  private static String[] decodeArgs(String[] args) {
    return "--encoded-args".equals(args[0]) ?  new String[] {
      ArgEncoding.decode(args[1]),
      ArgEncoding.decode(args[2]),
      args[3],
      args[4]
    } : args;
  }

  public static void main(String[] args) {
    try {
      logger.info("Starting"); //NON-NLS

      // Oh we have no heads, we have no HEADS!
      System.setProperty("java.awt.headless", "true");

      // Ensure that exceptions are logged.
      Thread.setDefaultUncaughtExceptionHandler(
        (thread, thrown) -> {
          logger.error(thread.getName(), thrown);
          System.exit(thrown instanceof OutOfMemoryError ? 2 : 1);
        }
      );

      args = decodeArgs(args);

      // Parse the arguments
      final String zpath = args[0];
      final String tpath = args[1];
      final int tw = Integer.parseInt(args[2]);
      final int th = Integer.parseInt(args[3]);

      // TODO: Remove after next VASL & VSQL releases?
      final String portProp = System.getProperty("VASSAL.port");

      if (portProp != null) {
        writeToSocket(portProp, zpath, tpath, tw, th);
      }
      else {
        writeToStream(System.out, zpath, tpath, tw, th);
      }
    }
    finally {
      logger.info("Exiting"); //NON-NLS
    }
  }

  // TODO: Remove after next VASL & VSQL releases?
  private static void writeToSocket(String portProp, String zpath, String tpath, int tw, int th) {
    final InetAddress lo;
    try {
      lo = InetAddress.getByName(null);
    }
    catch (UnknownHostException e) {
      logger.error("Could not determine local IP address", e); //NON-NLS
      return;
    }

    final int port = Integer.parseInt(portProp);
    try (Socket sock = new Socket(lo, port)) {
      sock.shutdownInput();
      writeToStream(sock.getOutputStream(), zpath, tpath, tw, th);
    }
    catch (IOException e) {
      logger.error("Error while setting up socket", e); //NON-NLS
    }
  }

  private static void writeToStream(OutputStream os, String zpath, String tpath, int tw, int th) {

    // TODO: Determine what the optimal number of threads is.
    final Runtime runtime = Runtime.getRuntime();
    final ExecutorService exec = new ThreadPoolExecutor(
      runtime.availableProcessors(),
      runtime.availableProcessors() + 1,
      60, TimeUnit.SECONDS,
      new LinkedBlockingQueue<>(),
      new DaemonThreadFactory(ZipFileImageTiler.class.getSimpleName())
    );

    final ImageTypeConverter itc = new MemoryImageTypeConverter();
    final ImageLoader loader = new ImageIOImageLoader(itc);

    final TileSlicer slicer = new TileSlicerImpl();
    final FileArchiveImageTiler tiler = new FileArchiveImageTiler();

    // Get the image paths from stdin, one per line
    try (BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8))) {
      final Iterable<String> ipaths = IteratorUtils.iterate(new LineIterator(stdin));

      try (DataOutputStream out = new DataOutputStream(os)) {
        out.writeByte(TILER_READY);
        out.flush();

        final Callback<String> imageStartL = ipath -> {
          out.writeByte(STARTING_IMAGE);
          out.writeUTF(ipath);
          out.flush();
        };

        final Callback<String> imageDoneL = ipath -> {
          out.writeByte(IMAGE_FINISHED);
          out.writeUTF(ipath);
          out.flush();
        };

        final Callback<Void> tileL = obj -> {
          out.writeByte(TILE_WRITTEN);
          out.flush();
        };

        final Callback<Void> doneL = obj -> {
          out.writeByte(TILING_FINISHED);
          out.flush();
        };

        try (FileArchive fa = new ZipArchive(zpath)) {
          // Tile the images
          tiler.run(
            fa, tpath, tw, th, ipaths, exec,
            loader, slicer,
            imageStartL, imageDoneL, tileL, doneL
          );
        }
        catch (IOException e) {
          logger.error("", e);
        }
      }
    }
    catch (IOException e) {
      logger.error("Tiling I/O error", e);
    }
  }
}
