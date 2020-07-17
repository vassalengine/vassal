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

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.STARTING_IMAGE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILE_WRITTEN;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILING_FINISHED;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.concurrent.DaemonThreadFactory;
import VASSAL.tools.image.FallbackImageTypeConverter;
import VASSAL.tools.image.ImageIOImageLoader;
import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.image.ImageTypeConverter;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.TemporaryFileFactory;
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

  public static void main(String[] args) {
    try {
      logger.info("Starting");

      // Oh we have no heads, we have no HEADS!
      System.setProperty("java.awt.headless", "true");

      // Ensure that exceptions are logged.
      Thread.setDefaultUncaughtExceptionHandler(
        new Thread.UncaughtExceptionHandler() {
          @Override
          public void uncaughtException(Thread thread, Throwable thrown) {
            logger.error(thread.getName(), thrown);
          }
        });

      // Parse the arguments
      final String zpath = args[0];
      final String tpath = args[1];
      final int tw = Integer.parseInt(args[2]);
      final int th = Integer.parseInt(args[3]);

      // Get the image paths from stdin, one per line
      final List<String> pl = new ArrayList<>();
      try (BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in))) {
        String s;
        while ((s = stdin.readLine()) != null) {
          pl.add(s);
        }
      }
      catch (IOException e) {
        logger.error("Error while reading image paths from stdin", e);
      }

      final String[] ipaths = pl.toArray(new String[0]);

      // TODO: Determine what the optimal number of threads is.
      final Runtime runtime = Runtime.getRuntime();
      final ExecutorService exec = new ThreadPoolExecutor(
        runtime.availableProcessors(),
        runtime.availableProcessors()+1,
        60, TimeUnit.SECONDS,
        new LinkedBlockingQueue<>(),
        new DaemonThreadFactory(ZipFileImageTiler.class.getSimpleName())
      );

      final TemporaryFileFactory tfac = new TemporaryFileFactory() {
        @Override
        public File create() throws IOException {
          return File.createTempFile("img", null, new File(tpath));
        }
      };

      final ImageTypeConverter itc = new FallbackImageTypeConverter(tfac);
      final ImageLoader loader = new ImageIOImageLoader(itc);

      final TileSlicer slicer = new TileSlicerImpl();
      final FileArchiveImageTiler tiler = new FileArchiveImageTiler();

      final String portProp = System.getProperty("VASSAL.port");

      if (portProp != null) {
        writeToSocket(portProp, zpath, tiler, tpath, tw, th, ipaths, exec,
          loader, slicer);
      }
      else {
        writeToSystemErr(zpath, tiler, tpath, tw, th, ipaths, exec,
          loader, slicer);
      }
    }
    finally {
      logger.info("Exiting");
    }
  }

  private static void writeToSystemErr(String zpath, FileArchiveImageTiler tiler, String tpath, int tw, int th,
                                       String[] ipaths, ExecutorService exec, ImageLoader loader,
                                       TileSlicer slicer) {

    writeToOutputStream(System.err, zpath, tiler, tpath, tw, th, ipaths, exec,
      loader, slicer);

  }

  private static void writeToSocket(String portProp, String zpath,
                                    FileArchiveImageTiler tiler, String tpath, int tw, int th,
                                    String[] ipaths, ExecutorService exec, ImageLoader loader,
                                    TileSlicer slicer) {

    final int port = Integer.parseInt(portProp);
    final InetAddress lo;
    try {
      lo = InetAddress.getByName(null);
    }
    catch (UnknownHostException e) {
      logger.error("Could not determine local IP address", e);
      return;
    }

    try (Socket sock = new Socket(lo, port)) {
      sock.shutdownInput();

      writeToOutputStream(sock.getOutputStream(), zpath, tiler, tpath, tw, th, ipaths, exec,
        loader, slicer);
    }
    catch (IOException e) {
      logger.error("Error while setting up socket", e);
    }
  }

  private static void writeToOutputStream(OutputStream os, String zpath, FileArchiveImageTiler tiler, String tpath, int tw, int th, String[] ipaths, ExecutorService exec,
                                          ImageLoader loader, TileSlicer slicer) {

    try (final DataOutputStream out = new DataOutputStream(os)) {
      final Callback<String> imageL = new Callback<>() {
        @Override
        public void receive(String ipath) throws IOException {
          out.writeByte(STARTING_IMAGE);
          out.writeUTF(ipath);
          out.flush();
        }
      };

      final Callback<Void> tileL = new Callback<>() {
        @Override
        public void receive(Void obj) throws IOException {
          out.writeByte(TILE_WRITTEN);
          out.flush();
        }
      };

      final Callback<Void> doneL = new Callback<>() {
        @Override
        public void receive(Void obj) throws IOException {
          out.writeByte(TILING_FINISHED);
          out.flush();
        }
      };

      try (FileArchive fa = new ZipArchive(zpath)) {
        // Tile the images
        tiler.run(
          fa, tpath, tw, th, ipaths, exec,
          loader, slicer, imageL, tileL, doneL
        );
      }
      catch (IOException e) {
        logger.error("", e);
      }
    }
    catch (IOException e) {
      logger.error("Error while writing to outputstream {}", os, e);
    }
  }

}
