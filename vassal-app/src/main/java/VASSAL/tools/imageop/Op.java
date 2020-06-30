/*
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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

package VASSAL.tools.imageop;

import java.awt.image.BufferedImage;

import VASSAL.build.BadDataReport;
import VASSAL.counters.GamePiece;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.UnrecognizedImageTypeException;
import VASSAL.tools.opcache.OpFailedException;

public class Op {
  protected Op() {}

  public static SourceOp load(String name) {
    if (!name.startsWith("/")) {
      name = "images/" + name;
    }

    if (name.endsWith(".svg")) {
      return new SourceOpSVGImpl(name);
    }
    else {
      return new SourceOpBitmapImpl(name);
    }
  }

  public static SourceOp load(BufferedImage image) {
    return new ImageSourceOpBitmapImpl(image);
  }

  public static SourceOp loadLarge(String name) {
    if (!name.startsWith("/")) {
      name = "images/" + name;
    }

    if (name.endsWith(".svg")) {
      return new SourceOpSVGImpl(name);
    }
    else {
      return new SourceOpTiledBitmapImpl(name);
    }
  }

  public static ScaleOp scale(ImageOp sop, double scale) {
    if (sop instanceof SVGOp) {
      return new RotateScaleOpSVGImpl((SVGOp) sop, 0.0, scale);
    }
    else if (sop instanceof SourceOpTiledBitmapImpl) {
      // use the tiled version only for tiled sources
      return new ScaleOpTiledBitmapImpl(sop, scale);
    }
    else {
      return new ScaleOpBitmapImpl(sop, scale);
    }
  }

  public static RotateOp rotate(ImageOp sop, double angle) {
    if (sop instanceof SVGOp) {
      return new RotateScaleOpSVGImpl((SVGOp) sop, angle, 1.0);
    }
    else if (angle % 90.0 == 0.0) {
      return new OrthoRotateOpBitmapImpl(sop, (int) angle);
    }
    else {
      return new RotateScaleOpBitmapImpl(sop, angle, 1.0);
    }
  }

  public static RotateScaleOp rotateScale(ImageOp sop,
                                          double angle, double scale) {
    if (sop instanceof SVGOp) {
      return new RotateScaleOpSVGImpl((SVGOp) sop, angle, scale);
    }
    else {
      return new RotateScaleOpBitmapImpl(sop, angle, scale);
    }
  }

  public static CropOp crop(ImageOp sop, int x0, int y0, int x1, int y1) {
    return new CropOpBitmapImpl(sop, x0, y0, x1, y1);
  }

  public static GamePieceOp piece(GamePiece gp) {
    return new GamePieceOpImpl(gp);
  }

  public static void clearCache() {
    AbstractOpImpl.clearCache();
  }

  public static boolean handleException(Exception e) {
    for (Throwable c = e; c != null; c = c.getCause()) {
      if (c instanceof OpFailedException) {
        // We ignore OpFailedExceptions since the original exceptions
        // which caused them have already been reported.
        return true;
      }
      else if (c instanceof ImageNotFoundException) {
        ErrorDialog.dataError(new BadDataReport(
          "Image not found",
          ((ImageNotFoundException) c).getFile().getName(),
          null
        ));
        return true;
      }
      else if (c instanceof UnrecognizedImageTypeException) {
        ErrorDialog.dataError(new BadDataReport(
          "Unrecognized image type",
          ((UnrecognizedImageTypeException) c).getFile().getName(),
          c
        ));
        return true;
      }
      else if (c instanceof ImageIOException) {
        ErrorDialog.dataError(new BadDataReport(
          "Error reading image",
          ((ImageIOException) c).getFile().getName(),
          c
        ));
        return true;
      }
    }

    return false;
  }
}
