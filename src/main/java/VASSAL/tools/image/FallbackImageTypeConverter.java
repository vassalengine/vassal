/*
 * $Id$
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

package VASSAL.tools.image;

import java.awt.image.BufferedImage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Reference;

/**
 * Convert a {@link BufferedImage} to a different type, falling back to
 * conversion on disk if convertion in memory fails.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class FallbackImageTypeConverter implements ImageTypeConverter {
  private static final Logger logger =
    LoggerFactory.getLogger(FallbackImageTypeConverter.class);

  protected final TemporaryFileFactory tfactory;
  protected final ImageTypeConverter memory_converter;
  protected final ImageTypeConverter file_converter;

  /**
   * Create a converter.
   *
   * @param tfactory the temporary file factory
   */
  public FallbackImageTypeConverter(TemporaryFileFactory tfactory) {
    this(
      tfactory,
      new MemoryImageTypeConverter(),
      new FileImageTypeConverter(tfactory)
    );
  }

  /**
   * Create a converter.
   *
   * @param tfactory the temporary file factory
   * @param memory_converter the in-memory image converter
   * @param file_converter the on-disk image converter
   */
  FallbackImageTypeConverter(
    TemporaryFileFactory tfactory,
    ImageTypeConverter memory_converter,
    ImageTypeConverter file_converter)
  {
    this.tfactory = tfactory;
    this.memory_converter = memory_converter;
    this.file_converter = file_converter;
  }

  /** {@inheritDoc} */
  @Override
  public BufferedImage convert(Reference<BufferedImage> ref, int type)
                                                      throws ImageIOException {
    try {
      return memory_converter.convert(ref, type);
    }
    catch (OutOfMemoryError e) {
      // This is ok, we just don't have enough free heap for the conversion.
      logger.info("Switching to FileImageTypeConverter...");
    }

    // Try converting on disk instead.
    return file_converter.convert(ref, type);
  }
}
