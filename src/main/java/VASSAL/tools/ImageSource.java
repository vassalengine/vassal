package VASSAL.tools;

import java.awt.Image;

import VASSAL.tools.imageop.ImageOp;

/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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

/**
 * Used for lazy initialization of images in a DataArchive
 * @see DataArchive#addImageSource
 * @deprecated Use an {@link ImageOp} instead.
 */
@Deprecated
public interface ImageSource {
  /** The lazy creation of the image */
  Image getImage();
}
