/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

import VASSAL.tools.opcache.OpObserver;

/**
 * <code>ImageOpObserver</code>s can be notified on the completion
 * of an <code>ImageOp</code>.
 *
 * <p>Objects implementing <code>ImageOpObserver</code> are intended to
 * be passed as an argument to {@link ImageOp.getImage}, which which
 * call back {@link #imageOpChange} on completion.</p>
 *
 * <p>The most common implementaion of this interface is {@link Repainter},
 * which can be used to repaint portions of {@link java.awt.Component}s
 * which are waiting for images to be computed.</p>
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see VASSAL.tools.imageop.Repainter
 */
public interface ImageOpObserver extends OpObserver<BufferedImage> {
  /**
   * Called when the <code>ImageOp</code> under observation completes.
   *
   * @param op the <code>ImageOp</code> being observed
   * @param success <code>true</code> iff the <code>op</code> succeeded
   */
  void imageOpChange(ImageOp op, boolean success);
}
