/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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
package VASSAL.tools.concurrent;

import java.util.concurrent.ExecutorService;

/**
 * An {@link ExecutorService} which submits to Event Dispatch Thread.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 * @see EDTRunnableFuture
 * @deprecated Moved to {@link VASSAL.tools.swing.EDTExecutorService}.
 */
@Deprecated
public class EDTExecutorService extends VASSAL.tools.swing.EDTExecutorService {
}
