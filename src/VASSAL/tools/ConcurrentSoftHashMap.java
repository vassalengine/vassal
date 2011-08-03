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

package VASSAL.tools;

import java.lang.ref.SoftReference;
import java.util.concurrent.ConcurrentMap;

/**
 * A memory-sensitive {@link ConcurrentMap} which stores the values
 * in {@link SoftReference}s. This <code>ConcurrentMap</code> grows
 * without bound, but when the JVM is under memory pressure, values
 * held by it may be garbage collected.
 *
 * <p>All methods except {@link #get} cause the <code>Map</code> to
 * be cleared of key-value pairs for which the value has been garbage
 * collected. Processing key-value pairs with dead values is <em>not</em>
 * an atomic operation. Thus, it is possible, though unlikely, that more
 * values will be garbage collected between the removal of dead key-value
 * pairs and the return of the method in which this takes place.</p>
 *
 * <p>This implementation does not permit <code>null</code> keys or
 * values.</p>
 *
 * @deprecated Moved to {@link VASSAL.tools.concurrent} package.
 * @since 3.1.0
 * @author Joel Uckelman
 */
@Deprecated
public class ConcurrentSoftHashMap<K,V>
  extends VASSAL.tools.concurrent.ConcurrentSoftHashMap<K,V> {
}
