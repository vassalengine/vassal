/*
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

package VASSAL.tools.concurrent;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
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
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ConcurrentSoftHashMap<K,V> extends AbstractMap<K,V>
                                        implements ConcurrentMap<K,V> {

  private static final class SoftValue<K,V> extends SoftReference<V> {
    private final K key;

    private SoftValue(K key, V value, ReferenceQueue<V> queue) {
      super(value, queue);
      this.key = key;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || o.getClass() != this.getClass()) return false;
      final SoftValue<?, ?> sv = (SoftValue<?, ?>) o;
      return key.equals(sv.key) &&
             get() == null ? sv.get() == null : get().equals(sv.get());
    }

    @Override
    public int hashCode() {
      return get() == null ? 0 : get().hashCode();
    }
  }

  private final ConcurrentMap<K,SoftValue<K,V>> map =
    new ConcurrentHashMap<>();

  private final ReferenceQueue<V> queue = new ReferenceQueue<>();

  @SuppressWarnings("unchecked")
  private void processQueue() {
    SoftValue<K,V> sv;
    // The ReferenceQueue API is broken. ReferenceQueue<T>.poll()
    // returns a Reference<? extends T>. WTF? How could you ever use
    // this without having to cast back to the kind of Reference
    // you put in?
    while ((sv = (SoftValue<K,V>) queue.poll()) != null) {
      map.remove(sv.key, sv);
//      System.out.println("Hasta la vista, " + sv.key + ".");
    }
//    System.out.println("Cache size = " + map.size());
  }

  // Query Operations

  /** {@inheritDoc} */
  @Override
  public int size() {
    processQueue();
    return map.size();
  }

  /** {@inheritDoc} */
  @Override
  public boolean containsKey(Object key) {
    if (key == null)
      throw new NullPointerException();

    processQueue();
    return map.containsKey(key);
  }

  /** {@inheritDoc} */
  @Override
  public V get(Object key) {
    if (key == null)
      throw new NullPointerException();

    final SoftValue<K,V> sv = map.get(key);
    if (sv != null) {
      final V value = sv.get();
      if (value == null) {
        map.remove(key, sv);
      }
      return value;
    }
    return null;
  }

  // Modification Operations

  /** {@inheritDoc} */
  @Override
  public V put(K key, V value) {
    if (key == null)
      throw new NullPointerException();
    if (value == null)
      throw new NullPointerException();

    processQueue();
    final SoftValue<K,V> oldSV =
      map.put(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }

  /** {@inheritDoc} */
  @Override
  public V remove(Object key) {
    if (key == null)
      throw new NullPointerException();

    processQueue();
    final SoftValue<K,V> oldSV = map.remove(key);
    return oldSV == null ? null : oldSV.get();
  }

  // Bulk Operations

  /** {@inheritDoc} */
  @Override
  public void clear() {
    map.clear();
    while (queue.poll() != null);
  }

  // Views

  private Set<Map.Entry<K,V>> entrySet;

  /**
   * An implementation of {@link Map.Entry}. Remove this and use
   * {@link AbstractMap.SimpleEntry} with 1.6+.
   */
  public static class SimpleEntry<K,V> implements Entry<K,V> {
    private final K key;
    private V value;

    public SimpleEntry(K key, V value) {
      this.key   = key;
      this.value = value;
    }

    public SimpleEntry(Entry<? extends K, ? extends V> entry) {
      this.key   = entry.getKey();
      this.value = entry.getValue();
    }

    @Override
    public K getKey() {
      return key;
    }

    @Override
    public V getValue() {
      return value;
    }

    @Override
    public V setValue(V value) {
      V oldValue = this.value;
      this.value = value;
      return oldValue;
    }

    public boolean equals(Object o) {
      if (!(o instanceof Map.Entry)) return false;
      Map.Entry<?,?> e = (Map.Entry<?,?>) o;
      return eq(key, e.getKey()) && eq(value, e.getValue());
    }

    public int hashCode() {
      return (key   == null ? 0 :   key.hashCode()) ^
             (value == null ? 0 : value.hashCode());
    }

    public String toString() {
      return key + "=" + value;
    }

    private static boolean eq(Object o1, Object o2) {
      return o1 == null ? o2 == null : o1.equals(o2);
    }
  }

  /** {@inheritDoc} */
  @Override
  public Set<Map.Entry<K, V>> entrySet() {
    processQueue();

    if (entrySet == null) {
      entrySet = new AbstractSet<>() {
        @Override
        public Iterator<Map.Entry<K, V>> iterator() {
          return new Iterator<>() {
            private final Iterator<Map.Entry<K, SoftValue<K, V>>> i =
              map.entrySet().iterator();

            @Override
            public boolean hasNext() {
              return i.hasNext();
            }

            @Override
            public Map.Entry<K, V> next() {
              final Map.Entry<K, SoftValue<K, V>> e = i.next();
              return new SimpleEntry<>(e.getKey(), e.getValue().get());
            }

            @Override
            public void remove() {
              i.remove();
            }
          };
        }

        @Override
        public int size() {
          return ConcurrentSoftHashMap.this.size();
        }

        @Override
        public boolean contains(Object v) {
          return ConcurrentSoftHashMap.this.containsValue(v);
        }
      };
    }

    return entrySet;
  }

  // Concurrent Operations

  /** {@inheritDoc} */
  @Override
  public V putIfAbsent(K key, V value) {
    if (key == null)
      throw new NullPointerException();
    if (value == null)
      throw new NullPointerException();

    processQueue();
    final SoftValue<K,V> oldSV =
      map.putIfAbsent(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }

  /** {@inheritDoc} */
  @Override
  public boolean remove(Object key, Object value) {
    if (key == null)
      throw new NullPointerException();
    if (value == null)
      throw new NullPointerException();

    processQueue();
    return map.remove(key, new SoftValue<>(key, value, null));
  }

  /** {@inheritDoc} */
  @Override
  public boolean replace(K key, V oldValue, V newValue) {
    if (key == null)
      throw new NullPointerException();
    if (oldValue == null)
      throw new NullPointerException();
    if (newValue == null)
      throw new NullPointerException();

    processQueue();
    return map.replace(key,
                       new SoftValue<>(key, oldValue, null),
                       new SoftValue<>(key, newValue, queue));
  }

  /** {@inheritDoc} */
  @Override
  public V replace(K key, V value) {
    if (key == null)
      throw new NullPointerException();
    if (value == null)
      throw new NullPointerException();

    processQueue();
    final SoftValue<K,V> oldSV =
      map.replace(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }
}
