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
import java.util.Objects;
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
public class ConcurrentSoftHashMap<K, V> extends AbstractMap<K, V>
                                        implements ConcurrentMap<K, V> {

  private static final class SoftValue<K, V> extends SoftReference<V> {
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
      return key.equals(sv.key) && Objects.equals(get(), sv.get());
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(get());
    }
  }

  private final ConcurrentMap<K, SoftValue<K, V>> map =
    new ConcurrentHashMap<>();

  private final ReferenceQueue<V> queue = new ReferenceQueue<>();

  @SuppressWarnings("unchecked")
  private void processQueue() {
    SoftValue<K, V> sv;
    // The ReferenceQueue API is broken. ReferenceQueue<T>.poll()
    // returns a Reference<? extends T>. WTF? How could you ever use
    // this without having to cast back to the kind of Reference
    // you put in?
    while ((sv = (SoftValue<K, V>) queue.poll()) != null) {
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
    Objects.requireNonNull(key);

    processQueue();
    return map.containsKey(key);
  }

  /** {@inheritDoc} */
  @Override
  public V get(Object key) {
    Objects.requireNonNull(key);

    final SoftValue<K, V> sv = map.get(key);
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
    Objects.requireNonNull(key);
    Objects.requireNonNull(value);

    processQueue();
    final SoftValue<K, V> oldSV =
      map.put(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }

  /** {@inheritDoc} */
  @Override
  public V remove(Object key) {
    Objects.requireNonNull(key);

    processQueue();
    final SoftValue<K, V> oldSV = map.remove(key);
    return oldSV == null ? null : oldSV.get();
  }

  // Bulk Operations

  /** {@inheritDoc} */
  @Override
  public void clear() {
    map.clear();
    while (queue.poll() != null); // NOPMD
  }

  // Views

  private Set<Map.Entry<K, V>> entrySet;

  /**
   * An implementation of {@link Map.Entry}. Remove this and use
   * {@link AbstractMap.SimpleEntry} with 1.6+.
   */
  public static class SimpleEntry<K, V> implements Entry<K, V> {
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
      final V oldValue = this.value;
      this.value = value;
      return oldValue;
    }

    @Override
    public boolean equals(Object o) {
      if (!(o instanceof Map.Entry)) return false;
      final Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
      return Objects.equals(key, e.getKey()) &&
             Objects.equals(value, e.getValue());
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(key) ^ Objects.hashCode(value);
    }

    @Override
    public String toString() {
      return key + "=" + value;
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
    Objects.requireNonNull(key);
    Objects.requireNonNull(value);

    processQueue();
    final SoftValue<K, V> oldSV =
      map.putIfAbsent(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }

  /** {@inheritDoc} */
  @Override
  public boolean remove(Object key, Object value) {
    Objects.requireNonNull(key);
    Objects.requireNonNull(value);

    processQueue();
    return map.remove(key, new SoftValue<>(key, value, null));
  }

  /** {@inheritDoc} */
  @Override
  public boolean replace(K key, V oldValue, V newValue) {
    Objects.requireNonNull(key);
    Objects.requireNonNull(oldValue);
    Objects.requireNonNull(newValue);

    processQueue();
    return map.replace(key,
                       new SoftValue<>(key, oldValue, null),
                       new SoftValue<>(key, newValue, queue));
  }

  /** {@inheritDoc} */
  @Override
  public V replace(K key, V value) {
    Objects.requireNonNull(key);
    Objects.requireNonNull(value);

    processQueue();
    final SoftValue<K, V> oldSV =
      map.replace(key, new SoftValue<>(key, value, queue));
    return oldSV == null ? null : oldSV.get();
  }
}
