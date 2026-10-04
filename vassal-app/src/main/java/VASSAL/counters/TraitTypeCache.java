/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.counters;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.Function;

import VASSAL.build.GameModule;

/**
 * Shares the parsed, immutable part of a trait's type between every
 * instance of that trait built from the same type string.
 *
 * <p>A trait's {@code mySetType} parses its type string into fields: key
 * strokes, property expressions, formatted strings, arrays of names. Those
 * values depend on nothing but the type string, and never change once the
 * trait is built (the editor makes a new trait from a new string rather than
 * altering an old one), yet every instance of the trait used to parse and
 * hold its own copy. A game built from prototypes holds thousands of pieces
 * that expand to the same traits, so a large game carried millions of
 * identical parsed objects. A trait that parses through this cache instead
 * builds those objects once per distinct type string and shares them,
 * keeping only its state (the values that {@code mySetState} sets) per
 * instance.</p>
 *
 * <p>What is shared must be immutable. Anything holding a reference to the
 * piece, such as a {@link KeyCommand}, or used as per-call scratch space,
 * such as a {@link VASSAL.tools.FormattedString} whose properties are set
 * before each evaluation, stays per instance. An array is not immutable, so
 * a trait never hands a shared array to one of its fields: a protected field
 * is open to a subclass, which may write into it, and such a write must
 * reach that instance alone. The trait assigns a {@code clone()} of the
 * shared array instead, which costs an array header per instance while the
 * elements stay shared. Nor are {@link VASSAL.tools.FormattedString} and
 * {@link VASSAL.configure.PropertyExpression} immutable: both have public
 * setters, so a trait keeps their source text in its shared data and builds
 * its own instance of each. That costs little, since a FormattedString is a
 * small handle on a parsed expression that its own cache already shares.</p>
 *
 * <p>Strings are not the point of this cache: {@link VASSAL.tools.SequenceEncoder.Decoder}
 * has interned every token since 2021. It is the objects built from them.</p>
 *
 * <p>The cache belongs to the {@link GameModule} ({@link GameModule#getTraitTypeCache()});
 * traits reach it through {@link #lookup}.</p>
 */
public final class TraitTypeCache {
  private final Map<Class<?>, ConcurrentMap<String, Object>> caches = new ConcurrentHashMap<>();

  /**
   * Returns the parsed data for a type string, parsing it with {@code parser}
   * the first time it is seen for {@code dataClass} and returning the same
   * object for every later request.
   *
   * @param dataClass the class of the parsed data; each class has its own
   *                  cache, so unrelated traits with equal type strings do
   *                  not collide
   * @param type the type string, as passed to {@code mySetType}
   * @param parser parses a type string; it must not return null
   * @param <T> the parsed data type
   * @return the shared parsed data
   */
  public <T> T get(Class<T> dataClass, String type, Function<String, T> parser) {
    final ConcurrentMap<String, Object> cache = caches.computeIfAbsent(dataClass, k -> new ConcurrentHashMap<>());
    return dataClass.cast(cache.computeIfAbsent(type, parser));
  }

  /** Forgets everything cached; the next request for each type parses again. */
  public void clear() {
    caches.clear();
  }

  /** @return the number of distinct type strings cached for {@code dataClass} */
  public int size(Class<?> dataClass) {
    final ConcurrentMap<String, Object> cache = caches.get(dataClass);
    return cache == null ? 0 : cache.size();
  }

  /**
   * Looks a type up in the current module's cache
   * ({@link GameModule#getTraitTypeCache()}). With no module, or a module
   * without a cache (a mocked one, in tests), the type is simply parsed, so
   * a trait built outside a game is correct but shares nothing.
   *
   * @see #get(Class, String, Function)
   */
  public static <T> T lookup(Class<T> dataClass, String type, Function<String, T> parser) {
    final GameModule g = GameModule.getGameModule();
    final TraitTypeCache cache = g == null ? null : g.getTraitTypeCache();
    return cache == null ? parser.apply(type) : cache.get(dataClass, type, parser);
  }
}
