/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.util.ArrayList;
import java.util.List;

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;

/**
 * Provides an XPath-like syntax for identifying configuration components
 */
public class ComponentPathBuilder {

  private static ComponentPathBuilder instance;

  public static ComponentPathBuilder getInstance() {
    if (instance == null) {
      instance = new ComponentPathBuilder();
    }
    return instance;
  }


  /**
   * Return a string identifying the specified {@link Configurable}
   * components as a paththrough the configuration parent-child hierarchy.
   *
   * @param targetPath
   * @return
   */
  public String getId(Configurable[] targetPath) {
    SequenceEncoder se = new SequenceEncoder('/');
    for (int i = 0; i < targetPath.length; ++i) {
      String name = targetPath[i].getConfigureName();
      SequenceEncoder se2 = new SequenceEncoder(targetPath[i].getClass().getName(), ':');
      if (name != null) {
        se2.append(name);
      }
      se.append(se2.getValue());
    }
    return se.getValue() == null ? "" : se.getValue();
  }

  /**
   * Return a list of {@link Configurable} components specified by the
   * given identifier.
   *
   * @param id
   * @return
   * @throws PathFormatException if no such component exists
   */
  public Configurable[] getPath(String id) throws PathFormatException {
    final ArrayList<Configurable> list = new ArrayList<Configurable>();
    if (id.length() > 0) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(id, '/');
      addToPath(GameModule.getGameModule(), st, list);
    }
    return list.toArray(new Configurable[list.size()]);
  }

  private void addToPath(Configurable parent,
                         SequenceEncoder.Decoder st,
                         List<Configurable> path)
                         throws PathFormatException {
    if (st.hasMoreTokens()) {
      String id = st.nextToken();
      String name = null;
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(id, ':');
      String className = st2.nextToken();
      if (st2.hasMoreTokens()) {
        name = st2.nextToken();
      }
      Configurable[] children = parent.getConfigureComponents();
      Configurable match = null;
      ArrayList<Configurable> partialMatches = new ArrayList<Configurable>();
      int i = -1;
      while (++i < children.length) {
        if (className.equals(children[i].getClass().getName())) {
          partialMatches.add(children[i]);
          if (name == null ? children[i].getConfigureName() == null
              : name.equals(children[i].getConfigureName())) {
            match = children[i];
            break;
          }
        }
      }
      if (match != null) {
        path.add(match);
        addToPath(match, st, path);
      }
      else if (!partialMatches.isEmpty()) {
        if (!st.hasMoreTokens()) {
           path.add(partialMatches.get(0));
        }
        else {
          ArrayList<Configurable> subPath = null;
          for (Configurable candidate : partialMatches) {
            ArrayList<Configurable> l = new ArrayList<Configurable>();
            try {
              addToPath(candidate, st.copy(), l);
              subPath = l;
// FIXME: adding to front of an ArrayList! Should we use LinkedList instead?
              subPath.add(0, candidate);
              break;
            }
            catch (PathFormatException e) {
              // No match found here.  Continue
            }
          }
          if (subPath != null) {
            path.addAll(subPath);
          }
          else {
            findFailed(className, name, parent);
          }
        }
      }
      else {
        findFailed(className, name, parent);
      }
    }
  }

  private void findFailed(String className, String name, Configurable parent)
    throws PathFormatException {

    String msgName = name;
    if (msgName == null) {
      msgName = className.substring(className.lastIndexOf('.') + 1);
    }
    throw new PathFormatException("Could not find " + msgName + " in " +
      VASSAL.configure.ConfigureTree.getConfigureName(parent.getClass()));
  }

  public static class PathFormatException extends Exception {
    private static final long serialVersionUID = 1L;

    public PathFormatException(String message) {
      super(message);
    }
  }
}
